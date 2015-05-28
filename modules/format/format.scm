;;; "format.scm" Common LISP text output formatter for SLIB
; Written 1992-1994 by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de)
; 2004 Aubrey Jaffer: made reentrant 
;
; This code is in the public domain.

; Authors of the original version (< 1.4) were Ken Dickey and Aubrey Jaffer.
; Please send error reports to the email address above.
; For documentation see slib.texi and format.doc.
; For testing load formatst.scm.
;
; Version 3.0

(##namespace ("format#"))
(##include "~~lib/gambit#.scm")
(##include "format#.scm")
(##namespace ("" log-error string-capitalize string-downcase string-upcase))

(define format:tab (integer->char 9))
(define format:form-feed (integer->char 12))

(define (format:call-with-output-string proc)
  (with-output-to-string "" (lambda () (proc (current-output-port)))))

;;; Configuration ------------------------------------------------------------

(define format:symbol-case-conv #f)
;; Symbols are converted by symbol->string so the case of the printed
;; symbols is implementation dependent. format:symbol-case-conv is a
;; one arg closure which is either #f (no conversion), string-upcase!,
;; string-downcase! or string-capitalize!.

(define format:iobj-case-conv #f)
;; As format:symbol-case-conv but applies for the representation of
;; implementation internal objects.

(define format:expch #\E)
;; The character prefixing the exponent value in ~e printing.

(define format:iteration-bounded #t)
;; If #t, "~{...~}" iterates no more than format:max-iterations times;
;; if #f, there is no bound.

(define format:max-iterations 100)
;; Compatible with previous versions.

(define format:floats #t)
;; Detects if the scheme system implements flonums (see at eof).

(define format:complex-numbers #t)
;; Detects if the scheme system implements complex numbers.

(define format:radix-pref (char=? #\# (string-ref (number->string 8 8) 0)))
;; Detects if number->string adds a radix prefix.

(define format:ascii-non-printable-charnames
  '#("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
     "bs"  "ht"  "nl"  "vt"  "np"  "cr"  "so"  "si"
     "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
     "can" "em"  "sub" "esc" "fs"  "gs"  "rs"  "us" "space"))

(define format:fn-max 200)		; max. number of number digits
(define format:en-max 10)	      ; max. number of exponent digits

;;; End of configuration ----------------------------------------------------

(define format:version "3.1")

(define format:space-ch (char->integer #\space))
(define format:zero-ch (char->integer #\0))

(define format:parameter-characters
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\v #\# #\'))

;; cardinals & ordinals (from dorai@cs.rice.edu)

(define format:cardinal-thousand-block-list
  '("" " thousand" " million" " billion" " trillion" " quadrillion"
    " quintillion" " sextillion" " septillion" " octillion" " nonillion"
    " decillion" " undecillion" " duodecillion" " tredecillion"
    " quattuordecillion" " quindecillion" " sexdecillion" " septendecillion"
    " octodecillion" " novemdecillion" " vigintillion"))

(define format:cardinal-ones-list
  '(#f "one" "two" "three" "four" "five"
       "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen"
       "fourteen" "fifteen" "sixteen" "seventeen" "eighteen"
       "nineteen"))

(define format:cardinal-tens-list
  '(#f #f "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty"
       "ninety"))

(define format:ordinal-ones-list
  '(#f "first" "second" "third" "fourth" "fifth"
       "sixth" "seventh" "eighth" "ninth" "tenth" "eleventh" "twelfth"
       "thirteenth" "fourteenth" "fifteenth" "sixteenth" "seventeenth"
       "eighteenth" "nineteenth"))

(define format:ordinal-tens-list
  '(#f #f "twentieth" "thirtieth" "fortieth" "fiftieth" "sixtieth"
       "seventieth" "eightieth" "ninetieth"))

;; roman numerals (from dorai@cs.rice.edu).

(define format:roman-alist
  '((1000 #\M) (500 #\D) (100 #\C) (50 #\L)
    (10 #\X) (5 #\V) (1 #\I)))

(define format:roman-boundary-values
  '(100 100 10 10 1 1 #f))

;@
(define (format . args)
  (define format:port #f)		; curr. format output port
  (define format:output-col 0)	      ; curr. format output tty column
  (define format:flush-output #f)  ; flush output at end of formatting
  (define format:case-conversion #f)
  (define format:pos 0)		; curr. format string parsing position

  ;; format string and char output routines on format:port

  (define (format:out-str str)
    (if format:case-conversion
	(display (format:case-conversion str) format:port)
	(display str format:port))
    (set! format:output-col
	  (+ format:output-col (string-length str))))

  (define (format:out-char ch)
    (if format:case-conversion
	(display (format:case-conversion (string ch)) format:port)
	(write-char ch format:port))
    (set! format:output-col
	  (if (char=? ch #\newline)
	      0
	      (+ format:output-col 1))))

  ;;(define (format:out-substr str i n)  ; this allocates a new string
  ;;  (display (substring str i n) format:port)
  ;;  (set! format:output-col (+ format:output-col n)))

  (define (format:out-substr str i n)
    (do ((k i (+ k 1)))
	((= k n))
      (write-char (string-ref str k) format:port))
    (set! format:output-col (+ format:output-col n)))

  ;;(define (format:out-fill n ch)       ; this allocates a new string
  ;;  (format:out-str (make-string n ch)))

  (define (format:out-fill n ch)
    (do ((i 0 (+ i 1)))
	((= i n))
      (write-char ch format:port))
    (set! format:output-col (+ format:output-col n)))

  (define (format:out-obj-padded pad-left obj slashify pars format:read-proof)
    (if (null? pars)
	(format:out-str (format:obj->str obj slashify format:read-proof))
	(let ((l (length pars)))
	  (let ((mincol (format:par pars l 0 0 "mincol"))
		(colinc (format:par pars l 1 1 "colinc"))
		(minpad (format:par pars l 2 0 "minpad"))
		(padchar (integer->char
			  (format:par pars l 3 format:space-ch #f)))
		(objstr (format:obj->str obj slashify format:read-proof)))
	    (if (not pad-left)
		(format:out-str objstr))
	    (do ((objstr-len (string-length objstr))
		 (i minpad (+ i colinc)))
		((>= (+ objstr-len i) mincol)
		 (format:out-fill i padchar)))
	    (if pad-left
		(format:out-str objstr))))))

  (define (format:out-num-padded modifier number pars radix)
    (if (not (integer? number))
	(log-error 'format "argument not an integer" number))
    (let ((numstr (number->string number radix)))
      (if (and format:radix-pref (not (= radix 10)))
	  (set! numstr (substring numstr 2 (string-length numstr))))
      (if (and (null? pars) (not modifier))
	  (format:out-str numstr)
	  (let ((l (length pars))
		(numstr-len (string-length numstr)))
	    (let ((mincol (format:par pars l 0 #f "mincol"))
		  (padchar (integer->char
			    (format:par pars l 1 format:space-ch #f)))
		  (commachar (integer->char
			      (format:par pars l 2 (char->integer #\,) #f)))
		  (commawidth (format:par pars l 3 3 "commawidth")))
	      (if mincol
		  (let ((numlen numstr-len)) ; calc. the output len of number
		    (if (and (memq modifier '(at colon-at)) (> number 0))
			(set! numlen (+ numlen 1)))
		    (if (memq modifier '(colon colon-at))
			(set! numlen (+ (quotient (- numstr-len
						     (if (< number 0) 2 1))
						  commawidth)
					numlen)))
		    (if (> mincol numlen)
			(format:out-fill (- mincol numlen) padchar))))
	      (if (and (memq modifier '(at colon-at))
		       (> number 0))
		  (format:out-char #\+))
	      (if (memq modifier '(colon colon-at)) ; insert comma character
		  (let ((start (remainder numstr-len commawidth))
			(ns (if (< number 0) 1 0)))
		    (format:out-substr numstr 0 start)
		    (do ((i start (+ i commawidth)))
			((>= i numstr-len))
		      (if (> i ns)
			  (format:out-char commachar))
		      (format:out-substr numstr i (+ i commawidth))))
		  (format:out-str numstr)))))))

  (define (format:tabulate modifier pars)
    (let ((l (length pars)))
      (let ((colnum (format:par pars l 0 1 "colnum"))
	    (colinc (format:par pars l 1 1 "colinc"))
	    (padch (integer->char (format:par pars l 2 format:space-ch #f))))
	(case modifier
	  ((colon colon-at)
	   (log-error 'format "unsupported modifier for ~~t" modifier))
	  ((at)				; relative tabulation
	   (format:out-fill
	    (if (= colinc 0)
		colnum			; colnum = colrel
		(do ((c 0 (+ c colinc))
		     (col (+ format:output-col colnum)))
		    ((>= c col)
		     (- c format:output-col))))
	    padch))
	  (else				; absolute tabulation
	   (format:out-fill
	    (cond
	     ((< format:output-col colnum)
	      (- colnum format:output-col))
	     ((= colinc 0)
	      0)
	     (else
	      (do ((c colnum (+ c colinc)))
		  ((>= c format:output-col)
		   (- c format:output-col)))))
	    padch))))))


  (define format:num->old-roman
    (lambda (n)
      (if (and (integer? n) (>= n 1))
	  (let loop ((n n)
		     (romans format:roman-alist)
		     (s '()))
	    (if (null? romans) (list->string (reverse s))
		(let ((roman-val (caar romans))
		      (roman-dgt (cadar romans)))
		  (do ((q (quotient n roman-val) (- q 1))
		       (s s (cons roman-dgt s)))
		      ((= q 0)
		       (loop (remainder n roman-val)
			     (cdr romans) s))))))
	  (log-error 'format "only positive integers can be romanized"))))

  (define format:num->roman
    (lambda (n)
      (if (and (integer? n) (> n 0))
	  (let loop ((n n)
		     (romans format:roman-alist)
		     (boundaries format:roman-boundary-values)
		     (s '()))
	    (if (null? romans)
		(list->string (reverse s))
		(let ((roman-val (caar romans))
		      (roman-dgt (cadar romans))
		      (bdry (car boundaries)))
		  (let loop2 ((q (quotient n roman-val))
			      (r (remainder n roman-val))
			      (s s))
		    (if (= q 0)
			(if (and bdry (>= r (- roman-val bdry)))
			    (loop (remainder r bdry) (cdr romans)
				  (cdr boundaries)
				  (cons roman-dgt
					(append
					 (cdr (assv bdry romans))
					 s)))
			    (loop r (cdr romans) (cdr boundaries) s))
			(loop2 (- q 1) r (cons roman-dgt s)))))))
	  (log-error 'format "only positive integers can be romanized"))))

  (define format:num->cardinal999
    (lambda (n)
      ;;this procedure is inspired by the Bruno Haible's CLisp
      ;;function format-small-cardinal, which converts numbers
      ;;in the range 1 to 999, and is used for converting each
      ;;thousand-block in a larger number
      (let* ((hundreds (quotient n 100))
	     (tens+ones (remainder n 100))
	     (tens (quotient tens+ones 10))
	     (ones (remainder tens+ones 10)))
	(append
	 (if (> hundreds 0)
	     (append
	      (string->list
	       (list-ref format:cardinal-ones-list hundreds))
	      (string->list" hundred")
	      (if (> tens+ones 0) '(#\space) '()))
	     '())
	 (if (< tens+ones 20)
	     (if (> tens+ones 0)
		 (string->list
		  (list-ref format:cardinal-ones-list tens+ones))
		 '())
	     (append
	      (string->list
	       (list-ref format:cardinal-tens-list tens))
	      (if (> ones 0)
		  (cons #\-
			(string->list
			 (list-ref format:cardinal-ones-list ones)))
		  '())))))))

  (define format:num->cardinal
    (lambda (n)
      (cond ((not (integer? n))
	     (log-error 'format
			 "only integers can be converted to English cardinals"))
	    ((= n 0) "zero")
	    ((< n 0) (string-append "minus " (format:num->cardinal (- n))))
	    (else
	     (let ((power3-word-limit
		    (length format:cardinal-thousand-block-list)))
	       (let loop ((n n)
			  (power3 0)
			  (s '()))
		 (if (= n 0)
		     (list->string s)
		     (let ((n-before-block (quotient n 1000))
			   (n-after-block (remainder n 1000)))
		       (loop n-before-block
			     (+ power3 1)
			     (if (> n-after-block 0)
				 (append
				  (if (> n-before-block 0)
				      (string->list ", ") '())
				  (format:num->cardinal999 n-after-block)
				  (if (< power3 power3-word-limit)
				      (string->list
				       (list-ref
					format:cardinal-thousand-block-list
					power3))
				      (append
				       (string->list " times ten to the ")
				       (string->list
					(format:num->ordinal
					 (* power3 3)))
				       (string->list " power")))
				  s)
				 s))))))))))

  (define format:num->ordinal
    (lambda (n)
      (cond ((not (integer? n))
	     (log-error 'format
			 "only integers can be converted to English ordinals"))
	    ((= n 0) "zeroth")
	    ((< n 0) (string-append "minus " (format:num->ordinal (- n))))
	    (else
	     (let ((hundreds (quotient n 100))
		   (tens+ones (remainder n 100)))
	       (string-append
		(if (> hundreds 0)
		    (string-append
		     (format:num->cardinal (* hundreds 100))
		     (if (= tens+ones 0) "th" " "))
		    "")
		(if (= tens+ones 0) ""
		    (if (< tens+ones 20)
			(list-ref format:ordinal-ones-list tens+ones)
			(let ((tens (quotient tens+ones 10))
			      (ones (remainder tens+ones 10)))
			  (if (= ones 0)
			      (list-ref format:ordinal-tens-list tens)
			      (string-append
			       (list-ref format:cardinal-tens-list tens)
			       "-"
			       (list-ref format:ordinal-ones-list ones))))
			))))))))

  ;; format fixed flonums (~F)

  (define (format:out-fixed modifier number pars)
    (if (not (or (number? number) (string? number)))
	(log-error 'format "argument is not a number or a number string"
		    number))

    (let ((l (length pars)))
      (let ((width (format:par pars l 0 #f "width"))
	    (digits (format:par pars l 1 #f "digits"))
	    (scale (format:par pars l 2 0 #f))
	    (overch (format:par pars l 3 #f #f))
	    (padch (format:par pars l 4 format:space-ch #f)))

	(if digits

	    (begin			; fixed precision
	      (format:parse-float
	       (if (string? number) number (number->string number)) #t scale)
	      (if (<= (- format:fn-len format:fn-dot) digits)
		  (format:fn-zfill #f (- digits (- format:fn-len format:fn-dot)))
		  (format:fn-round digits))
	      (if width
		  (let ((numlen (+ format:fn-len 1)))
		    (if (or (not format:fn-pos?) (eq? modifier 'at))
			(set! numlen (+ numlen 1)))
		    (if (and (= format:fn-dot 0) (> width (+ digits 1)))
			(set! numlen (+ numlen 1)))
		    (if (< numlen width)
			(format:out-fill (- width numlen) (integer->char padch)))
		    (if (and overch (> numlen width))
			(format:out-fill width (integer->char overch))
			(format:fn-out modifier (> width (+ digits 1)))))
		  (format:fn-out modifier #t)))

	    (begin			; free precision
	      (format:parse-float
	       (if (string? number) number (number->string number)) #t scale)
	      (format:fn-strip)
	      (if width
		  (let ((numlen (+ format:fn-len 1)))
		    (if (or (not format:fn-pos?) (eq? modifier 'at))
			(set! numlen (+ numlen 1)))
		    (if (= format:fn-dot 0)
			(set! numlen (+ numlen 1)))
		    (if (< numlen width)
			(format:out-fill (- width numlen) (integer->char padch)))
		    (if (> numlen width) ; adjust precision if possible
			(let ((dot-index (- numlen
					    (- format:fn-len format:fn-dot))))
			  (if (> dot-index width)
			      (if overch ; numstr too big for required width
				  (format:out-fill width (integer->char overch))
				  (format:fn-out modifier #t))
			      (begin
				(format:fn-round (- width dot-index))
				(format:fn-out modifier #t))))
			(format:fn-out modifier #t)))
		  (format:fn-out modifier #t)))))))

  ;; format exponential flonums (~E)

  (define (format:out-expon modifier number pars)
    (if (not (or (number? number) (string? number)))
	(log-error 'format "argument is not a number" number))

    (let ((l (length pars)))
      (let ((width (format:par pars l 0 #f "width"))
	    (digits (format:par pars l 1 #f "digits"))
	    (edigits (format:par pars l 2 #f "exponent digits"))
	    (scale (format:par pars l 3 1 #f))
	    (overch (format:par pars l 4 #f #f))
	    (padch (format:par pars l 5 format:space-ch #f))
	    (expch (format:par pars l 6 #f #f)))

	(if digits			; fixed precision

	    (let ((digits (if (> scale 0)
			      (if (< scale (+ digits 2))
				  (+ (- digits scale) 1)
				  0)
			      digits)))
	      (format:parse-float
	       (if (string? number) number (number->string number)) #f scale)
	      (if (<= (- format:fn-len format:fn-dot) digits)
		  (format:fn-zfill #f (- digits (- format:fn-len format:fn-dot)))
		  (format:fn-round digits))
	      (if width
		  (if (and edigits overch (> format:en-len edigits))
		      (format:out-fill width (integer->char overch))
		      (let ((numlen (+ format:fn-len 3))) ; .E+
			(if (or (not format:fn-pos?) (eq? modifier 'at))
			    (set! numlen (+ numlen 1)))
			(if (and (= format:fn-dot 0) (> width (+ digits 1)))
			    (set! numlen (+ numlen 1)))
			(set! numlen
			      (+ numlen
				 (if (and edigits (>= edigits format:en-len))
				     edigits
				     format:en-len)))
			(if (< numlen width)
			    (format:out-fill (- width numlen)
					     (integer->char padch)))
			(if (and overch (> numlen width))
			    (format:out-fill width (integer->char overch))
			    (begin
			      (format:fn-out modifier (> width (- numlen 1)))
			      (format:en-out edigits expch)))))
		  (begin
		    (format:fn-out modifier #t)
		    (format:en-out edigits expch))))

	    (begin			; free precision
	      (format:parse-float
	       (if (string? number) number (number->string number)) #f scale)
	      (format:fn-strip)
	      (if width
		  (if (and edigits overch (> format:en-len edigits))
		      (format:out-fill width (integer->char overch))
		      (let ((numlen (+ format:fn-len 3))) ; .E+
			(if (or (not format:fn-pos?) (eq? modifier 'at))
			    (set! numlen (+ numlen 1)))
			(if (= format:fn-dot 0)
			    (set! numlen (+ numlen 1)))
			(set! numlen
			      (+ numlen
				 (if (and edigits (>= edigits format:en-len))
				     edigits
				     format:en-len)))
			(if (< numlen width)
			    (format:out-fill (- width numlen)
					     (integer->char padch)))
			(if (> numlen width) ; adjust precision if possible
			    (let ((f (- format:fn-len format:fn-dot))) ; fract len
			      (if (> (- numlen f) width)
				  (if overch ; numstr too big for required width
				      (format:out-fill width
						       (integer->char overch))
				      (begin
					(format:fn-out modifier #t)
					(format:en-out edigits expch)))
				  (begin
				    (format:fn-round (+ (- f numlen) width))
				    (format:fn-out modifier #t)
				    (format:en-out edigits expch))))
			    (begin
			      (format:fn-out modifier #t)
			      (format:en-out edigits expch)))))
		  (begin
		    (format:fn-out modifier #t)
		    (format:en-out edigits expch))))))))

  ;; format general flonums (~G)

  (define (format:out-general modifier number pars)
    (if (not (or (number? number) (string? number)))
	(log-error 'format "argument is not a number or a number string"
		    number))

    (let ((l (length pars)))
      (let ((width (if (> l 0) (list-ref pars 0) #f))
	    (digits (if (> l 1) (list-ref pars 1) #f))
	    (edigits (if (> l 2) (list-ref pars 2) #f))
	    (overch (if (> l 4) (list-ref pars 4) #f))
	    (padch (if (> l 5) (list-ref pars 5) #f)))
	(format:parse-float
	 (if (string? number) number (number->string number)) #t 0)
	(format:fn-strip)
	(let* ((ee (if edigits (+ edigits 2) 4)) ; for the following algorithm
	       (ww (if width (- width ee) #f)) ; see Steele's CL book p.395
	       (n (if (= format:fn-dot 0) ; number less than (abs 1.0) ?
		      (- (format:fn-zlead))
		      format:fn-dot))
	       (d (if digits
		      digits
		      (max format:fn-len (min n 7)))) ; q = format:fn-len
	       (dd (- d n)))
	  (if (<= 0 dd d)
	      (begin
		(format:out-fixed modifier number (list ww dd #f overch padch))
		(format:out-fill ee #\space)) ;~@T not implemented yet
	      (format:out-expon modifier number pars))))))

  ;; format dollar flonums (~$)

  (define (format:out-dollar modifier number pars)
    (if (not (or (number? number) (string? number)))
	(log-error 'format "argument is not a number or a number string"
		    number))

    (let ((l (length pars)))
      (let ((digits (format:par pars l 0 2 "digits"))
	    (mindig (format:par pars l 1 1 "mindig"))
	    (width (format:par pars l 2 0 "width"))
	    (padch (format:par pars l 3 format:space-ch #f)))

	(format:parse-float
	 (if (string? number) number (number->string number)) #t 0)
	(if (<= (- format:fn-len format:fn-dot) digits)
	    (format:fn-zfill #f (- digits (- format:fn-len format:fn-dot)))
	    (format:fn-round digits))
	(let ((numlen (+ format:fn-len 1)))
	  (if (or (not format:fn-pos?) (memq modifier '(at colon-at)))
	      (set! numlen (+ numlen 1)))
	  (if (and mindig (> mindig format:fn-dot))
	      (set! numlen (+ numlen (- mindig format:fn-dot))))
	  (if (and (= format:fn-dot 0) (not mindig))
	      (set! numlen (+ numlen 1)))
	  (if (< numlen width)
	      (case modifier
		((colon)
		 (if (not format:fn-pos?)
		     (format:out-char #\-))
		 (format:out-fill (- width numlen) (integer->char padch)))
		((at)
		 (format:out-fill (- width numlen) (integer->char padch))
		 (format:out-char (if format:fn-pos? #\+ #\-)))
		((colon-at)
		 (format:out-char (if format:fn-pos? #\+ #\-))
		 (format:out-fill (- width numlen) (integer->char padch)))
		(else
		 (format:out-fill (- width numlen) (integer->char padch))
		 (if (not format:fn-pos?)
		     (format:out-char #\-))))
	      (if format:fn-pos?
		  (if (memq modifier '(at colon-at)) (format:out-char #\+))
		  (format:out-char #\-))))
	(if (and mindig (> mindig format:fn-dot))
	    (format:out-fill (- mindig format:fn-dot) #\0))
	(if (and (= format:fn-dot 0) (not mindig))
	    (format:out-char #\0))
	(format:out-substr format:fn-str 0 format:fn-dot)
	(format:out-char #\.)
	(format:out-substr format:fn-str format:fn-dot format:fn-len))))

					; the flonum buffers

  (define format:fn-str (make-string format:fn-max)) ; number buffer
  (define format:fn-len 0)		; digit length of number
  (define format:fn-dot #f)		; dot position of number
  (define format:fn-pos? #t)		; number positive?
  (define format:en-str (make-string format:en-max)) ; exponent buffer
  (define format:en-len 0)		; digit length of exponent
  (define format:en-pos? #t)		; exponent positive?

  (define (format:parse-float num-str fixed? scale)
    (set! format:fn-pos? #t)
    (set! format:fn-len 0)
    (set! format:fn-dot #f)
    (set! format:en-pos? #t)
    (set! format:en-len 0)
    (do ((i 0 (+ i 1))
	 (left-zeros 0)
	 (mantissa? #t)
	 (all-zeros? #t)
	 (num-len (string-length num-str))
	 (c #f))		  ; current exam. character in num-str
	((= i num-len)
	 (if (not format:fn-dot)
	     (set! format:fn-dot format:fn-len))

	 (if all-zeros?
	     (begin
	       (set! left-zeros 0)
	       (set! format:fn-dot 0)
	       (set! format:fn-len 1)))

	 ;; now format the parsed values according to format's need

	 (if fixed?

	     (begin			; fixed format m.nnn or .nnn
	       (if (and (> left-zeros 0) (> format:fn-dot 0))
		   (if (> format:fn-dot left-zeros)
		       (begin		; norm 0{0}nn.mm to nn.mm
			 (format:fn-shiftleft left-zeros)
			 (set! left-zeros 0)
			 (set! format:fn-dot (- format:fn-dot left-zeros)))
		       (begin		; normalize 0{0}.nnn to .nnn
			 (format:fn-shiftleft format:fn-dot)
			 (set! left-zeros (- left-zeros format:fn-dot))
			 (set! format:fn-dot 0))))
	       (if (or (not (= scale 0)) (> format:en-len 0))
		   (let ((shift (+ scale (format:en-int))))
		     (cond
		      (all-zeros? #t)
		      ((> (+ format:fn-dot shift) format:fn-len)
		       (format:fn-zfill
			#f (- shift (- format:fn-len format:fn-dot)))
		       (set! format:fn-dot format:fn-len))
		      ((< (+ format:fn-dot shift) 0)
		       (format:fn-zfill #t (- (- shift) format:fn-dot))
		       (set! format:fn-dot 0))
		      (else
		       (if (> left-zeros 0)
			   (if (<= left-zeros shift) ; shift always > 0 here
			       (format:fn-shiftleft shift) ; shift out 0s
			       (begin
				 (format:fn-shiftleft left-zeros)
				 (set! format:fn-dot (- shift left-zeros))))
			   (set! format:fn-dot (+ format:fn-dot shift))))))))

	     (let ((negexp		; expon format m.nnnEee
		    (if (> left-zeros 0)
			(- left-zeros format:fn-dot -1)
			(if (= format:fn-dot 0) 1 0))))
	       (if (> left-zeros 0)
		   (begin		; normalize 0{0}.nnn to n.nn
		     (format:fn-shiftleft left-zeros)
		     (set! format:fn-dot 1))
		   (if (= format:fn-dot 0)
		       (set! format:fn-dot 1)))
	       (format:en-set (- (+ (- format:fn-dot scale) (format:en-int))
				 negexp))
	       (cond
		(all-zeros?
		 (format:en-set 0)
		 (set! format:fn-dot 1))
		((< scale 0)		; leading zero
		 (format:fn-zfill #t (- scale))
		 (set! format:fn-dot 0))
		((> scale format:fn-dot)
		 (format:fn-zfill #f (- scale format:fn-dot))
		 (set! format:fn-dot scale))
		(else
		 (set! format:fn-dot scale)))))
	 #t)

      ;; do body
      (set! c (string-ref num-str i)) ; parse the output of number->string
      (cond			       ; which can be any valid number
       ((char-numeric? c)	       ; representation of R4RS except
	(if mantissa?			; complex numbers
	    (begin
	      (if (char=? c #\0)
		  (if all-zeros?
		      (set! left-zeros (+ left-zeros 1)))
		  (begin
		    (set! all-zeros? #f)))
	      (string-set! format:fn-str format:fn-len c)
	      (set! format:fn-len (+ format:fn-len 1)))
	    (begin
	      (string-set! format:en-str format:en-len c)
	      (set! format:en-len (+ format:en-len 1)))))
       ((or (char=? c #\-) (char=? c #\+))
	(if mantissa?
	    (set! format:fn-pos? (char=? c #\+))
	    (set! format:en-pos? (char=? c #\+))))
       ((char=? c #\.)
	(set! format:fn-dot format:fn-len))
       ((char=? c #\e)
	(set! mantissa? #f))
       ((char=? c #\E)
	(set! mantissa? #f))
       ((char-whitespace? c) #t)
       ((char=? c #\d) #t)		; decimal radix prefix
       ((char=? c #\#) #t)
       (else
	(log-error 'format "illegal character in number->string" c)))))

  (define (format:en-int)	  ; convert exponent string to integer
    (if (= format:en-len 0)
	0
	(do ((i 0 (+ i 1))
	     (n 0))
	    ((= i format:en-len)
	     (if format:en-pos?
		 n
		 (- n)))
	  (set! n (+ (* n 10) (- (char->integer (string-ref format:en-str i))
				 format:zero-ch))))))

  (define (format:en-set en)		; set exponent string number
    (set! format:en-len 0)
    (set! format:en-pos? (>= en 0))
    (let ((en-str (number->string en)))
      (do ((i 0 (+ i 1))
	   (en-len (string-length en-str))
	   (c #f))
	  ((= i en-len))
	(set! c (string-ref en-str i))
	(if (char-numeric? c)
	    (begin
	      (string-set! format:en-str format:en-len c)
	      (set! format:en-len (+ format:en-len 1)))))))

  (define (format:fn-zfill left? n) ; fill current number string with 0s
    (if (> (+ n format:fn-len) format:fn-max) ; from the left or right
	(log-error 'format "number is too long to format (enlarge format:fn-max)"))
    (set! format:fn-len (+ format:fn-len n))
    (if left?
	(do ((i format:fn-len (- i 1)))	; fill n 0s to left
	    ((< i 0))
	  (string-set! format:fn-str i
		       (if (< i n)
			   #\0
			   (string-ref format:fn-str (- i n)))))
	(do ((i (- format:fn-len n) (+ i 1))) ; fill n 0s to the right
	    ((= i format:fn-len))
	  (string-set! format:fn-str i #\0))))

  (define (format:fn-shiftleft n) ; shift left current number n positions
    (if (> n format:fn-len)
	(log-error 'format "internal error in format:fn-shiftleft"
		    (list n format:fn-len)))
    (do ((i n (+ i 1)))
	((= i format:fn-len)
	 (set! format:fn-len (- format:fn-len n)))
      (string-set! format:fn-str (- i n) (string-ref format:fn-str i))))

  (define (format:fn-round digits)	; round format:fn-str
    (set! digits (+ digits format:fn-dot))
    (do ((i digits (- i 1))		; "099",2 -> "10"
	 (c 5))				; "023",2 -> "02"
	((or (= c 0) (< i 0))		; "999",2 -> "100"
	 (if (= c 1)			; "005",2 -> "01"
	     (begin			; carry overflow
	       (set! format:fn-len digits)
	       (format:fn-zfill #t 1)	; add a 1 before fn-str
	       (string-set! format:fn-str 0 #\1)
	       (set! format:fn-dot (+ format:fn-dot 1)))
	     (set! format:fn-len digits)))
      (set! c (+ (- (char->integer (string-ref format:fn-str i))
		    format:zero-ch) c))
      (string-set! format:fn-str i (integer->char
				    (if (< c 10)
					(+ c format:zero-ch)
					(+ (- c 10) format:zero-ch))))
      (set! c (if (< c 10) 0 1))))

  (define (format:fn-out modifier add-leading-zero?)
    (if format:fn-pos?
	(if (eq? modifier 'at)
	    (format:out-char #\+))
	(format:out-char #\-))
    (if (= format:fn-dot 0)
	(if add-leading-zero?
	    (format:out-char #\0))
	(format:out-substr format:fn-str 0 format:fn-dot))
    (format:out-char #\.)
    (format:out-substr format:fn-str format:fn-dot format:fn-len))

  (define (format:en-out edigits expch)
    (format:out-char (if expch (integer->char expch) format:expch))
    (format:out-char (if format:en-pos? #\+ #\-))
    (if edigits
	(if (< format:en-len edigits)
	    (format:out-fill (- edigits format:en-len) #\0)))
    (format:out-substr format:en-str 0 format:en-len))

  (define (format:fn-strip)		; strip trailing zeros but one
    (string-set! format:fn-str format:fn-len #\0)
    (do ((i format:fn-len (- i 1)))
	((or (not (char=? (string-ref format:fn-str i) #\0))
	     (<= i format:fn-dot))
	 (set! format:fn-len (+ i 1)))))

  (define (format:fn-zlead)		; count leading zeros
    (do ((i 0 (+ i 1)))
	((or (= i format:fn-len)
	     (not (char=? (string-ref format:fn-str i) #\0)))
	 (if (= i format:fn-len)	; found a real zero
	     0
	     i))))

  (define (format:format-work format-string arglist) ; does the formatting work
    (letrec
	((format-string-len (string-length format-string))
	 (arg-pos 0)			; argument position in arglist
	 (arg-len (length arglist))	; number of arguments
	 (modifier #f)		       ; 'colon | 'at | 'colon-at | #f
	 (params '())			; directive parameter list
	 (param-value-found #f)	   ; a directive parameter value found
	 (conditional-nest 0)		; conditional nesting level
	 (clause-pos 0)		; last cond. clause beginning char pos
	 (clause-default #f)	   ; conditional default clause string
	 (clauses '())		      ; conditional clause string list
	 (conditional-type #f)	    ; reflects the contional modifiers
	 (conditional-arg #f)	   ; argument to apply the conditional
	 (iteration-nest 0)		; iteration nesting level
	 (iteration-pos 0)	 ; iteration string beginning char pos
	 (iteration-type #f)	    ; reflects the iteration modifiers
	 (max-iterations #f)		; maximum number of iterations
	 (recursive-pos-save format:pos)

	 (next-char	       ; gets the next char from format-string
	  (lambda ()
	    (let ((ch (peek-next-char)))
	      (set! format:pos (+ 1 format:pos))
	      ch)))

	 (peek-next-char
	  (lambda ()
	    (if (>= format:pos format-string-len)
		(log-error 'format "illegal format string")
		(string-ref format-string format:pos))))

	 (one-positive-integer?
	  (lambda (params)
	    (cond
	     ((null? params) #f)
	     ((and (integer? (car params))
		   (>= (car params) 0)
		   (= (length params) 1)) #t)
	     (else (log-error 'format "one positive integer parameter expected")))))

	 (next-arg
	  (lambda ()
	    (if (>= arg-pos arg-len)
		(begin
		  (log-error 'format "missing argument(s)")))
	    (add-arg-pos 1)
	    (list-ref arglist (- arg-pos 1))))

	 (prev-arg
	  (lambda ()
	    (add-arg-pos -1)
	    (if (negative? arg-pos)
		(log-error 'format "missing backward argument(s)"))
	    (list-ref arglist arg-pos)))

	 (rest-args
	  (lambda ()
	    (let loop ((l arglist) (k arg-pos)) ; list-tail definition
	      (if (= k 0) l (loop (cdr l) (- k 1))))))

	 (add-arg-pos
	  (lambda (n)
	    (set! arg-pos (+ n arg-pos))))

	 (anychar-dispatch		; dispatches the format-string
	  (lambda ()
	    (if (>= format:pos format-string-len)
		arg-pos			; used for ~? continuance
		(let ((char (next-char)))
		  (cond
		   ((char=? char #\~)
		    (set! modifier #f)
		    (set! params '())
		    (set! param-value-found #f)
		    (tilde-dispatch))
		   (else
		    (if (and (zero? conditional-nest)
			     (zero? iteration-nest))
			(format:out-char char))
		    (anychar-dispatch)))))))

	 (tilde-dispatch
	  (lambda ()
	    (cond
	     ((>= format:pos format-string-len)
	      (format:out-str "~") ; tilde at end of string is just output
	      arg-pos)			; used for ~? continuance
	     ((and (or (zero? conditional-nest)
		       (memv (peek-next-char) ; find conditional directives
			     (append '(#\[ #\] #\; #\: #\@ #\^)
				     format:parameter-characters)))
		   (or (zero? iteration-nest)
		       (memv (peek-next-char) ; find iteration directives
			     (append '(#\{ #\} #\: #\@ #\^)
				     format:parameter-characters))))
	      (case (char-upcase (next-char))

		;; format directives

		((#\A)			; Any -- for humans
		 (format:out-obj-padded (memq modifier '(at colon-at))
					(next-arg) #f params
					(memq modifier '(colon colon-at)))
		 (anychar-dispatch))
		((#\S)			; Slashified -- for parsers
		 (format:out-obj-padded (memq modifier '(at colon-at))
					(next-arg) #t params
					(memq modifier '(colon colon-at)))
		 (anychar-dispatch))
		((#\D)			; Decimal
		 (format:out-num-padded modifier (next-arg) params 10)
		 (anychar-dispatch))
		((#\X)			; Hexadecimal
		 (format:out-num-padded modifier (next-arg) params 16)
		 (anychar-dispatch))
		((#\O)			; Octal
		 (format:out-num-padded modifier (next-arg) params 8)
		 (anychar-dispatch))
		((#\B)			; Binary
		 (format:out-num-padded modifier (next-arg) params 2)
		 (anychar-dispatch))
		((#\R)
		 (if (null? params)
		     (format:out-obj-padded ; Roman, cardinal, ordinal numerals
		      #f
		      ((case modifier
			 ((at) format:num->roman)
			 ((colon-at) format:num->old-roman)
			 ((colon) format:num->ordinal)
			 (else format:num->cardinal))
		       (next-arg))
		      #f params #f)	;was format:read-proof
		     (format:out-num-padded ; any Radix
		      modifier (next-arg) (cdr params) (car params)))
		 (anychar-dispatch))
		((#\F)			; Fixed-format floating-point
		 (if format:floats
		     (format:out-fixed modifier (next-arg) params)
		     (format:out-str (number->string (next-arg))))
		 (anychar-dispatch))
		((#\E)			; Exponential floating-point
		 (if format:floats
		     (format:out-expon modifier (next-arg) params)
		     (format:out-str (number->string (next-arg))))
		 (anychar-dispatch))
		((#\G)			; General floating-point
		 (if format:floats
		     (format:out-general modifier (next-arg) params)
		     (format:out-str (number->string (next-arg))))
		 (anychar-dispatch))
		((#\$)			; Dollars floating-point
		 (if format:floats
		     (format:out-dollar modifier (next-arg) params)
		     (format:out-str (number->string (next-arg))))
		 (anychar-dispatch))
		((#\I)			; Complex numbers
		 (if (not format:complex-numbers)
		     (log-error 'format
				 "complex numbers not supported by this scheme system"))
		 (let ((z (next-arg)))
		   (if (not (complex? z))
		       (log-error 'format "argument not a complex number"))
		   (format:out-fixed modifier (real-part z) params)
		   (format:out-fixed 'at (imag-part z) params)
		   (format:out-char #\i))
		 (anychar-dispatch))
		((#\C)			; Character
		 (let ((ch (if (one-positive-integer? params)
			       (integer->char (car params))
			       (next-arg))))
		   (if (not (char? ch)) (log-error 'format "~~c expects a character" ch))
		   (case modifier
		     ((at)
		      (format:out-str (format:char->str ch)))
		     ((colon)
		      (let ((c (char->integer ch)))
			(if (< c 0)
			    (set! c (+ c 256)))	; compensate complement impl.
			(cond
			 ((< c #x20) ; assumes that control chars are < #x20
			  (format:out-char #\^)
			  (format:out-char
			   (integer->char (+ c #x40))))
			 ((>= c #x7f)
			  (format:out-str "#\\")
			  (format:out-str
			   (if format:radix-pref
			       (let ((s (number->string c 8)))
				 (substring s 2 (string-length s)))
			       (number->string c 8))))
			 (else
			  (format:out-char ch)))))
		     (else (format:out-char ch))))
		 (anychar-dispatch))
		((#\P)			; Plural
		 (if (memq modifier '(colon colon-at))
		     (prev-arg))
		 (let ((arg (next-arg)))
		   (if (not (number? arg))
		       (log-error 'format "~~p expects a number argument" arg))
		   (if (= arg 1)
		       (if (memq modifier '(at colon-at))
			   (format:out-char #\y))
		       (if (memq modifier '(at colon-at))
			   (format:out-str "ies")
			   (format:out-char #\s))))
		 (anychar-dispatch))
		((#\~)			; Tilde
		 (if (one-positive-integer? params)
		     (format:out-fill (car params) #\~)
		     (format:out-char #\~))
		 (anychar-dispatch))
		((#\%)			; Newline
		 (if (one-positive-integer? params)
		     (format:out-fill (car params) #\newline)
		     (format:out-char #\newline))
		 (set! format:output-col 0)
		 (anychar-dispatch))
		((#\&)			; Fresh line
		 (if (one-positive-integer? params)
		     (begin
		       (if (> (car params) 0)
			   (format:out-fill (- (car params)
					       (if (> format:output-col 0) 0 1))
					    #\newline))
		       (set! format:output-col 0))
		     (if (> format:output-col 0)
			 (format:out-char #\newline)))
		 (anychar-dispatch))
		((#\_)			; Space character
		 (if (one-positive-integer? params)
		     (format:out-fill (car params) #\space)
		     (format:out-char #\space))
		 (anychar-dispatch))
		((#\/)			; Tabulator character
		 (if (one-positive-integer? params)
		     (format:out-fill (car params) format:tab)
		     (format:out-char format:tab))
		 (anychar-dispatch))
		((#\|)			; Page seperator
		 (if (one-positive-integer? params)
		     (format:out-fill (car params) format:form-feed)
		     (format:out-char format:form-feed))
		 (set! format:output-col 0)
		 (anychar-dispatch))
		((#\T)			; Tabulate
		 (format:tabulate modifier params)
		 (anychar-dispatch))
		((#\Y)			; Pretty-print
		 ;;(require 'pretty-print)
		 (pretty-print (next-arg) format:port)
		 (set! format:output-col 0)
		 (anychar-dispatch))
		((#\? #\K)	   ; Indirection (is "~K" in T-Scheme)
		 (cond
		  ((memq modifier '(colon colon-at))
		   (log-error 'format "illegal modifier in ~~?" modifier))
		  ((eq? modifier 'at)
		   (let* ((frmt (next-arg))
			  (args (rest-args)))
		     (add-arg-pos (format:format-work frmt args))))
		  (else
		   (let* ((frmt (next-arg))
			  (args (next-arg)))
		     (format:format-work frmt args))))
		 (anychar-dispatch))
		((#\!)			; Flush output
		 (set! format:flush-output #t)
		 (anychar-dispatch))
		((#\newline)		; Continuation lines
		 (if (eq? modifier 'at)
		     (format:out-char #\newline))
		 (if (< format:pos format-string-len)
		     (do ((ch (peek-next-char) (peek-next-char)))
			 ((or (not (char-whitespace? ch))
			      (= format:pos (- format-string-len 1))))
		       (if (eq? modifier 'colon)
			   (format:out-char (next-char))
			   (next-char))))
		 (anychar-dispatch))
		((#\*)			; Argument jumping
		 (case modifier
		   ((colon)		; jump backwards
		    (if (one-positive-integer? params)
			(do ((i 0 (+ i 1)))
			    ((= i (car params)))
			  (prev-arg))
			(prev-arg)))
		   ((at)		; jump absolute
		    (set! arg-pos (if (one-positive-integer? params)
				      (car params) 0)))
		   ((colon-at)
		    (log-error 'format "illegal modifier `:@' in ~~* directive"))
		   (else		; jump forward
		    (if (one-positive-integer? params)
			(do ((i 0 (+ i 1)))
			    ((= i (car params)))
			  (next-arg))
			(next-arg))))
		 (anychar-dispatch))
		((#\()			; Case conversion begin
		 (set! format:case-conversion
		       (case modifier
			 ((at) format:string-capitalize-first)
			 ((colon) string-capitalize)
			 ((colon-at) string-upcase)
			 (else string-downcase)))
		 (anychar-dispatch))
		((#\))			; Case conversion end
		 (if (not format:case-conversion)
		     (log-error 'format "missing ~~("))
		 (set! format:case-conversion #f)
		 (anychar-dispatch))
		((#\[)			; Conditional begin
		 (set! conditional-nest (+ conditional-nest 1))
		 (cond
		  ((= conditional-nest 1)
		   (set! clause-pos format:pos)
		   (set! clause-default #f)
		   (set! clauses '())
		   (set! conditional-type
			 (case modifier
			   ((at) 'if-then)
			   ((colon) 'if-else-then)
			   ((colon-at) (log-error 'format "illegal modifier in ~~["))
			   (else 'num-case)))
		   (set! conditional-arg
			 (if (one-positive-integer? params)
			     (car params)
			     (next-arg)))))
		 (anychar-dispatch))
		((#\;)			; Conditional separator
		 (if (zero? conditional-nest)
		     (log-error 'format "~~; not in ~~[~~] conditional"))
		 (if (not (null? params))
		     (log-error 'format "no parameter allowed in ~~;"))
		 (if (= conditional-nest 1)
		     (let ((clause-str
			    (cond
			     ((eq? modifier 'colon)
			      (set! clause-default #t)
			      (substring format-string clause-pos
					 (- format:pos 3)))
			     ((memq modifier '(at colon-at))
			      (log-error 'format "illegal modifier in ~~;"))
			     (else
			      (substring format-string clause-pos
					 (- format:pos 2))))))
		       (set! clauses (append clauses (list clause-str)))
		       (set! clause-pos format:pos)))
		 (anychar-dispatch))
		((#\])			; Conditional end
		 (if (zero? conditional-nest) (log-error 'format "missing ~~["))
		 (set! conditional-nest (- conditional-nest 1))
		 (if modifier
		     (log-error 'format "no modifier allowed in ~~]"))
		 (if (not (null? params))
		     (log-error 'format "no parameter allowed in ~~]"))
		 (cond
		  ((zero? conditional-nest)
		   (let ((clause-str (substring format-string clause-pos
						(- format:pos 2))))
		     (if clause-default
			 (set! clause-default clause-str)
			 (set! clauses (append clauses (list clause-str)))))
		   (case conditional-type
		     ((if-then)
		      (if conditional-arg
			  (format:format-work (car clauses)
					      (list conditional-arg))))
		     ((if-else-then)
		      (add-arg-pos
		       (format:format-work (if conditional-arg
					       (cadr clauses)
					       (car clauses))
					   (rest-args))))
		     ((num-case)
		      (if (or (not (integer? conditional-arg))
			      (< conditional-arg 0))
			  (log-error 'format "argument not a positive integer"))
		      (if (not (and (>= conditional-arg (length clauses))
				    (not clause-default)))
			  (add-arg-pos
			   (format:format-work
			    (if (>= conditional-arg (length clauses))
				clause-default
				(list-ref clauses conditional-arg))
			    (rest-args))))))))
		 (anychar-dispatch))
		((#\{)			; Iteration begin
		 (set! iteration-nest (+ iteration-nest 1))
		 (cond
		  ((= iteration-nest 1)
		   (set! iteration-pos format:pos)
		   (set! iteration-type
			 (case modifier
			   ((at) 'rest-args)
			   ((colon) 'sublists)
			   ((colon-at) 'rest-sublists)
			   (else 'list)))
		   (set! max-iterations (if (one-positive-integer? params)
					    (car params) #f))))
		 (anychar-dispatch))
		((#\})			; Iteration end
		 (if (zero? iteration-nest) (log-error 'format "missing ~~{"))
		 (set! iteration-nest (- iteration-nest 1))
		 (case modifier
		   ((colon)
		    (if (not max-iterations) (set! max-iterations 1)))
		   ((colon-at at) (log-error 'format "illegal modifier" modifier))
		   (else (if (not max-iterations)
			     (set! max-iterations format:max-iterations))))
		 (if (not (null? params))
		     (log-error 'format "no parameters allowed in ~~}" params))
		 (if (zero? iteration-nest)
		     (let ((iteration-str
			    (substring format-string iteration-pos
				       (- format:pos (if modifier 3 2)))))
		       (if (string=? iteration-str "")
			   (set! iteration-str (next-arg)))
		       (case iteration-type
			 ((list)
			  (let ((args (next-arg))
				(args-len 0))
			    (if (not (list? args))
				(log-error 'format "expected a list argument" args))
			    (set! args-len (length args))
			    (do ((arg-pos 0 (+ arg-pos
					       (format:format-work
						iteration-str
						(list-tail args arg-pos))))
				 (i 0 (+ i 1)))
				((or (>= arg-pos args-len)
				     (and format:iteration-bounded
					  (>= i max-iterations)))))))
			 ((sublists)
			  (let ((args (next-arg))
				(args-len 0))
			    (if (not (list? args))
				(log-error 'format "expected a list argument" args))
			    (set! args-len (length args))
			    (do ((arg-pos 0 (+ arg-pos 1)))
				((or (>= arg-pos args-len)
				     (and format:iteration-bounded
					  (>= arg-pos max-iterations))))
			      (let ((sublist (list-ref args arg-pos)))
				(if (not (list? sublist))
				    (log-error 'format
						"expected a list of lists argument" args))
				(format:format-work iteration-str sublist)))))
			 ((rest-args)
			  (let* ((args (rest-args))
				 (args-len (length args))
				 (usedup-args
				  (do ((arg-pos 0 (+ arg-pos
						     (format:format-work
						      iteration-str
						      (list-tail
						       args arg-pos))))
				       (i 0 (+ i 1)))
				      ((or (>= arg-pos args-len)
					   (and format:iteration-bounded
						(>= i max-iterations)))
				       arg-pos))))
			    (add-arg-pos usedup-args)))
			 ((rest-sublists)
			  (let* ((args (rest-args))
				 (args-len (length args))
				 (usedup-args
				  (do ((arg-pos 0 (+ arg-pos 1)))
				      ((or (>= arg-pos args-len)
					   (and format:iteration-bounded
						(>= arg-pos max-iterations)))
				       arg-pos)
				    (let ((sublist (list-ref args arg-pos)))
				      (if (not (list? sublist))
					  (log-error 'format "expected list arguments" args))
				      (format:format-work iteration-str sublist)))))
			    (add-arg-pos usedup-args)))
			 (else (log-error 'format "internal error in ~~}")))))
		 (anychar-dispatch))
		((#\^)			; Up and out
		 (let* ((continue
			 (cond
			  ((not (null? params))
			   (not
			    (case (length params)
			      ((1) (zero? (car params)))
			      ((2) (= (list-ref params 0) (list-ref params 1)))
			      ((3) (<= (list-ref params 0)
				       (list-ref params 1)
				       (list-ref params 2)))
			      (else (log-error 'format "too many parameters")))))
			  (format:case-conversion ; if conversion stop conversion
			   (set! format:case-conversion string-copy) #t)
			  ((= iteration-nest 1) #t)
			  ((= conditional-nest 1) #t)
			  ((>= arg-pos arg-len)
			   (set! format:pos format-string-len) #f)
			  (else #t))))
		   (if continue
		       (anychar-dispatch))))

		;; format directive modifiers and parameters

		((#\@)			; `@' modifier
		 (if (memq modifier '(at colon-at))
		     (log-error 'format "double `@' modifier"))
		 (set! modifier (if (eq? modifier 'colon) 'colon-at 'at))
		 (tilde-dispatch))
		((#\:)			; `:' modifier
		 (if (memq modifier '(colon colon-at))
		     (log-error 'format "double `:' modifier"))
		 (set! modifier (if (eq? modifier 'at) 'colon-at 'colon))
		 (tilde-dispatch))
		((#\')			; Character parameter
		 (if modifier (log-error 'format "misplaced modifier" modifier))
		 (set! params (append params (list (char->integer (next-char)))))
		 (set! param-value-found #t)
		 (tilde-dispatch))
		((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+) ; num. paramtr
		 (if modifier (log-error 'format "misplaced modifier" modifier))
		 (let ((num-str-beg (- format:pos 1))
		       (num-str-end format:pos))
		   (do ((ch (peek-next-char) (peek-next-char)))
		       ((not (char-numeric? ch)))
		     (next-char)
		     (set! num-str-end (+ 1 num-str-end)))
		   (set! params
			 (append params
				 (list (string->number
					(substring format-string
						   num-str-beg
						   num-str-end))))))
		 (set! param-value-found #t)
		 (tilde-dispatch))
		((#\V)		 ; Variable parameter from next argum.
		 (if modifier (log-error 'format "misplaced modifier" modifier))
		 (set! params (append params (list (next-arg))))
		 (set! param-value-found #t)
		 (tilde-dispatch))
		((#\#)	       ; Parameter is number of remaining args
		 (if modifier (log-error 'format "misplaced modifier" modifier))
		 (set! params (append params (list (length (rest-args)))))
		 (set! param-value-found #t)
		 (tilde-dispatch))
		((#\,)			; Parameter separators
		 (if modifier (log-error 'format "misplaced modifier" modifier))
		 (if (not param-value-found)
		     (set! params (append params '(#f)))) ; append empty paramtr
		 (set! param-value-found #f)
		 (tilde-dispatch))
		((#\Q)			; Inquiry messages
		 (if (eq? modifier 'colon)
		     (format:out-str format:version)
		     (let ((nl (string #\newline)))
		       (format:out-str
			(string-append
			 "SLIB Common LISP format version " format:version nl
			 "  This code is in the public domain." nl
			 "  Please send bug reports to `lutzeb@cs.tu-berlin.de'"
			 nl))))
		 (anychar-dispatch))
		(else			; Unknown tilde directive
		 (log-error 'format "unknown control character"
			     (string-ref format-string (- format:pos 1))))))
	     (else (anychar-dispatch)))))) ; in case of conditional

      (set! format:pos 0)
      (anychar-dispatch)		; start the formatting
      (set! format:pos recursive-pos-save)
      arg-pos))

  (define (format:out fmt args)	       ; the output handler for a port
    ;;(set! format:case-conversion #f) ; modifier case conversion procedure
    ;;(set! format:flush-output #f)	; ~! reset
    (let ((arg-pos (format:format-work fmt args))
	  (arg-len (length args)))
      (cond ((< arg-pos arg-len)
	     (set! format:pos (string-length fmt))
	     (log-error 'format (- arg-len arg-pos) "superfluous arguments"))
	    ((> arg-pos arg-len)
	     (log-error 'format (- arg-pos arg-len) "missing arguments")))))

  ;;(set! format:pos 0)
  (if (< (length args) 1) (log-error 'format "not enough arguments"))

  ;; If the first argument is a string, then that's the format string.
  ;; (Scheme->C)
  ;; In this case, put the argument list in canonical form.
  (let ((args (if (string? (car args)) (cons #f args) args)))
    (let ((destination (car args))
	  (arglist (cdr args)))
      (cond
       ((or (and (boolean? destination) ; port output
		 destination)
	    (output-port? destination)
	    (number? destination))
	(let ((port (cond ((boolean? destination) (current-output-port))
			  ((output-port? destination) destination)
			  ((number? destination) (current-error-port)))))
	  (set! format:port port)    ; global port for output routines
	  (set! format:output-col (format:get-port-column port))
	  (format:out (car arglist) (cdr arglist))
	  (format:set-port-column! port format:output-col)
	  (if format:flush-output (force-output format:port))
	  #t))
       ((and (boolean? destination)	; string output
	     (not destination))
	(format:call-with-output-string
	    (lambda (port)
	      (set! format:port port)
	      (format:out (car arglist) (cdr arglist)))))
       (else
	(log-error 'format "illegal destination" destination))))))

;; format:obj->str returns a R4RS representation as a string of an
;; arbitrary scheme object.

;; First parameter is the object, second parameter is a boolean if
;; the representation should be slashified as `write' does.

;; It uses format:char->str which converts a character into a
;; slashified string as `write' does and which is implementation
;; dependent.

;; It uses format:iobj->str to print out internal objects as quoted
;; strings so that the output can always be processed by (read)

;; If format:read-proof is set to #t the resulting string is
;; additionally set into string quotes.

(define (format:obj->str obj slashify format:read-proof)
  (cond
   ((string? obj)
    (if slashify
	(let ((obj-len (string-length obj)))
	  (string-append
	   "\""
	   (let loop ((i 0) (j 0))   ; taken from Marc Feeley's pp.scm
	     (if (= j obj-len)
		 (string-append (substring obj i j) "\"")
		 (let ((c (string-ref obj j)))
		   (if (or (char=? c #\\)
			   (char=? c #\"))
		       (string-append (substring obj i j) "\\"
				      (loop j (+ j 1)))
		       (loop i (+ j 1))))))))
	obj))

   ((boolean? obj) (if obj "#t" "#f"))

   ((number? obj) (number->string obj))

   ((symbol? obj)
    (if format:symbol-case-conv
	(format:symbol-case-conv (symbol->string obj))
	(symbol->string obj)))

   ((char? obj)
    (if slashify
	(format:char->str obj)
	(string obj)))

   ((null? obj) "()")

   ((input-port? obj)
    (format:iobj->str obj format:read-proof))

   ((output-port? obj)
    (format:iobj->str obj format:read-proof))

   ((list? obj)
    (string-append "("
		   (let loop ((obj-list obj))
		     (if (null? (cdr obj-list))
			 (format:obj->str (car obj-list) #t format:read-proof)
			 (string-append
			  (format:obj->str (car obj-list) #t format:read-proof)
			  " "
			  (loop (cdr obj-list)))))
		   ")"))

   ((pair? obj)
    (string-append "("
		   (format:obj->str (car obj) #t format:read-proof)
		   " . "
		   (format:obj->str (cdr obj) #t format:read-proof)
		   ")"))

   ((vector? obj)
    (string-append "#" (format:obj->str (vector->list obj) #t format:read-proof)))

   (else				; only objects with an #<...>
    (format:iobj->str obj format:read-proof))))
					; representation should fall in here

;; format:iobj->str reveals the implementation dependent
;; representation of #<...> objects with the use of display and
;; format:call-with-output-string.

;; If format:read-proof is set to #t the resulting string is
;; additionally set into string quotes.

(define (format:iobj->str iobj format:read-proof)
  (if (or format:read-proof
	  format:iobj-case-conv)
      (string-append
       (if format:read-proof "\"" "")
       (if format:iobj-case-conv
	   (format:iobj-case-conv
	    (format:call-with-output-string (lambda (p) (display iobj p))))
	   (format:call-with-output-string (lambda (p) (display iobj p))))
       (if format:read-proof "\"" ""))
      (format:call-with-output-string (lambda (p) (display iobj p)))))

(define (format:par pars length index default name)
  (if (> length index)
      (let ((par (list-ref pars index)))
	(if par
	    (if name
		(if (< par 0)
		    (log-error name "parameter must be a positive integer")
		    par)
		par)
	    default))
      default))

;; format:char->str converts a character into a slashified string as
;; done by `write'. The procedure is dependent on the integer
;; representation of characters and assumes a character number
;; according to the ASCII character set.

(define (format:char->str ch)
  (let ((int-rep (char->integer ch)))
    (if (< int-rep 0)			; if chars are [-128...+127]
	(set! int-rep (+ int-rep 256)))
    (string-append
     "#\\"
     (cond
      ((char=? ch #\newline) "newline")
      ((and (>= int-rep 0) (<= int-rep 32))
       (vector-ref format:ascii-non-printable-charnames int-rep))
      ((= int-rep 127) "del")
      ((>= int-rep 128)			; octal representation
       (if format:radix-pref
	   (let ((s (number->string int-rep 8)))
	     (substring s 2 (string-length s)))
	   (number->string int-rep 8)))
      (else (string ch))))))

;;; We should keep separate track of columns for each port, but
;;; keeping pointers to ports will foil GC.  Instead, keep
;;; associations indexed by the DISPLAYed representation of the ports.
(define *port-columns* '())
(define (format:get-port-column port)
  (define pair (assoc (format:call-with-output-string
			  (lambda (sport) (display port sport)))
		      *port-columns*))
  (if pair (cdr pair) 0))
(define (format:set-port-column! port col)
  (define pname (format:call-with-output-string
		    (lambda (sport) (display port sport))))
  (let ((pair (assoc pname *port-columns*)))
    (if pair
	(set-cdr! pair col)
	(set! *port-columns* (cons (cons pname col) *port-columns*)))))

;;; some global functions not found in SLIB

(define (format:string-capitalize-first str) ; "hello" -> "Hello"
  (let ((cap-str (string-copy str))	; "hELLO" -> "Hello"
	(non-first-alpha #f)		; "*hello" -> "*Hello"
	(str-len (string-length str)))	; "hello you" -> "Hello you"
    (do ((i 0 (+ i 1)))
	((= i str-len) cap-str)
      (let ((c (string-ref str i)))
	(if (char-alphabetic? c)
	    (if non-first-alpha
		(string-set! cap-str i (char-downcase c))
		(begin
		  (set! non-first-alpha #t)
		  (string-set! cap-str i (char-upcase c)))))))))

;; eof
