
;;(include "../libs/gambit/common.sch")
;;(include "../libs/gambit/myenv.sch")

; Top-level cond-expand expanded automatically
(define (any? <pred?> coll)
  (cond
   ((list? coll)
    (let loop ((curr-l coll))
      (if (null? curr-l) #f (or (<pred?> (car curr-l)) (loop (cdr curr-l))))))
   ((vector? coll)
    (let ((len (vector-length coll)))
      (let loop ((i 0))
        (if (>= i len) #f (or (<pred?> (vector-ref coll i)) (loop (inc i)))))))
   ((string? coll)
    (let ((len (string-length coll)))
      (let loop ((i 0))
        (if (>= i len) #f (or (<pred?> (string-ref coll i)) (loop (inc i)))))))
   ((input-port? coll)
    (let loop ((c (read-char coll)))
      (if (eof-object? c) #f (or (<pred?> c) (loop (read-char coll))))))
   (else (error "any? on an invalid collection"))))


;------------------------------------------------------------------------
;		Some list manipulation functions

; -- procedure+: list-intersperse SRC-L ELEM
; inserts ELEM between elements of the SRC-L, returning a freshly allocated
; list (cells, that is)
      
(define (list-intersperse src-l elem)
  (if (null? src-l) src-l
    (let loop ((l (cdr src-l)) (dest (cons (car src-l) '())))
      (if (null? l) (reverse dest)
        (loop (cdr l) (cons (car l) (cons elem dest)))))))


; -- procedure+: list-intersperse! SRC-L ELEM
; inserts ELEM between elements of the SRC-L inplace
      
(define (list-intersperse! src-l elem)
  (if (null? src-l) src-l
    (let loop ((l src-l))
      (let ((next-l (cdr l)))
        (if (null? next-l) src-l
          (begin
            (set-cdr! l (cons elem next-l))
            (loop next-l)))))))


	; List-tail-difference: given two lists, list1 and list2 where
	; list2 is presumably a tail of list1, return 
	; a (freshly allocated) list which is a difference between list1 
	; and list2. If list2 is *not* a tail of list1, the entire list1
	; is returned.
(define (list-tail-diff list1 list2)
  (let loop ((l1-curr list1) (difference '()))
    (cond
      ((eq? l1-curr list2) (reverse difference))
      ((null? l1-curr) (reverse difference))
      (else (loop (cdr l1-curr) (cons (car l1-curr) difference))))))


;------------------------------------------------------------------------
;			String utilities
; See SRFI-13 or srfi-13-local.scm


; Return the index of the last occurence of a-char in str, or #f
; See SRFI-13
(define string-rindex string-index-right)

; -- procedure+: substring? PATTERN STRING
;     Searches STRING to see if it contains the substring PATTERN.
;     Returns the index of the first substring of STRING that is equal
;     to PATTERN; or `#f' if STRING does not contain PATTERN.
;
;          (substring? "rat" "pirate")             =>  2
;          (substring? "rat" "outrage")            =>  #f
;          (substring? "" any-string)              =>  0
(define (substring? pattern str) (string-contains str pattern))
; Top-level cond-expand expanded automatically
(define (string->integer str start end)
  (and (< -1 start end (inc (string-length str)))
       (let loop ((pos start) (accum 0))
         (cond
          ((>= pos end) accum)
          ((char-numeric? (string-ref str pos))
           (loop
            (inc pos)
            (+
             (char->integer (string-ref str pos))
             (- (char->integer #\0))
             (* 10 accum))))
          (else #f)))))


; 
; -- procedure+: ssax-string-split STRING
; -- procedure+: ssax-string-split STRING '()
; -- procedure+: ssax-string-split STRING '() MAXSPLIT
;
; Returns a list of whitespace delimited words in STRING.
; If STRING is empty or contains only whitespace, then the empty list
; is returned. Leading and trailing whitespaces are trimmed.
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; -- procedure+: ssax-string-split STRING CHARSET
; -- procedure+: ssax-string-split STRING CHARSET MAXSPLIT
;
; Returns a list of words delimited by the characters in CHARSET in
; STRING. CHARSET is a list of characters that are treated as delimiters.
; Leading or trailing delimeters are NOT trimmed. That is, the resulting
; list will have as many initial empty string elements as there are
; leading delimiters in STRING.
;
; If MAXSPLIT is specified and positive, the resulting list will
; contain at most MAXSPLIT elements, the last of which is the string
; remaining after (MAXSPLIT - 1) splits. If MAXSPLIT is specified and
; non-positive, the empty list is returned. "In time critical
; applications it behooves you not to split into more fields than you
; really need."
;
; This is based on the split function in Python/Perl
;
; (ssax-string-split " abc d e f  ") ==> ("abc" "d" "e" "f")
; (ssax-string-split " abc d e f  " '() 1) ==> ("abc d e f  ")
; (ssax-string-split " abc d e f  " '() 0) ==> ()
; (ssax-string-split ":abc:d:e::f:" '(#\:)) ==> ("" "abc" "d" "e" "" "f" "")
; (ssax-string-split ":" '(#\:)) ==> ("" "")
; (ssax-string-split "root:x:0:0:Lord" '(#\:) 2) ==> ("root" "x:0:0:Lord")
; (ssax-string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:))
; ==> ("/usr/local/bin" "/usr/bin" "/usr/ucb/bin")
; (ssax-string-split "/usr/local/bin" '(#\/)) ==> ("" "usr" "local" "bin")

(define (ssax-string-split str . rest)
		; maxsplit is a positive number
  (define (split-by-whitespace str maxsplit)
    (define (skip-ws i yet-to-split-count)
      (cond
        ((>= i (string-length str)) '())
        ((char-whitespace? (string-ref str i))
          (skip-ws (inc i) yet-to-split-count))
        (else (scan-beg-word (inc i) i yet-to-split-count))))
    (define (scan-beg-word i from yet-to-split-count)
      (cond
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word i from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((char-whitespace? (string-ref str i))
          (cons (substring str from i) 
            (skip-ws (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (skip-ws 0 (- maxsplit 1)))

		; maxsplit is a positive number
		; str is not empty
  (define (split-by-charset str delimeters maxsplit)
    (define (scan-beg-word from yet-to-split-count)
      (cond
        ((>= from (string-length str)) '(""))
        ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
        (else (scan-word from from yet-to-split-count))))
    (define (scan-word i from yet-to-split-count)
      (cond
        ((>= i (string-length str))
          (cons (substring str from i) '()))
        ((memq (string-ref str i) delimeters)
          (cons (substring str from i) 
            (scan-beg-word (inc i) (- yet-to-split-count 1))))
        (else (scan-word (inc i) from yet-to-split-count))))
    (scan-beg-word 0 (- maxsplit 1)))

			; resolver of overloading...
			; if omitted, maxsplit defaults to
			; (inc (string-length str))
  (if (string-null? str) '()
    (if (null? rest) 
      (split-by-whitespace str (inc (string-length str)))
      (let ((charset (car rest))
          (maxsplit
            (if (pair? (cdr rest)) (cadr rest) (inc (string-length str)))))
        (cond 
          ((not (positive? maxsplit)) '())
          ((null? charset) (split-by-whitespace str maxsplit))
          (else (split-by-charset str charset maxsplit))))))
)


; make-char-quotator QUOT-RULES
;
; Given QUOT-RULES, an assoc list of (char . string) pairs, return
; a quotation procedure. The returned quotation procedure takes a string
; and returns either a string or a list of strings. The quotation procedure
; check to see if its argument string contains any instance of a character
; that needs to be encoded (quoted). If the argument string is "clean",
; it is returned unchanged. Otherwise, the quotation procedure will
; return a list of string fragments. The input straing will be broken
; at the places where the special characters occur. The special character
; will be replaced by the corresponding encoding strings.
;
; For example, to make a procedure that quotes special HTML characters,
; do
;	(make-char-quotator
;	    '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;") (#\" . "&quot;")))

(define (make-char-quotator char-encoding)
  (let ((bad-chars (map car char-encoding)))

    ; Check to see if str contains one of the characters in charset,
    ; from the position i onward. If so, return that character's index.
    ; otherwise, return #f
    (define (index-cset str i charset)
      (let loop ((i i))
	(and (< i (string-length str))
	     (if (memv (string-ref str i) charset) i
		 (loop (inc i))))))

    ; The body of the function
    (lambda (str)
      (let ((bad-pos (index-cset str 0 bad-chars)))
	(if (not bad-pos) str	; str had all good chars
	    (let loop ((from 0) (to bad-pos))
	      (cond
	       ((>= from (string-length str)) '())
	       ((not to)
		(cons (substring str from (string-length str)) '()))
	       (else
		(let ((quoted-char
		       (cdr (assv (string-ref str to) char-encoding)))
		      (new-to 
		       (index-cset str (inc to) bad-chars)))
		  (if (< from to)
		      (cons
		       (substring str from to)
		       (cons quoted-char (loop (inc to) new-to)))
		      (cons quoted-char (loop (inc to) new-to))))))))))
))
