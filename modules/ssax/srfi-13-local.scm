
;;(include "../libs/gambit/myenv.sch")
;;(include "../libs/gambit/common.sch")

; Top-level cond-expand expanded automatically
(define (string-xcopy! target tstart s sfrom sto)
  (do ((i sfrom (inc i)) (j tstart (inc j)))
      ((>= i sto))
    (string-set! target j (string-ref s i))))


; procedure string-concatenate-reverse STRINGS FINAL END
(define (string-concatenate-reverse strs final end)
  (if (null? strs) (substring final 0 end)
    (let*
      ((total-len
	 (let loop ((len end) (lst strs))
	   (if (null? lst) len
	     (loop (+ len (string-length (car lst))) (cdr lst)))))
	(result (make-string total-len)))
      (let loop ((len end) (j total-len) (str final) (lst strs))
	(string-xcopy! result (- j len) str 0 len)
	(if (null? lst) result
	  (loop (string-length (car lst)) (- j len)
	    (car lst) (cdr lst)))))))


; string-concatenate/shared STRING-LIST -> STRING
(define (string-concatenate/shared strs)
  (cond
    ((null? strs) "")			; Test for the fast path first
    ((null? (cdr strs)) (car strs))
    (else
      (let*
	((total-len
	   (let loop ((len (string-length (car strs))) (lst (cdr strs)))
	   (if (null? lst) len
	     (loop (+ len (string-length (car lst))) (cdr lst)))))
	  (result (make-string total-len)))
	(let loop ((j 0) (str (car strs)) (lst (cdr strs)))
	  (string-xcopy! result j str 0 (string-length str))
	  (if (null? lst) result
	    (loop (+ j (string-length str))
	      (car lst) (cdr lst))))))))


; string-concatenate-reverse/shared STRING-LIST [FINAL-STRING END] -> STRING
; We do not use the optional arguments of this procedure. Therefore,
; we do not implement them. See SRFI-13 for the complete
; implementation.
(define (string-concatenate-reverse/shared strs)
  (cond
    ((null? strs) "")			; Test for the fast path first
    ((null? (cdr strs)) (car strs))
    (else
      (string-concatenate-reverse (cdr strs)
	(car strs) (string-length (car strs))))))



; Return the index of the first occurence of a-char in str, or #f
; This is a subset of the corresponding SRFI-13 function.
; The latter is more generic.

(define (string-index str a-char)
  (let loop ((pos 0))
    (cond
      ((>= pos (string-length str)) #f) ; whole string has been searched, in vain
      ((char=? a-char (string-ref str pos)) pos)
      (else (loop (inc pos))))))

; Return the index of the last occurence of a-char in str, or #f
; This is a subset of the corresponding SRFI-13 function.
; The latter is more generic.

(define (string-index-right str a-char)
  (let loop ((pos (dec (string-length str))))
    (cond
      ((negative? pos) #f) 	; whole string has been searched, in vain
      ((char=? a-char (string-ref str pos)) pos)
      (else (loop (dec pos))))))


; string-contains    s1 s2 [start1 end1 start2 end2] -> integer or false
; string-contains-ci s1 s2 [start1 end1 start2 end2] -> integer or false
;     Does string s1 contain string s2?
;     Return the index in s1 where s2 occurs as a substring, or false. The
;     optional start/end indices restrict the operation to the indicated
;     substrings.
; We do not support the optional arguments
(define (string-contains str pattern)
  (let* ((pat-len (string-length pattern))
         (search-span (- (string-length str) pat-len))
         (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
         (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
    (cond
     ((not c1) 0)           ; empty pattern, matches upfront
     ((not c2) (string-index str c1)) ; one-char pattern
     (else                  ; matching a pattern of at least two chars
	(let outer ((pos 0))
          (cond
	    ((> pos search-span) #f)	; nothing was found thru the whole str
            ((not (char=? c1 (string-ref str pos)))
                (outer (+ 1 pos)))	; keep looking for the right beginning
            ((not (char=? c2 (string-ref str (+ 1 pos))))
                (outer (+ 1 pos)))	; could've done pos+2 if c1 == c2....
            (else                  	; two char matched: high probability
				   	; the rest will match too
		(let inner ((i-pat 2) (i-str (+ 2 pos)))
                   (if (>= i-pat pat-len) pos ; whole pattern matched
                      (if (char=? (string-ref pattern i-pat)
                                  (string-ref str i-str))
                        (inner (+ 1 i-pat) (+ 1 i-str))
                        (outer (+ 1 pos))))))))))))	; mismatch after partial match


; Here are some specialized substring? functions
;
; -- procedure+: string-prefix? PATTERN STRING
; -- procedure+: string-prefix-ci? PATTERN STRING
; checks to make sure that PATTERN is a prefix of STRING
;
;          (string-prefix? "pir" "pirate")             =>  #t
;          (string-prefix? "rat" "outrage")            =>  #f
;          (string-prefix? "" any-string)              =>  #t
;          (string-prefix? any-string any-string)      =>  #t

(define (string-prefix? pattern str)
  (let loop ((i 0))
    (cond
      ((>= i (string-length pattern)) #t)
      ((>= i (string-length str)) #f)
      ((char=? (string-ref pattern i) (string-ref str i))
        (loop (inc i)))
      (else #f))))

(define (string-prefix-ci? pattern str)
  (let loop ((i 0))
    (cond
      ((>= i (string-length pattern)) #t)
      ((>= i (string-length str)) #f)
      ((char-ci=? (string-ref pattern i) (string-ref str i))
        (loop (inc i)))
      (else #f))))

; -- procedure+: string-suffix? PATTERN STRING
; -- procedure+: string-suffix-ci? PATTERN STRING
; checks to make sure that PATTERN is a suffix of STRING
;
;          (string-suffix? "ate" "pirate")             =>  #t
;          (string-suffix? "rag" "outrage")            =>  #f
;          (string-suffix? "" any-string)              =>  #t
;          (string-suffix? any-string any-string)      =>  #t

(define (string-suffix? pattern str)
  (let loop ((i (dec (string-length pattern))) (j (dec (string-length str))))
    (cond
      ((negative? i) #t)
      ((negative? j) #f)
      ((char=? (string-ref pattern i) (string-ref str j))
        (loop (dec i) (dec j)))
      (else #f))))

(define (string-suffix-ci? pattern str)
  (let loop ((i (dec (string-length pattern))) (j (dec (string-length str))))
    (cond
      ((negative? i) #t)
      ((negative? j) #f)
      ((char-ci=? (string-ref pattern i) (string-ref str j))
        (loop (dec i) (dec j)))
      (else #f))))
; Top-level cond-expand expanded automatically
(define (string-downcase str)
  (do ((target-str (make-string (string-length str))) (i 0 (inc i)))
      ((>= i (string-length str)) target-str)
    (string-set! target-str i (char-downcase (string-ref str i)))))

(define (string-upcase str)
  (do ((target-str (make-string (string-length str))) (i 0 (inc i)))
      ((>= i (string-length str)) target-str)
    (string-set! target-str i (char-upcase (string-ref str i)))))
; Top-level cond-expand expanded automatically
(define (string-downcase! str)
  (do ((i 0 (inc i)))
      ((>= i (string-length str)))
    (string-set! str i (char-downcase (string-ref str i)))))

(define (string-upcase! str)
  (do ((i 0 (inc i)))
      ((>= i (string-length str)))
    (string-set! str i (char-upcase (string-ref str i)))))
