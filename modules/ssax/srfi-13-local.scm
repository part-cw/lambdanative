
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

; Return the index of the last occurence of a-char in str, or #f
; This is a subset of the corresponding SRFI-13 function.
; The latter is more generic.

(define (string-index-right str a-char)
  (let loop ((pos (dec (string-length str))))
    (cond
      ((negative? pos) #f) 	; whole string has been searched, in vain
      ((char=? a-char (string-ref str pos)) pos)
      (else (loop (dec pos))))))
