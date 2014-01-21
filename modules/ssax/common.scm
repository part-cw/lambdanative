
;;(include "../../libs/gambit/common.sch")

;(define command-line argv)
(define system ##shell-command)

(define close-output-string close-output-port)

;; Port information: see lib/_io.scm from gambit sources.

(##define-macro (port-input-char-count port)                                    
   `(##vector-ref ,port 16)) 

(##define-macro (port-input-line-count port)                                    
   `(##vector-ref ,port 17)) 

(##define-macro (port-input-line-start port)                                    
   `(##vector-ref ,port 18))                                                    

(##define-macro (port-name port)
  `(##vector-ref ,port 4))

; This function is based on SRFI-1 Reference implementation by Olin Shivers. 
; Original version may be found at  http://www.ai.mit.edu/~shivers/
;  Changes: Parameter-checking is removed
;           Dotted lists permitted
(define (filter pred lis)			
  (let recur ((lis lis))		
    (if (not (pair? lis)) lis			
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))	; Replicate the RECUR call so
		(if (eq? tail new-tail) lis
		    (cons head new-tail)))
	      (recur tail))))))			; this one can be a tail call.
