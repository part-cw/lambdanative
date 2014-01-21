
;;(include "../../libs/gambit/myenv.sch")


(define (assure exp error-msg)
  (or exp (error error-msg)))

;==============================================================================
; Error handling

; DL: commented out, since `##identify-error' is not defined in Gambit 4
;(define (identify-error msg args . disposition-msgs)
;  (##identify-error "ERROR" #f #f msg args disposition-msgs))

;==============================================================================
; Monadic IO

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)
(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x)
                  (x (current-error-port))
                  (display x (current-error-port))))
            args))

(define nl (string #\newline))

(define (cons* a1 a2 . rest)
  (if (null? rest)
      (cons a1 a2)
      (cons a1 (apply cons* (cons a2 rest)))))
