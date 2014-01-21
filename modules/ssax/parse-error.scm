
;;(include "../../libs/gambit/myenv.sch")

; This code provides informative error messages
;   for SSAX (S)XML parser.
;
;
; NOTE: Gambit-specific ! 
; It was tested with SSAX version 4.2
;
; $Id: ssax-error.scm,v 1.1 2001/03/06 20:10:15 kl Exp kl $

;;;; Port information primitives.
;;;; Look at lib/_io.scm from gambit sources for more details.

;;; Position
(##define-macro (port-input-char-count port)
   `(##vector-ref ,port 16))

;;; Lines count
(##define-macro (port-input-line-count port)
   `(##vector-ref ,port 17))

;;; 
(##define-macro (port-input-line-start port)
   `(##vector-ref ,port 18))

;;; Input file name (Gambit uses full path) or
;;;   or: (stdin) / (stdout) / (stderr)
;;;   or: (string) for string port
(##define-macro (port-name port)
  `(##vector-ref ,port 4))

;;;; Error handler
;;;; Use it as a replacement for "error"

;;; According to the SSAX convention this function
;;; accepts the port as its first argument which is used for
;;; location of the error in input file.
;;; Other parameters are considered as error messages,
;;;  they are printed to stderr as is.

#|
(define parser-error
  (lambda  args
    (if
      (##port? (car args))
      (begin
	(cerr nl "Error: " (port-name (car args)))
	(cerr " at position " (input-port-byte-position (car args)) nl)
	; (apply cerr (cdr args)))  
	(cerr (cdr args)))
      (cerr nl "Error in error handler :-) " nl args))
    (cerr nl)
    (exit -1)))

(define SSAX:warn
  (lambda  args
    (if
      (##port? (car args))
	(cerr nl "Warning: " (port-name (car args))
	      " at position " (input-port-byte-position (car args)) nl
	      (cdr args) nl)
	#f)
    ))
|#

(define (parser-error p . x)
  (set! ssax:errormsg (with-output-to-string "" (lambda ()
    (for-each display (append (list "Error: ") x)))))
  (log-system "SSAX: " ssax:errormsg))

(define (SSAX:warn p . x)
  (set! ssax:warningmsg (with-output-to-string "" (lambda ()
    (for-each display (append (list "Warning: ") x)))))
  (log-system "SSAX: " ssax:warningmsg))

; DL: case sensitivity
(define ssax:warn SSAX:warn)
