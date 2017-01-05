;;; Gambit-c requires that macro-code be `included` directly into
;;; source code that uses it.  Hence, we separate the macro code from
;;; the main body of the scran so that the main body of the code
;;; doesn't need to be endlessly recompiled.

(##define-syntax define-component
  (lambda (stx)
    (define (ensure-code o)
      (if (##source? o) (##source-code o) o))
    (let* ((code (cdr (ensure-code stx)))
	   (name (car code))
	   (args (car (cdr code)))
	   (body (cdr (cdr code))))
      (##sourcify
       `(define ,name (component! (lambda ,args ,@body) (symbol->string ',name))) stx))))

(##define-syntax define-system
  (lambda (stx)
    (define (ensure-code o)
      (if (##source? o) (##source-code o) o))
    (let* ((code (cdr (ensure-code stx)))
	   (name (car code))
	   (body (cdr code)))
      (##sourcify
       `(define ,name (system! ,@body)) stx))))

