;;; Gambit-c requires that macro-code be `included` directly into
;;; source code that uses it.  Hence, we separate the macro code from
;;; the main body of the scran so that the main body of the code
;;; doesn't need to be endlessly recompiled.

(define-macro (define-component name args #!rest body)
  `(define ,name (component! (lambda ,args ,@body) (symbol->string ',name))))
