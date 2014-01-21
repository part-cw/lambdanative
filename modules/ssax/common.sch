
; SRFI-2 and-let* : it evaluates a sequence of forms one after another
; till the first one that yields #f; the non-#f result of a form can be bound
; to a fresh variable and used in the subsequent forms.
;  From Oleg Kiselyovs's vland.scm,v 1.0 1998/02/21 
(define-macro (and-let* claws . body)
  (let* ((new-vars '()) (result (cons 'and '())) (growth-point result))
			; We need a way to report a syntax error
			; the following is how Gambit compiler does it...
    (##define-macro (ct-error-syntax msg . args)
      `((lambda x #t) '##signal.syntax-error #t ,msg ,@args))
      ;`(##signal '##signal.syntax-error #t ,msg ,@args))

    (define (andjoin! clause)
      (let ((prev-point growth-point) (clause-cell (cons clause '())))
        (set-cdr! growth-point clause-cell)
        (set! growth-point clause-cell)))

    (if (not (list? claws))
      (ct-error-syntax "bindings must be a list " bindings))
    (for-each 
      (lambda (claw)
        (cond
          ((symbol? claw)	; BOUND-VARIABLE form
            (andjoin! claw))
          ((and (pair? claw) (null? (cdr claw))) ; (EXPRESSION) form
            (andjoin! (car claw)))
						; (VARIABLE EXPRESSION) form
          ((and (pair? claw) (symbol? (car claw))
              (pair? (cdr claw)) (null? (cddr claw)))
            (let* ((var (car claw)) (var-cell (cons var '())))
              (if (memq var new-vars)
                (ct-error-syntax "duplicate variable " var " in the bindings"))
              (set! new-vars (cons var new-vars))
              (set-cdr! growth-point `((let (,claw) (and . ,var-cell))))
              (set! growth-point var-cell)))
          (else
            (ct-error-syntax "An ill-formed binding in a syntactic form land* " 
              claw))
        ))
      claws)
    (if (not (null? body))
      (andjoin! `(begin ,@body)))
    result))
