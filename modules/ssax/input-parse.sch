; Top-level cond-expand expanded automatically
(define-macro (define-opt bindings body . body-rest)
  (let* ((rev-bindings (reverse bindings))
         (opt-bindings
           (and (pair? rev-bindings)
                (pair? (car rev-bindings))
                (eq? 'optional (caar rev-bindings))
                (cdar rev-bindings))))
    (if opt-bindings
      `(define (unquote
                (append
                  (reverse
                    (cons
                     (with-input-from-string "#!optional" read)
                     (cdr rev-bindings)))
                  opt-bindings))
         ,body
         ,@body-rest)
      `(define ,bindings ,body ,@body-rest))))
; Top-level cond-expand expanded automatically
(define-macro (gambitize clause)
  `(define-macro ,clause
     ,(list
       'quasiquote
       (cons
        (string->symbol (string-append "##" (symbol->string (car clause))))
        (map (lambda (id) (list 'unquote id)) (cdr clause))))))
