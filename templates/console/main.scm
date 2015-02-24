;; LambdaNative console template

;; place your code here

;; this will provide an interactive prompt
(let loop ()
  (with-exception-catcher (lambda (e)
    (for-each display (list (exception->string e) "\n")) #f)
      (lambda () (##repl-debug)))
  (loop))

;; eof
