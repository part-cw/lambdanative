; Top-level cond-expand expanded automatically
(define-macro (_gid id)
  (string->symbol (string-append "##" (symbol->string id))))
