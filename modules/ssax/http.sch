;^^^^^^^


; syntax: define-def ident assoc-list defaultvalue
; Bind a variable ident to a value found in an assoc list.
; assoc-list is a list of pairs (symbol . value)
; We look up 'ident' in the assoc-list, and bind it to the found value, unless
; the latter is #f.
; If the lookup fails, the defaultvalue is used.

(define-macro (define-def ident assoc-list defaultvalue)
  `(define ,ident 
     (or
      (cond
       ((assq ',ident ,assoc-list) => cdr)
       (else #f))
      ,defaultvalue)))
