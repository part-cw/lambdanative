; $Id: SSAX.scm,v 1.8 2003/08/02 02:14:18 oleg Exp $
; DL: let*-values rewritten to call-with-values

(define-macro run-test
  (lambda body
    (define (re-write body)
      (cond
       ((vector? body) (list->vector (re-write (vector->list body))))
       ((not (pair? body)) body)
       ((and (eq? 'quote (car body)) (pair? (cdr body)) (string? (cadr body)))
        (string->symbol (cadr body)))
       (else (cons (re-write (car body)) (re-write (cdr body))))))
    (cons 'begin (re-write body))))
(define-macro xml-token-kind (lambda (token) `(car ,token)))
(define-macro xml-token-head (lambda (token) `(cdr ,token)))
(define-macro ssax:make-pi-parser
  (lambda (my-pi-handlers)
    `(lambda (port target seed)
       (case target
         (unquote-splicing
          (let loop ((pi-handlers my-pi-handlers) (default #f))
            (cond
             ((null? pi-handlers)
              (if default
                `((else (,default port target seed)))
                '((else
                   (ssax:warn port "Skipping PI: " target nl)
                   (ssax:skip-pi port)
                   seed))))
             ((eq? '*DEFAULT* (caar pi-handlers))
              (loop (cdr pi-handlers) (cdar pi-handlers)))
             (else
              (cons
               `((,(caar pi-handlers)) (,(cdar pi-handlers) port target seed))
               (loop (cdr pi-handlers) default))))))))))
(define-macro ssax:make-elem-parser
  (lambda (my-new-level-seed
           my-finish-element
           my-char-data-handler
           my-pi-handlers)
    `(lambda (start-tag-head port elems entities namespaces preserve-ws? seed)
       (define xml-space-gi (cons ssax:Prefix-XML (string->symbol "space")))
       (let handle-start-tag ((start-tag-head start-tag-head)
                              (port port)
                              (entities entities)
                              (namespaces namespaces)
                              (preserve-ws? preserve-ws?)
                              (parent-seed seed))
         (call-with-values
          (lambda () (ssax:complete-start-tag
                          start-tag-head
                          port
                          elems
                          entities
                          namespaces))
          (lambda (elem-gi attributes namespaces expected-content)
            (let ((seed (,my-new-level-seed
                         elem-gi
                         attributes
                         namespaces
                         expected-content
                         parent-seed)))
              (case expected-content
                ((EMPTY-TAG)
                 (,my-finish-element
                  elem-gi
                  attributes
                  namespaces
                  parent-seed
                  seed))
                ((EMPTY)
                 (ssax:assert-token
                  (and (eqv? #\< (ssax:skip-S port))
                       (ssax:read-markup-token port))
                  'END
                  start-tag-head
                  (lambda (token exp-kind exp-head)
                    (parser-error
                     port
                     "[elementvalid] broken for "
                     token
                     " while expecting "
                     exp-kind
                     exp-head)))
                 (,my-finish-element
                  elem-gi
                  attributes
                  namespaces
                  parent-seed
                  seed))
                (else
                 (let ((preserve-ws?
                        (cond
                          ((assoc xml-space-gi attributes)
                           =>
                           (lambda (name-value)
                             (equal? "preserve" (cdr name-value))))
                          (else preserve-ws?))))
                   (let loop ((port port)
                              (entities entities)
                              (expect-eof? #f)
                              (seed seed))
                     (call-with-values
                      (lambda () (ssax:read-char-data
                                  port
                                  expect-eof?
                                  ,my-char-data-handler
                                  seed))
                      (lambda (seed term-token)
                        (if (eof-object? term-token)
                            seed
                            (case (xml-token-kind term-token)
                              ((END)
                               (ssax:assert-token
                                term-token
                                'END
                                start-tag-head
                                (lambda (token exp-kind exp-head)
                                  (parser-error
                                   port
                                   "[GIMatch] broken for "
                                   term-token
                                   " while expecting "
                                   exp-kind
                                   exp-head)))
                               (,my-finish-element
                                elem-gi
                                attributes
                                namespaces
                                parent-seed
                                seed))
                              ((PI)
                               (let ((seed
                                      ((ssax:make-pi-parser ,my-pi-handlers)
                                       port
                                       (xml-token-head term-token)
                                       seed)))
                                 (loop port entities expect-eof? seed)))
                              ((ENTITY-REF)
                               (let ((seed
                                      (ssax:handle-parsed-entity
                                       port
                                       (xml-token-head term-token)
                                       entities
                                       (lambda (port entities seed)
                                         (loop port entities #t seed))
                                       ,my-char-data-handler
                                       seed)))
                                 (loop port entities expect-eof? seed)))
                              ((START)
                               (if (eq? expected-content 'PCDATA)
                                   (parser-error
                                    port
                                    "[elementvalid] broken for "
                                    elem-gi
                                    " with char content only; unexpected token "
                                    term-token))
                               (let ((seed
                                      (handle-start-tag
                                       (xml-token-head term-token)
                                       port
                                       entities
                                       namespaces
                                       preserve-ws?
                                       seed)))
                                 (loop port entities expect-eof? seed)))
                              (else
                               (parser-error
                                port
                                "XML [43] broken for "
                                term-token)))))))))))))))))
(define-macro ssax:make-parser
  (lambda user-handlers
    (define all-handlers
      '((DOCTYPE
          lambda
          (port docname systemid internal-subset? seed)
          (when internal-subset?
            (ssax:warn port "Internal DTD subset is not currently handled ")
            (ssax:skip-internal-dtd port))
          (ssax:warn
            port
            "DOCTYPE DECL "
            docname
            " "
            systemid
            " found and skipped")
          (values #f '() '() seed))
        (UNDECL-ROOT lambda (elem-gi seed) (values #f '() '() seed))
        (DECL-ROOT lambda (elem-gi seed) seed)
        (NEW-LEVEL-SEED . REQD)
        (FINISH-ELEMENT . REQD)
        (CHAR-DATA-HANDLER . REQD)
        (PI)))
    (define (delete-assoc alist tag cont)
      (let loop ((alist alist) (scanned '()))
        (cond
         ((null? alist) (error "Unknown user-handler-tag: " tag))
         ((eq? tag (caar alist))
          (cont tag (cdar alist) (append scanned (cdr alist))))
         (else (loop (cdr alist) (cons (car alist) scanned))))))
    (define (merge-handlers declared-handlers given-handlers)
      (cond
       ((null? given-handlers)
        (cond
         ((null? declared-handlers) '())
         ((not (eq? 'REQD (cdar declared-handlers)))
          (cons
           (car declared-handlers)
           (merge-handlers (cdr declared-handlers) given-handlers)))
         (else
          (error
           "The handler for the tag "
           (caar declared-handlers)
           " must be specified"))))
       ((null? (cdr given-handlers))
        (error "Odd number of arguments to ssax:make-parser"))
       (else
        (delete-assoc
          declared-handlers
          (car given-handlers)
          (lambda (tag value alist)
            (cons
             (cons tag (cadr given-handlers))
             (merge-handlers alist (cddr given-handlers))))))))
    (let ((user-handlers (merge-handlers all-handlers user-handlers)))
      (define (get-handler tag)
        (cond
         ((assq tag user-handlers) => cdr)
         (else (error "unknown tag: " tag))))
      `(lambda (port seed)
         (define (handle-decl port token-head seed)
           (or (eq? (string->symbol "DOCTYPE") token-head)
               (parser-error
                 port
                 "XML [22], expected DOCTYPE declaration, found "
                 token-head))
           (assert-curr-char ssax:S-chars "XML [28], space after DOCTYPE" port)
           (ssax:skip-S port)
           (let* ((docname (ssax:read-QName port))
                  (systemid
                    (and (ssax:ncname-starting-char? (ssax:skip-S port))
                         (ssax:read-external-id port)))
                  (internal-subset?
                   (begin
                     (ssax:skip-S port)
                     (eqv?
                      #\[
                      (assert-curr-char
                       '(#\> #\[)
                       "XML [28], end-of-DOCTYPE"
                       port)))))
             (call-with-values
              (lambda () (,(get-handler 'DOCTYPE)
                          port
                          docname
                          systemid
                          internal-subset?
                          seed))
              (lambda (elems entities namespaces seed)
                (scan-for-significant-prolog-token-2
                 port
                 elems
                 entities
                 namespaces
                 seed)))))
         (define (scan-for-significant-prolog-token-1 port seed)
           (let ((token (ssax:scan-Misc port)))
             (if (eof-object? token)
               (parser-error port "XML [22], unexpected EOF")
               (case (xml-token-kind token)
                 ((PI)
                  (let ((seed
                         ((ssax:make-pi-parser ,(get-handler 'PI))
                          port
                          (xml-token-head token)
                          seed)))
                    (scan-for-significant-prolog-token-1 port seed)))
                 ((DECL) (handle-decl port (xml-token-head token) seed))
                 ((START)
                  (call-with-values
                   (lambda () (,(get-handler 'UNDECL-ROOT)
                               (xml-token-head token)
                               seed))
                   (lambda (elems entities namespaces seed)
                     (element-parser
                      (xml-token-head token)
                      port
                      elems
                      entities
                      namespaces
                      #f
                      seed))))
                 (else
                  (parser-error port "XML [22], unexpected markup " token))))))
         (define (scan-for-significant-prolog-token-2
                  port
                  elems
                  entities
                  namespaces
                  seed)
           (let ((token (ssax:scan-Misc port)))
             (if (eof-object? token)
               (parser-error port "XML [22], unexpected EOF")
               (case (xml-token-kind token)
                 ((PI)
                  (let ((seed
                         ((ssax:make-pi-parser ,(get-handler 'PI))
                          port
                          (xml-token-head token)
                          seed)))
                    (scan-for-significant-prolog-token-2
                      port
                      elems
                      entities
                      namespaces
                      seed)))
                 ((START)
                  (element-parser
                    (xml-token-head token)
                    port
                    elems
                    entities
                    namespaces
                    #f
                    (,(get-handler 'DECL-ROOT) (xml-token-head token) seed)))
                 (else
                  (parser-error port "XML [22], unexpected markup " token))))))
         (define element-parser
           (ssax:make-elem-parser
             ,(get-handler 'NEW-LEVEL-SEED)
             ,(get-handler 'FINISH-ELEMENT)
             ,(get-handler 'CHAR-DATA-HANDLER)
             ,(get-handler 'PI)))
         (scan-for-significant-prolog-token-1 port seed)))))
