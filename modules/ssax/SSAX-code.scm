
;;(include "../libs/gambit/common.sch")
;;(include "../libs/gambit/myenv.sch")
;;(include "../libs/input-parse.sch")
;;(include "../ssax/SSAX-code.sch")

(define (make-xml-token kind head) (cons kind head))
(define xml-token? pair?)
(define (string-whitespace? str)
  (let ((len (string-length str)))
    (cond
     ((zero? len) #t)
     ((= 1 len) (char-whitespace? (string-ref str 0)))
     ((= 2 len)
      (and (char-whitespace? (string-ref str 0))
           (char-whitespace? (string-ref str 1))))
     (else
      (let loop ((i 0))
        (or (>= i len)
            (and (char-whitespace? (string-ref str i)) (loop (++ i)))))))))
(define (assq-values val alist)
  (let loop ((alist alist) (scanned '()))
    (cond
     ((null? alist) (values #f scanned))
     ((equal? val (caar alist))
      (values (car alist) (append scanned (cdr alist))))
     (else (loop (cdr alist) (cons (car alist) scanned))))))
(define (fold-right kons knil lis1)
  (let recur ((lis lis1))
    (if (null? lis)
      knil
      (let ((head (car lis))) (kons head (recur (cdr lis)))))))
(define (fold kons knil lis1)
  (let lp ((lis lis1) (ans knil))
    (if (null? lis) ans (lp (cdr lis) (kons (car lis) ans)))))
(define ssax:S-chars (map ascii->char '(32 10 9 13)))
(define (ssax:skip-S port) (skip-while ssax:S-chars port))
(define (ssax:ncname-starting-char? a-char)
  (and (char? a-char) (or (char-alphabetic? a-char) (char=? #\_ a-char))))
(define (ssax:read-NCName port)
  (let ((first-char (peek-char port)))
    (or (ssax:ncname-starting-char? first-char)
        (parser-error port "XMLNS [4] for '" first-char "'")))
  (string->symbol
    (next-token-of
      (lambda (c)
        (cond
         ((eof-object? c) #f)
         ((char-alphabetic? c) c)
         ((string-index "0123456789.-_" c) c)
         (else #f)))
      port)))
(define (ssax:read-QName port)
  (let ((prefix-or-localpart (ssax:read-NCName port)))
    (case (peek-char port)
      ((#\:)
       (read-char port)
       (cons prefix-or-localpart (ssax:read-NCName port)))
      (else prefix-or-localpart))))
(define ssax:Prefix-XML (string->symbol "xml"))
(define name-compare
  (letrec ((symbol-compare
             (lambda (symb1 symb2)
               (cond
                ((eq? symb1 symb2) '=)
                ((string<? (symbol->string symb1) (symbol->string symb2)) '<)
                (else '>)))))
    (lambda (name1 name2)
      (cond
       ((symbol? name1) (if (symbol? name2) (symbol-compare name1 name2) '<))
       ((symbol? name2) '>)
       ((eq? name2 ssax:largest-unres-name) '<)
       ((eq? name1 ssax:largest-unres-name) '>)
       ((eq? (car name1) (car name2)) (symbol-compare (cdr name1) (cdr name2)))
       (else (symbol-compare (car name1) (car name2)))))))
(define ssax:largest-unres-name
  (cons (string->symbol "#LARGEST-SYMBOL") (string->symbol "#LARGEST-SYMBOL")))
(define ssax:read-markup-token
  (let ()
    (define (skip-comment port)
      (assert-curr-char '(#\-) "XML [15], second dash" port)
      (if (not (find-string-from-port? "-->" port))
        (parser-error port "XML [15], no -->"))
      (make-xml-token 'COMMENT #f))
    (define (read-cdata port)
      (assert (string=? "CDATA[" (read-string 6 port)))
      (make-xml-token 'CDSECT #f))
    (lambda (port)
      (assert-curr-char '(#\<) "start of the token" port)
      (case (peek-char port)
        ((#\/)
         (read-char port)
         (begin0
           (make-xml-token 'END (ssax:read-QName port))
           (ssax:skip-S port)
           (assert-curr-char '(#\>) "XML [42]" port)))
        ((#\?) (read-char port) (make-xml-token 'PI (ssax:read-NCName port)))
        ((#\!)
         (case (peek-next-char port)
           ((#\-) (read-char port) (skip-comment port))
           ((#\[) (read-char port) (read-cdata port))
           (else (make-xml-token 'DECL (ssax:read-NCName port)))))
        (else (make-xml-token 'START (ssax:read-QName port)))))))
(define (ssax:skip-pi port)
  (if (not (find-string-from-port? "?>" port))
    (parser-error port "Failed to find ?> terminating the PI")))
(define (ssax:read-pi-body-as-string port)
  (ssax:skip-S port)
  (string-concatenate/shared
    (let loop ()
      (let ((pi-fragment (next-token '() '(#\?) "reading PI content" port)))
        (if (eqv? #\> (peek-next-char port))
          (begin (read-char port) (cons pi-fragment '()))
          (cons* pi-fragment "?" (loop)))))))
(define (ssax:skip-internal-dtd port)
  (if (not (find-string-from-port? "]>" port))
    (parser-error
      port
      "Failed to find ]> terminating the internal DTD subset")))
(define ssax:read-cdata-body
  (let ((cdata-delimiters (list char-return #\newline #\] #\&)))
    (lambda (port str-handler seed)
      (let loop ((seed seed))
        (let ((fragment
                (next-token '() cdata-delimiters "reading CDATA" port)))
          (case (read-char port)
            ((#\newline) (loop (str-handler fragment nl seed)))
            ((#\])
             (if (not (eqv? (peek-char port) #\]))
               (loop (str-handler fragment "]" seed))
               (let check-after-second-braket ((seed
                                                (if (string-null? fragment)
                                                  seed
                                                  (str-handler
                                                    fragment
                                                    ""
                                                    seed))))
                 (case (peek-next-char port)
                   ((#\>) (read-char port) seed)
                   ((#\])
                    (check-after-second-braket (str-handler "]" "" seed)))
                   (else (loop (str-handler "]]" "" seed)))))))
            ((#\&)
             (let ((ent-ref
                     (next-token-of
                       (lambda (c)
                         (and (not (eof-object? c)) (char-alphabetic? c) c))
                       port)))
               (cond
                ((and (string=? "gt" ent-ref) (eqv? (peek-char port) #\;))
                 (read-char port)
                 (loop (str-handler fragment ">" seed)))
                (else
                 (loop
                  (str-handler ent-ref "" (str-handler fragment "&" seed)))))))
            (else
             (if (eqv? (peek-char port) #\newline) (read-char port))
             (loop (str-handler fragment nl seed)))))))))
(define (ssax:read-char-ref port)
  (let* ((base
          (cond ((eqv? (peek-char port) #\x) (read-char port) 16) (else 10)))
         (name (next-token '() '(#\;) "XML [66]" port))
         (char-code (string->number name base)))
    (read-char port)
    (if (integer? char-code)
      (ucscode->char char-code)
      (parser-error port "[wf-Legalchar] broken for '" name "'"))))
(define ssax:predefined-parsed-entities
  `((,(string->symbol "amp") . "&")
    (,(string->symbol "lt") . "<")
    (,(string->symbol "gt") . ">")
    (,(string->symbol "apos") . "'")
    (,(string->symbol "quot") . "\"")))
(define (ssax:handle-parsed-entity
         port
         name
         entities
         content-handler
         str-handler
         seed)
  (cond
   ((assq name entities)
    =>
    (lambda (decl-entity)
      (let ((ent-body (cdr decl-entity))
            (new-entities (cons (cons name #f) entities)))
        (cond
         ((string? ent-body)
          (call-with-input-string
            ent-body
            (lambda (port) (content-handler port new-entities seed))))
         ((procedure? ent-body)
          (let ((port (ent-body)))
            (begin0
              (content-handler port new-entities seed)
              (close-input-port port))))
         (else (parser-error port "[norecursion] broken for " name))))))
   ((assq name ssax:predefined-parsed-entities)
    =>
    (lambda (decl-entity) (str-handler (cdr decl-entity) "" seed)))
   (else (parser-error port "[wf-entdeclared] broken for " name))))
(define (make-empty-attlist) '())
(define (attlist-add attlist name-value)
  (if (null? attlist)
    (cons name-value attlist)
    (case (name-compare (car name-value) (caar attlist))
      ((=) #f)
      ((<) (cons name-value attlist))
      (else (cons (car attlist) (attlist-add (cdr attlist) name-value))))))
(define attlist-null? null?)
(define (attlist-remove-top attlist) (values (car attlist) (cdr attlist)))
(define (attlist->alist attlist) attlist)
(define attlist-fold fold)
(define ssax:read-attributes
  (let ((value-delimeters (append ssax:S-chars '(#\< #\&))))
    (define (read-attrib-value delimiter port entities prev-fragments)
      (let* ((new-fragments
               (cons
                (next-token
                  '()
                  (cons delimiter value-delimeters)
                  "XML [10]"
                  port)
                prev-fragments))
             (cterm (read-char port)))
        (cond
         ((or (eof-object? cterm) (eqv? cterm delimiter)) new-fragments)
         ((eqv? cterm char-return)
          (if (eqv? (peek-char port) #\newline) (read-char port))
          (read-attrib-value delimiter port entities (cons " " new-fragments)))
         ((memv cterm ssax:S-chars)
          (read-attrib-value delimiter port entities (cons " " new-fragments)))
         ((eqv? cterm #\&)
          (cond
           ((eqv? (peek-char port) #\#)
            (read-char port)
            (read-attrib-value
              delimiter
              port
              entities
              (cons (string (ssax:read-char-ref port)) new-fragments)))
           (else
            (read-attrib-value
              delimiter
              port
              entities
              (read-named-entity port entities new-fragments)))))
         (else (parser-error port "[CleanAttrVals] broken")))))
    (define (read-named-entity port entities fragments)
      (let ((name (ssax:read-NCName port)))
        (assert-curr-char '(#\;) "XML [68]" port)
        (ssax:handle-parsed-entity
          port
          name
          entities
          (lambda (port entities fragments)
            (read-attrib-value '*eof* port entities fragments))
          (lambda (str1 str2 fragments)
            (if (equal? "" str2)
              (cons str1 fragments)
              (cons* str2 str1 fragments)))
          fragments)))
    (lambda (port entities)
      (let loop ((attr-list (make-empty-attlist)))
        (if (not (ssax:ncname-starting-char? (ssax:skip-S port)))
          attr-list
          (let ((name (ssax:read-QName port)))
            (ssax:skip-S port)
            (assert-curr-char '(#\=) "XML [25]" port)
            (ssax:skip-S port)
            (let ((delimiter (assert-curr-char '(#\' #\") "XML [10]" port)))
              (loop
               (or (attlist-add
                     attr-list
                     (cons
                      name
                      (string-concatenate-reverse/shared
                        (read-attrib-value delimiter port entities '()))))
                   (parser-error
                     port
                     "[uniqattspec] broken for "
                     name))))))))))
(define (ssax:resolve-name port unres-name namespaces apply-default-ns?)
  (cond
   ((pair? unres-name)
    (cons
     (cond
      ((assq (car unres-name) namespaces) => cadr)
      ((eq? (car unres-name) ssax:Prefix-XML) ssax:Prefix-XML)
      (else
       (parser-error
         port
         "[nsc-NSDeclared] broken; prefix "
         (car unres-name))))
     (cdr unres-name)))
   (apply-default-ns?
    (let ((default-ns (assq '*DEFAULT* namespaces)))
      (if (and default-ns (cadr default-ns))
        (cons (cadr default-ns) unres-name)
        unres-name)))
   (else unres-name)))
(define (ssax:uri-string->symbol uri-str) (string->symbol uri-str))
(define ssax:complete-start-tag
  (let ((xmlns (string->symbol "xmlns"))
        (largest-dummy-decl-attr (list ssax:largest-unres-name #f #f #f)))
    (define (validate-attrs port attlist decl-attrs)
      (define (add-default-decl decl-attr result)
        (call-with-values
         (lambda () (apply values decl-attr))
         (lambda (attr-name content-type use-type default-value)
           (and (eq? use-type 'REQUIRED)
                (parser-error port "[RequiredAttr] broken for" attr-name))
           (if default-value
               (cons (cons attr-name default-value) result)
               result))))
      (let loop ((attlist attlist) (decl-attrs decl-attrs) (result '()))
        (if (attlist-null? attlist)
          (attlist-fold add-default-decl result decl-attrs)
          (call-with-values
           (lambda () (attlist-remove-top attlist))
           (lambda (attr attr-others)
             (call-with-values
              (lambda ()
                (if (attlist-null? decl-attrs)
                    (values largest-dummy-decl-attr decl-attrs)
                    (attlist-remove-top decl-attrs)))
              (lambda (decl-attr other-decls)
                (case (name-compare (car attr) (car decl-attr))
                  ((<)
                   (if (or (eq? xmlns (car attr))
                           (and (pair? (car attr)) (eq? xmlns (caar attr))))
                       (loop attr-others decl-attrs (cons attr result))
                       (parser-error port "[ValueType] broken for " attr)))
                  ((>)
                   (loop attlist other-decls (add-default-decl decl-attr result)))
                  (else
                   (call-with-values
                    (lambda () (apply values decl-attr))
                    (lambda (attr-name content-type use-type default-value)
                      (cond
                        ((eq? use-type 'FIXED)
                         (or (equal? (cdr attr) default-value)
                             (parser-error
                              port
                              "[FixedAttr] broken for "
                              attr-name)))
                        ((eq? content-type 'CDATA) #t)
                        ((pair? content-type)
                         (or (member (cdr attr) content-type)
                             (parser-error
                              port
                              "[enum] broken for "
                              attr-name
                              "="
                              (cdr attr))))
                        (else
                         (ssax:warn
                          port
                          "declared content type "
                          content-type
                          " not verified yet")))
                      (loop attr-others other-decls
                            (cons attr result)))))))))))))
    (define (add-ns port prefix uri-str namespaces)
      (and (equal? "" uri-str)
           (parser-error port "[dt-NSName] broken for " prefix))
      (let ((uri-symbol (ssax:uri-string->symbol uri-str)))
        (let loop ((nss namespaces))
          (cond
           ((null? nss) (cons (cons* prefix uri-symbol uri-symbol) namespaces))
           ((eq? uri-symbol (cddar nss))
            (cons (cons* prefix (cadar nss) uri-symbol) namespaces))
           (else (loop (cdr nss)))))))
    (define (adjust-namespace-decl port attrs namespaces)
      (let loop ((attrs attrs) (proper-attrs '()) (namespaces namespaces))
        (cond
         ((null? attrs) (values proper-attrs namespaces))
         ((eq? xmlns (caar attrs))
          (loop
           (cdr attrs)
           proper-attrs
           (if (equal? "" (cdar attrs))
             (cons (cons* '*DEFAULT* #f #f) namespaces)
             (add-ns port '*DEFAULT* (cdar attrs) namespaces))))
         ((and (pair? (caar attrs)) (eq? xmlns (caaar attrs)))
          (loop
           (cdr attrs)
           proper-attrs
           (add-ns port (cdaar attrs) (cdar attrs) namespaces)))
         (else
          (loop (cdr attrs) (cons (car attrs) proper-attrs) namespaces)))))
    (lambda (tag-head port elems entities namespaces)
      (let* ((attlist (ssax:read-attributes port entities))
             (empty-el-tag?
              (begin
                (ssax:skip-S port)
                (and (eqv?
                      #\/
                      (assert-curr-char
                       '(#\> #\/)
                       "XML [40], XML [44], no '>'"
                       port))
                     (assert-curr-char
                      '(#\>)
                      "XML [44], no '>'"
                      port)))))
        (call-with-values
         (lambda () (if elems
                        (cond
                          ((assoc tag-head elems)
                           =>
                           (lambda (decl-elem)
                             (values
                              (if empty-el-tag? 'EMPTY-TAG (cadr decl-elem))
                              (caddr decl-elem))))
                          (else
                           (parser-error
                            port
                            "[elementvalid] broken, no decl for "
                            tag-head)))
                        (values (if empty-el-tag? 'EMPTY-TAG 'ANY) #f)))
         (lambda (elem-content decl-attrs)
           (let ((merged-attrs
                  (if decl-attrs
                      (validate-attrs port attlist decl-attrs)
                      (attlist->alist attlist))))
             (call-with-values
              (lambda ()
                (adjust-namespace-decl port merged-attrs namespaces))
              (lambda (proper-attrs namespaces)
                (values
                 (ssax:resolve-name port tag-head namespaces #t)
                 (fold-right
                  (lambda (name-value attlist)
                    (or (attlist-add
                         attlist
                         (cons
                          (ssax:resolve-name port (car name-value)
                                             namespaces #f)
                          (cdr name-value)))
                        (parser-error
                         port
                         "[uniqattspec] after NS expansion broken for "
                         name-value)))
                  (make-empty-attlist)
                  proper-attrs)
                 namespaces
                 elem-content))))))))))
(define (ssax:read-external-id port)
  (let ((discriminator (ssax:read-NCName port)))
    (assert-curr-char ssax:S-chars "space after SYSTEM or PUBLIC" port)
    (ssax:skip-S port)
    (let ((delimiter (assert-curr-char '(#\' #\") "XML [11], XML [12]" port)))
      (cond
       ((eq? discriminator (string->symbol "SYSTEM"))
        (begin0
          (next-token '() (list delimiter) "XML [11]" port)
          (read-char port)))
       ((eq? discriminator (string->symbol "PUBLIC"))
        (skip-until (list delimiter) port)
        (assert-curr-char ssax:S-chars "space after PubidLiteral" port)
        (ssax:skip-S port)
        (let* ((delimiter (assert-curr-char '(#\' #\") "XML [11]" port))
               (systemid (next-token '() (list delimiter) "XML [11]" port)))
          (read-char port)
          systemid))
       (else
        (parser-error
          port
          "XML [75], "
          discriminator
          " rather than SYSTEM or PUBLIC"))))))
(define (ssax:scan-Misc port)
  (let loop ((c (ssax:skip-S port)))
    (cond
     ((eof-object? c) c)
     ((not (char=? c #\<))
      (parser-error port "XML [22], char '" c "' unexpected"))
     (else
      (let ((token (ssax:read-markup-token port)))
        (case (xml-token-kind token)
          ((COMMENT) (loop (ssax:skip-S port)))
          ((PI DECL START) token)
          (else
           (parser-error
             port
             "XML [22], unexpected token of kind "
             (xml-token-kind token)))))))))
(define ssax:read-char-data
  (let ((terminators-usual (list #\< #\& char-return))
        (terminators-usual-eof (list #\< '*eof* #\& char-return))
        (handle-fragment
          (lambda (fragment str-handler seed)
            (if (string-null? fragment) seed (str-handler fragment "" seed)))))
    (lambda (port expect-eof? str-handler seed)
      (if (eqv? #\< (peek-char port))
        (let ((token (ssax:read-markup-token port)))
          (case (xml-token-kind token)
            ((START END) (values seed token))
            ((CDSECT)
             (let ((seed (ssax:read-cdata-body port str-handler seed)))
               (ssax:read-char-data port expect-eof? str-handler seed)))
            ((COMMENT) (ssax:read-char-data port expect-eof? str-handler seed))
            (else (values seed token))))
        (let ((char-data-terminators
                (if expect-eof? terminators-usual-eof terminators-usual)))
          (let loop ((seed seed))
            (let* ((fragment
                     (next-token
                       '()
                       char-data-terminators
                       "reading char data"
                       port))
                   (term-char (peek-char port)))
              (if (eof-object? term-char)
                (values (handle-fragment fragment str-handler seed) term-char)
                (case term-char
                  ((#\<)
                   (let ((token (ssax:read-markup-token port)))
                     (case (xml-token-kind token)
                       ((CDSECT)
                        (loop
                         (ssax:read-cdata-body
                           port
                           str-handler
                           (handle-fragment fragment str-handler seed))))
                       ((COMMENT)
                        (loop (handle-fragment fragment str-handler seed)))
                       (else
                        (values
                          (handle-fragment fragment str-handler seed)
                          token)))))
                  ((#\&)
                   (case (peek-next-char port)
                     ((#\#)
                      (read-char port)
                      (loop
                       (str-handler
                         fragment
                         (string (ssax:read-char-ref port))
                         seed)))
                     (else
                      (let ((name (ssax:read-NCName port)))
                        (assert-curr-char '(#\;) "XML [68]" port)
                        (values
                          (handle-fragment fragment str-handler seed)
                          (make-xml-token 'ENTITY-REF name))))))
                  (else
                   (if (eqv? (peek-next-char port) #\newline) (read-char port))
                   (loop
                    (str-handler fragment (string #\newline) seed))))))))))))
(define (ssax:assert-token token kind gi error-cont)
  (or (and (xml-token? token)
           (eq? kind (xml-token-kind token))
           (equal? gi (xml-token-head token)))
      (error-cont token kind gi)))
(define (ssax:reverse-collect-str fragments)
  (cond
   ((null? fragments) '())
   ((null? (cdr fragments)) fragments)
   (else
    (let loop ((fragments fragments) (result '()) (strs '()))
      (cond
       ((null? fragments)
        (if (null? strs)
          result
          (cons (string-concatenate/shared strs) result)))
       ((string? (car fragments))
        (loop (cdr fragments) result (cons (car fragments) strs)))
       (else
        (loop
         (cdr fragments)
         (cons
          (car fragments)
          (if (null? strs)
            result
            (cons (string-concatenate/shared strs) result)))
         '())))))))
(define (ssax:reverse-collect-str-drop-ws fragments)
  (cond
   ((null? fragments) '())
   ((null? (cdr fragments))
    (if (and (string? (car fragments)) (string-whitespace? (car fragments)))
      '()
      fragments))
   (else
    (let loop ((fragments fragments)
               (result '())
               (strs '())
               (all-whitespace? #t))
      (cond
       ((null? fragments)
        (if all-whitespace?
          result
          (cons (string-concatenate/shared strs) result)))
       ((string? (car fragments))
        (loop
         (cdr fragments)
         result
         (cons (car fragments) strs)
         (and all-whitespace? (string-whitespace? (car fragments)))))
       (else
        (loop
         (cdr fragments)
         (cons
          (car fragments)
          (if all-whitespace?
            result
            (cons (string-concatenate/shared strs) result)))
         '()
         #t)))))))

; procedure: ssax:xml->sxml PORT NAMESPACE-PREFIX-ASSIG
;
; This is an instance of a SSAX parser that returns an SXML
; representation of the XML document to be read from PORT.
; NAMESPACE-PREFIX-ASSIG is a list of (USER-PREFIX . URI-STRING)
; that assigns USER-PREFIXes to certain namespaces identified by
; particular URI-STRINGs. It may be an empty list.
; The procedure returns an SXML tree. The port points out to the
; first character after the root element.
(define (ssax:xml->sxml port namespace-prefix-assig)
  (letrec ((namespaces
             (map
              (lambda (el)
                (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
              namespace-prefix-assig))
           (RES-NAME->SXML
             (lambda (res-name)
               (string->symbol
                 (string-append
                   (symbol->string (car res-name))
                   ":"
                   (symbol->string (cdr res-name)))))))
    (let ((result
            (reverse
              ((ssax:make-parser
                 NEW-LEVEL-SEED
                 (lambda (elem-gi attributes namespaces expected-content seed)
                   '())
                 FINISH-ELEMENT
                 (lambda (elem-gi attributes namespaces parent-seed seed)
                   (let ((seed (ssax:reverse-collect-str-drop-ws seed))
                         (attrs
                          (attlist-fold
                            (lambda (attr accum)
                              (cons
                               (list
                                (if (symbol? (car attr))
                                  (car attr)
                                  (RES-NAME->SXML (car attr)))
                                (cdr attr))
                               accum))
                            '()
                            attributes)))
                     (cons
                      (cons
                       (if (symbol? elem-gi) elem-gi (RES-NAME->SXML elem-gi))
                       (if (null? attrs) seed (cons (cons '@ attrs) seed)))
                      parent-seed)))
                 CHAR-DATA-HANDLER
                 (lambda (string1 string2 seed)
                   (if (string-null? string2)
                     (cons string1 seed)
                     (cons* string2 string1 seed)))
                 DOCTYPE
                 (lambda (port docname systemid internal-subset? seed)
                   (when internal-subset?
                     (ssax:warn
                       port
                       "Internal DTD subset is not currently handled ")
                     (ssax:skip-internal-dtd port))
                   (ssax:warn
                     port
                     "DOCTYPE DECL "
                     docname
                     " "
                     systemid
                     " found and skipped")
                   (values #f '() namespaces seed))
                 UNDECL-ROOT
                 (lambda (elem-gi seed) (values #f '() namespaces seed))
                 PI
                 ((*DEFAULT*
                    lambda
                    (port pi-tag seed)
                    (cons
                     (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
                     seed))))
               port
               '()))))
      (cons
       '*TOP*
       (if (null? namespace-prefix-assig)
         result
         (cons
          (list
           '@@
           (cons
            '*NAMESPACES*
            (map
             (lambda (ns) (list (car ns) (cdr ns)))
             namespace-prefix-assig)))
          result))))))
(define SSAX:XML->SXML ssax:xml->sxml)
