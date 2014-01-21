
;;(include "../sxml-tools/sxml-tools.sch")
;;(include "../html-prag/htmlprag.sch")
;;(include "../multi-parser/id/srfi-12.sch")
;;(include "../multi-parser/id/http.sch")
;;(include "../libs/input-parse.sch")
;;(include "../libs/gambit/myenv.sch")
;;(include "../libs/gambit/common.sch")
;;(include "../ssax/SSAX-code.sch")

;; XLink implementation and the API for XLink processing in Scheme
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; doc ::= '(*TOP*
;           (@@
;            (sxlink
;             (declared-here  <sxlink-arc>* )
;             (embedded)?
;             (outgoing
;              (node  <sxlink-arc>+ )*
;             )
;            )
;            ...   ; more aux list members
;           )
;          ...)

;==========================================================================
; XLink-related node tests
; They test whether an SXML node has a definite XLink type
; ATTENTION:
;  1. A non-prefixed XLink namespace uri is used for these node tests. If
; a prefix is used, these functions behave incorrectly.
;  2. These predicates should be used carefully - element's XLink-related
; meaning depends not only on its xlink:type attribute, but also on its
; position among other XLink element. For example, an element with an
; xlink:type="arc" attribute is not an arc element if it has anything other
; then an extended-link element as a parent

; Helper for predicates
;  type - a string, is supposed to have one of the following values:
; "extended", "simple", "locator", "resource", "arc", "title".
; A lambda is returned. When applied to an SXML node, it determines
; whether the node's xlink:type attribute has a 'type' value.
(define (xlink:ntype?? type)
  (lambda (node)
    (let ((attval
           ((select-kids (ntype?? '*text*))
            ((select-kids (ntype?? 'http://www.w3.org/1999/xlink:type))
             ((select-kids (ntype?? '@)) node)))))
      (if (null? attval)  ; there is no xlink:type attribute
          #f
          (string=? (car attval) type)))))

; Node tests for different XLink elements
(define xlink:elem-extended? (xlink:ntype?? "extended"))
(define xlink:elem-simple? (xlink:ntype?? "simple"))
(define xlink:elem-locator? (xlink:ntype?? "locator"))
(define xlink:elem-resource? (xlink:ntype?? "resource"))
(define xlink:elem-arc? (xlink:ntype?? "arc"))
(define xlink:elem-title? (xlink:ntype?? "title"))


;==========================================================================
; Utility functions over document auxiliary information

;-------------------------------------------------
; Document's URI
; The following functions moved to "xlink-parser.scm"
;  xlink:get-uri
;  xlink:set-uri-for-sxlink-arcs

; Sets the URI for the SXML document
(define (xlink:set-uri uri doc)
  (let ((aux-nset ((select-kids (ntype?? '@@)) doc)))
    (if
     (or (null? aux-nset)  ; no aux node at all yet
         ; no sxlink/declared-here subnode
         (null? ((select-kids (ntype?? 'declared-here))
                 ((select-kids (ntype?? 'sxlink)) (car aux-nset)))))
     (xlink:replace-branch  ; inserts the @@/uri node in the document
      doc '(@@ uri) (list uri))
     (xlink:replace-branch
      doc
      '(@@)
      (cdr
       ((xlink:branch-helper  ; inserts URI to sxlink-arcs
         (lambda (declared-here-node dummy)
           (cons
            (car declared-here-node)
            (xlink:set-uri-for-sxlink-arcs
             uri (cdr declared-here-node)))))
        (xlink:replace-branch  ; inserts (modified) URI
         (car aux-nset) '(uri) (list uri))
        '(sxlink declared-here)
        '()  ; dummy
        ))))))

;-------------------------------------------------
; Id-index of the document

; Returns the id-index of the SXML document
; #f is returned is there is no "@@/id-index" subtree in the document
(define (xlink:id-index doc)
  (let ((nodeset ((select-kids (ntype?? 'id-index))
                  ((select-kids (ntype?? '@@)) doc))))
    (if (null? nodeset)  ; there is no "@@/id-index" subtree
        #f
        (cdar nodeset))))

;-------------------------------------------------
; SXLink members of the auxiliary list

; Returns (listof sxlink-arc) located in "@@/sxlink/declared-here"
; These are SXLink arcs that are declared in this document
(define (xlink:arcs-declared-here doc)
  ((select-kids (ntype?? '*any*))
   ((select-kids (ntype?? 'declared-here))
    ((select-kids (ntype?? 'sxlink))
     ((select-kids (ntype?? '@@)) doc)))))

; Whether outgoing SXLink arcs are embedded into the document.
; This is denoted by the presense of "@@/sxlink/embedded" empty element.
(define (xlink:arcs-embedded? doc)
  (not (null? ((select-kids (ntype?? 'embedded))
               ((select-kids (ntype?? 'sxlink))
                ((select-kids (ntype?? '@@)) doc))))))

; Returns the content of "@@/sxlink/outgoing"
; The result is the associative list between nodes of the document and
; SXLink arcs that start from the corresponding node
(define (xlink:arcs-outgoing doc)
  ((select-kids (ntype?? '*any*))
   ((select-kids (ntype?? 'outgoing))
    ((select-kids (ntype?? 'sxlink))
     ((select-kids (ntype?? '@@)) doc)))))


;==========================================================================
; Get the document by its URI

; Handler for error messages
(define (xlink:api-error . text)
  (cerr "XLink API error: ")
  (apply cerr text)
  (cerr nl))

; Id+XLink parser parameterized
(define xlink:parser (ssax:multi-parser 'id 'xlink))

; Returns the SXML representation for the resource specified by REQ-URI.
; Resource types supported: XML and HTML. XML is parsed into SXML with SSAX,
; HTML is parsed with HTML Prag.
; Additionally, linking information is parsed. For XML, linking information is
; assumed to be specified with XLink. For HTML, <a> elements are treated as
; simple links.
; In case of an error (resource doesn't exist or its type is unsupported), an
; error is signalled with 'xlink:api-error' and #f is returned.
(define (xlink:get-document-by-uri req-uri)
  (case (ar:resource-type req-uri)
    ((#f)  ; resource doesn't exist
     (xlink:api-error "resource doesn't exist: " req-uri)
     #f)
    ((xml plain unknown)
     (let* ((port (open-input-resource req-uri))
            (doc (xlink:parser port)))
       (close-input-port port)
       (xlink:set-uri req-uri doc)))
    ((html)
     (let* ((port (open-input-resource req-uri))
            (doc (html->sxml port)))
       (close-input-port port)
       (SHTML->SHTML+xlink
        (xlink:set-uri req-uri doc))))    
    (else  ; unknown resource type
     (xlink:api-error "resource type not supported: " req-uri)
     #f)))


;==========================================================================
; Loading multiple documents by their URIs

;-------------------------------------------------
; Helper accessors to SXLink arcs

; Returns URIs of resources that participate in SXLink arcs
;  sxlink-arcs ::= (listof sxlink-arc)
; Result: (listof string)
; The result may contain duplicates
(define (xlink:arcs-uris sxlink-arcs)
  ((select-kids (ntype?? '*text*))
   ((select-kids (ntype?? 'uri))
    ((select-kids (ntype-names?? '(from to))) sxlink-arcs))))

; Returns URIs of all linkbases encountered among SXLink arcs
; Result: (listof string)
; The result may contain duplicates
(define (xlink:arcs-linkbase-uris sxlink-arcs)
  ((select-kids (ntype?? '*text*))
   ((select-kids (ntype?? 'uri))
    ((select-kids (ntype?? 'to))
     (filter (ntype?? 'linkbase) sxlink-arcs)))))

;-------------------------------------------------
; Working on the set of SXML documents
;  doc-set ::= (listof document)

; Returns the list of URIs of the documents in the doc-set
(define (xlink:uris doc-set)
  (filter
   (lambda (x) x)
   (map xlink:get-uri doc-set)))

; Removes equal duplicates from the list
(define (xlink:remove-equal-duplicates lst)
  (cond
    ((null? lst) lst)
    ((member (car lst) (cdr lst))
     (xlink:remove-equal-duplicates (cdr lst)))
    (else
     (cons (car lst) (xlink:remove-equal-duplicates (cdr lst))))))

; procedure xlink:find-doc :: URI-STRING (listof SXML-TREE) -> SXML-TREE
;
; Finding a document in 'doc-set' by its 'uri-string'.
; If there is no such document, #f is returned.
;  doc-set ::= (listof SXML-TREE)
(define (xlink:find-doc uri-string doc-set)
  (let loop ((doc-set doc-set))
    (cond
      ((null? doc-set) #f)
      ((equal? (xlink:get-uri (car doc-set)) uri-string)
       (car doc-set))
      (else (loop (cdr doc-set))))))

;-------------------------------------------------
; Extending the set of documents with additional documents being referred to

; Returns a list of URIs which are refered by XLink markup
; Result:  (listof string)
; The list may contain duplicates.
(define (xlink:referenced-uris doc-set)
  (apply append
         (map
          (lambda (doc)
            (xlink:arcs-uris (xlink:arcs-declared-here doc)))
          doc-set)))

; Returns a list of linkbase URIs which are refered by XLink markup
; Result:  (listof string)
; The list may contain duplicates.
(define (xlink:referenced-linkbase-uris doc-set)
  (apply append
         (map
          (lambda (doc)
            (xlink:arcs-linkbase-uris (xlink:arcs-declared-here doc)))
          doc-set)))

; A helped low-level function for extending the doc-set with more documents
; Is parameterized with
;  referenced-uris ::= (lambda (doc-set) ...)
; that would return URIs refered by XLink markup in the doc-set
; When parameterized, returns
;  (lambda (doc-set . max-steps) ...)
;  max-steps - maximal number of recursive steps
;  The lambda returns the expanded doc-set
(define (xlink:add-documents-helper referenced-uris)
  (lambda (doc-set . max-steps)
    (let ((max-steps (if (null? max-steps) -1 (car max-steps))))
      (let loop ((doc-set doc-set)
                 (loaded-uris (xlink:uris doc-set))
                 (to-load (referenced-uris doc-set))
                 (step 0))
        (if
         (or (null? to-load) (= step max-steps))
         doc-set
         (let rpt ((loaded-uris loaded-uris)
                   (to-load to-load)
                   (added-docs '()))
           (cond
             ((null? to-load)
              (loop (append added-docs doc-set)
                    loaded-uris
                    (referenced-uris added-docs)
                    (+ step 1)))
             ((member (car to-load) loaded-uris)
              (rpt loaded-uris
                   (cdr to-load)
                   added-docs))
             (else   ; we load the linkbase
              (let ((doc (xlink:get-document-by-uri (car to-load))))
                (rpt (cons (car to-load) loaded-uris)
                     (cdr to-load)
                     (if doc (cons doc added-docs) added-docs)))))))))))

; Two most common parameterized functions. The first one recursively loads
; linkbases. The second one recursively loads all refered documents
(define xlink:add-linkbases-recursively
  (xlink:add-documents-helper xlink:referenced-linkbase-uris))
(define xlink:add-documents-recursively
  (xlink:add-documents-helper xlink:referenced-uris))

;-------------------------------------------------
; Higher-level functions

; Parameterized with options, returns
;  (lambda (uri . uris) ...)
; which is the lambda for getting documents by their URIs
; Options include the following:
;  'linkbases - load linkbases recursively
;  '(linkbases  <number> ) - load linkbases recursively, with the maximal
; number of recursive steps defined by the <number> supplied
;  'docs - load documents recursively
;  '(docs  <number> ) - load documents recursively, with the maximal number
; of recursive steps defined by the <number> supplied
(define (xlink:get-documents-with-params . options)
  (let ((get-initial-docs  ; Returns documents by their URIs
         (lambda (uris)
           (filter  ; keeps only correctly loaded documents
            (lambda (x) x)
            (map xlink:get-document-by-uri
                 (xlink:remove-equal-duplicates uris)))))
        (linkbases-pairs
         (filter
          (lambda (option) (and (pair? option) (eq? (car option) 'linkbases)))
          options))
        (docs-pairs
         (filter
          (lambda (option) (and (pair? option) (eq? (car option) 'docs)))
          options)))
    (let ((linkbases? (or (memq 'linkbases options)
                          (not (null? linkbases-pairs))))
          (max-steps-linkbases (if (null? linkbases-pairs)
                                   -1
                                   (cadar linkbases-pairs)))
          (documents? (or (memq 'docs options)
                          (not (null? docs-pairs))))
          (max-steps-documents (if (null? docs-pairs)
                                   -1
                                   (cadar docs-pairs))))
      (cond
        ((and linkbases? documents?)
         (lambda (uri . uris)
           (xlink:add-linkbases-recursively
            (xlink:add-documents-recursively
             (get-initial-docs (cons uri uris))
             max-steps-documents)
            max-steps-linkbases)))
        (linkbases?
         (lambda (uri . uris)
           (xlink:add-linkbases-recursively            
            (get-initial-docs (cons uri uris))            
            max-steps-linkbases)))
        (documents?
         (lambda (uri . uris)           
           (xlink:add-documents-recursively
            (get-initial-docs (cons uri uris))
            max-steps-documents)))
        (else  ; nothing extra to be loaded
         (lambda (uri . uris) (get-initial-docs (cons uri uris))))))))

; The most common parameterized case.
; Loads documents and all linkbases
(define xlink:get-documents+linkbases
  (xlink:get-documents-with-params 'linkbases))


;==========================================================================
; Working on the set of linked documents
;  linked-docs ::= (listof document)

;  alist ::= (listof
;             (cons key (listof item)))
; For equal keys in the alist, the function unites the corresponding key values
; Returns the new alist
(define (xlink:unite-duplicate-keys-in-alist alist)
  (let loop ((src alist)
             (res '()))
    (if
     (null? src)
     res
     (let ((curr-key (caar src)))
       (let rpt ((scan (cdr src))
                 (content (cdar src))
                 (other '()))
         (cond
           ((null? scan)
            (loop other
                  (cons (cons curr-key content)
                        res)))
           ((equal? (caar scan) curr-key)
            (rpt (cdr scan)
                 (append content (cdar scan))
                 other))
           (else  ; a different key
            (rpt (cdr scan) content
                 (cons (car scan) other)))))))))
                         
; Documents exchange their SXLink arcs, such as each arc is moved to the
; "@@/sxlink/outgoing" branch of the document where the arc's starting
; resource is
; Additional SXLink arcs may be specified in the optional argument.
(define (xlink:docs-exchange-arcs doc-set . sxlink-arcs)
  (let ((doc-set-uris (xlink:uris doc-set))
        (sxlink-arcs (if (null? sxlink-arcs) '() (car sxlink-arcs))))
    ; outgoing-alist ::= (listof
    ;                     (cons uri
    ;                           (listof (cons node (listof sxlink-arc)))))
    ; declared-here-alist ::= (listof
    ;                           (cons uri (listof sxlink-arc)))
    (let loop ((outgoing-alist (map
                                (lambda (doc)
                                  (cons
                                   (xlink:get-uri doc)
                                   (xlink:arcs-outgoing doc)))
                                doc-set))
               (declared-here-alist (map list doc-set-uris))
               (arcs-to-scan
                (append sxlink-arcs
                        (apply append
                               (map xlink:arcs-declared-here doc-set)))))
      (if
       (null? arcs-to-scan)  ; all arcs processed
       (let ((outgoing-alist
              (xlink:unite-duplicate-keys-in-alist outgoing-alist))
             (declared-here-alist
              (xlink:unite-duplicate-keys-in-alist declared-here-alist)))
         (map
          (lambda (doc)
            (let ((uri (xlink:get-uri doc)))
              (xlink:replace-branch               
                doc
                '(@@ sxlink)
                `((declared-here
                   ,@(cdr (assoc uri declared-here-alist)))
                  ,@(if (xlink:arcs-embedded? doc) '((embedded)) '())
                  (outgoing
                   ,@(xlink:unite-duplicate-keys-in-alist
                      (cdr (assoc uri outgoing-alist))))))))
            doc-set))
       (let* ((curr-arc (car arcs-to-scan))
              (uri-from (car  ; URI must be presented
                         ((select-kids (ntype?? '*text*))
                          ((select-kids (ntype?? 'uri))
                           ((select-kids (ntype?? 'from))
                            curr-arc)))))
              (uri-decl (car  ; URI must be presented
                         ((select-kids (ntype?? '*text*))
                          ((select-kids (ntype?? 'uri))
                           ((select-kids (ntype?? 'declaration))
                            curr-arc))))))
         (if
          (not (member uri-from doc-set-uris))
          ; This arc starts from none of the documents from doc-set
          (loop outgoing-alist
                (cons (list uri-decl curr-arc) declared-here-alist)
                (cdr arcs-to-scan))
          (let ((nodes  ; nodes that are the starting resource
                 (let ((nodes-nset
                        ((select-kids (ntype?? 'nodes))
                         ((select-kids (ntype?? 'from))
                          curr-arc))))
                   (if
                    (not (null? nodes-nset))
                    (cdar nodes-nset)
                    (let ((xpointer-nset
                           ((select-kids (ntype?? 'xpointer))
                            ((select-kids (ntype?? 'from)) curr-arc)))
                          (starting-doc (xlink:find-doc uri-from doc-set)))
                      (if
                       (null? xpointer-nset)  ; no XPointer
                       ((select-kids (ntype?? '*))  ; document element
                        starting-doc)
                       (let ((func (sxml:xpointer (cadar xpointer-nset))))
                         (if
                          (not func)  ; parser error
                          #f
                          (let ((starting-nset (func starting-doc)))
                            (if
                             (nodeset? starting-nset)
                             starting-nset
                             #f))))))))))
            (if
              nodes   ; starting resource selects some nodes
              (loop               
               (cons (cons uri-from
                           (map
                            (lambda (node) (list node curr-arc))
                            nodes))
                     outgoing-alist)
               declared-here-alist
               (cdr arcs-to-scan))
              (loop outgoing-alist
                    (cons (list uri-decl curr-arc) declared-here-alist)
                    (cdr arcs-to-scan))))))))))

;-------------------------------------------------
; Embedding XLink arcs into the document
; The element node with embedded XLink arcs looks as follows
;  element-node ::= (name
;                    (@ ...)
;                    (@@
;                     (sxlink  <sxlink-arc>+ )
;                     ...)  ; other members of the aux list
;                    ...)
;  attribute-node ::= (name "value"
;                           (@@
;                            (sxlink  <sxlink-arc>+ )
;                            ...)  ; other members of the aux list
;                          )

; Takes SXLink arcs outgoing from the document and embeds these arcs into
; element and attribute nodes of the document.
; The modified document is returned
; The function doesn't make a copy of nodes that remain unchanged
(define (xlink:embed-arcs-into-document document)
  (letrec
      (; These helper functions return
       ; (values node outgoing-alist changed?)
       ;  node - the (modified) node
       ;  outgoing-alist ::= (listof (cons node (listof sxlink-arc)))
       ;  changed? - whether the node was changed      
       (process-element-node
        (lambda (node outgoing-alist)
          (cond
            ((or (not (pair? node))
                 (eq? (car node) '@@))
             ; Text node or aux node
             (values node outgoing-alist #f))
            ((eq? (car node) '@)
             (call-with-values
              (lambda ()
                ((process-nodeset process-attribute-node)
                 (cdr node) outgoing-alist))
              (lambda (content new-out-alist changed?)
                (if changed?
                    (values (cons '@ content)
                            new-out-alist
                            changed?)
                    (values node outgoing-alist changed?)))))
            (else  ; this is the element node
             (call-with-values
              (lambda ()
                (cond
                  ((assq node outgoing-alist)
                   => (lambda (alist-member)
                        (values
                         (cdr alist-member)
                         (filter
                          (lambda (memb) (not (eq? memb alist-member)))
                          outgoing-alist))))
                  (else  ; the node is not the starting resource
                   (values #f outgoing-alist))))
              (lambda (outgoing-arcs new-out-alist)
                (call-with-values
                 (lambda () ((process-nodeset process-element-node)
                             (cdr node) new-out-alist))
                 (lambda (content new-out-alist changed?)
                   (cond
                     ((not (or outgoing-arcs changed?))
                      ; node remains unchanged                    
                      (values node outgoing-alist changed?))
                     ((not outgoing-arcs)  ; no arcs from that node
                      (values (cons (car node) content)
                              new-out-alist
                              changed?))
                     (else  ; the node is the starting resource
                      (let ((new-content
                             (if changed? content (cdr node))))
                        (values
                         (cond
                           ((not (null?  ; aux list presented
                                  ((select-kids (ntype?? '@@)) new-content)))
                            (xlink:append-branch
                             (cons (car node) new-content)
                             '(@@ sxlink) outgoing-arcs))
                           (((ntype?? '@)  ; attribute node presented                         
                             (car new-content))
                            `(,(car node)
                              ,(car content)  ; attribute node
                              (@@ (sxlink ,@outgoing-arcs))
                              ,@(cdr content)))
                           (else  ; no attribute node
                            `(,(car node)
                              (@)
                              (@@ (sxlink ,@outgoing-arcs))
                              ,@content)))
                         new-out-alist
                         #t))))))))))))
       (process-attribute-node
        (lambda (node outgoing-alist)
          (cond
            ((assq node outgoing-alist)
             => (lambda (alist-member)
                  (values
                   (if
                    (null?  ; no aux node in the attribute
                     ((select-kids (ntype?? '@@)) node))
                    (append node
                            `((@@
                               (sxlink ,@(cdr alist-member)))))
                    (xlink:append-branch
                     node '(@@ sxlink) (cdr alist-member)))
                   (filter
                    (lambda (memb) (not (eq? memb alist-member)))
                    outgoing-alist)
                   #t)))
            (else   ; the attribute node is not a starting resource
             (values node outgoing-alist #f)))))
       ; Is parameterized with one of the previous functions and
       ; processes the nodeset
       (process-nodeset
        (lambda (processing-func)
          (lambda (nodeset outgoing-alist)
            (let loop ((nset nodeset)
                       (out-alist outgoing-alist)
                       (changed? #f)
                       (res '()))
              (if
               (null? nset)  ; nodeset processed
               (values (reverse res)
                       out-alist
                       changed?)
               (call-with-values
                (lambda () (processing-func (car nset) out-alist))
                (lambda (new-node new-out-alist ch?)
                  (loop (cdr nset)
                        new-out-alist
                        (or changed? ch?)
                        (cons new-node res))))))))))
    (call-with-values
     (lambda () ((process-nodeset process-element-node)
                 (cdr document)
                 (xlink:arcs-outgoing document)))
     (lambda (content new-out-alist changed?)
       (if (not changed?)  ; the document remains unchanged
           (xlink:replace-branch
            document '(@@ sxlink embedded) '())
           (xlink:replace-branch
            (cons '*TOP* content)
            '(@@ sxlink)
            `((declared-here ,@(xlink:arcs-declared-here document))
              (embedded)
              (outgoing ,@new-out-alist))))))))

; Returns all embedded SXLink arcs in the document
; Result: (listof sxlink-arc)
(define (xlink:arcs-embedded doc)
  (let ((get-kids
         (select-kids
          (lambda (node) (and (pair? node) (not (eq? '@@ (car node))))))))
    (let loop ((nodes-to-scan (get-kids doc))
               (res '()))
      (if
       (null? nodes-to-scan)  ; everyone processed
       (draft:remove-eq-duplicates res)
       (loop
        (append (get-kids (car nodes-to-scan)) (cdr nodes-to-scan))
        (append
         ((select-kids (ntype?? '*any*))
          ((select-kids (ntype?? 'sxlink))
           ((select-kids (ntype?? '@@)) (car nodes-to-scan))))
         res))))))


;==========================================================================
; Load documents with respect to the other documents

; Parameterized with options, returns
;  (lambda (linked-docs uri . uris) ...)
; which is the lambda for getting more documents by their URIs
; Options include the following:
;  'linkbases - load linkbases recursively
;  '(linkbases  <number> ) - load linkbases recursively, with the maximal
;                            number of recursive steps defined by the <number>
;                            supplied
;  'docs - load documents recursively
;  '(docs  <number> ) - load documents recursively, with the maximal number
;                       of recursive steps defined by the <number> supplied
;  'embed - embed SXLink arcs into nodes that are starting resources for that
;           arcs
;  'no-embed - don't embed SXLink arcs into documents loaded
(define (xlink:parameterized-load-with-respect-documents . options)
  (let ((doc-getter (apply xlink:get-documents-with-params options))
        (embed? (memq 'embed options))
        (no-embed? (memq 'no-embed options)))
    (lambda (linked-docs . uris)
      (let* ((loaded-uris (xlink:uris linked-docs))
             (req-docs
              (xlink:docs-exchange-arcs
               (filter
                (lambda (x) x)
                (map
                 (lambda (uri)
                   (if
                    (member uri loaded-uris)  ; document already loaded
                    (xlink:find-doc uri linked-docs)
                    (xlink:get-document-by-uri uri)))
                 (xlink:remove-equal-duplicates uris)))
               (apply append (map xlink:arcs-declared-here linked-docs)))))
        (cond
          (no-embed? req-docs)
          ((or embed?  ; embed arcs
               (member #t (map xlink:arcs-embedded? linked-docs)))
           (map xlink:embed-arcs-into-document req-docs))
          (else req-docs))))))

; The most common case of parametrization
(define xlink:get-docs-with-respect-to-loaded
  (xlink:parameterized-load-with-respect-documents 'linkbase))


;==========================================================================
; Excluding documents from linked-docs
; TODO: to be implemented later

; Returns all SXLink arcs encountered in the document. This envolves:
;  a) declared here arcs,
;  b) outgoing arcs, and
;  c) embedded arcs
; Returns (listof sxlink-arcs)
;(define (xlink:arcs-all doc)

; Returns linked-docs 
;(define (xlink:exclude-documents linked-docs uri . uris)
  

;==========================================================================
; High-level API functions

; Parameterized with options, returns
;  (lambda (uri . uris) ...)
; which is the lambda for getting documents by their URIs
; Options include the following:
;  'linkbases - load linkbases recursively
;  '(linkbases  <number> ) - load linkbases recursively, with the maximal
;                            number of recursive steps defined by the <number>
;                            supplied
;  'docs - load documents recursively
;  '(docs  <number> ) - load documents recursively, with the maximal number
;                       of recursive steps defined by the <number> supplied
;  'embed - embed SXLink arcs into nodes that are starting resources for that
;           arcs
(define (xlink:load-linked-docs-with-params . options)
  (let ((doc-getter (apply xlink:get-documents-with-params options)))
    (if
     (memq 'embed options)  ; embed
     (lambda (uri . uris)
       (map
        xlink:embed-arcs-into-document
        (xlink:docs-exchange-arcs (apply doc-getter (cons uri uris)))))
     (lambda (uri . uris)
       (xlink:docs-exchange-arcs (apply doc-getter (cons uri uris)))))))

; procedure xlink:documents :: {REQ-URI}+  -> (listof SXML-TREE)
; procedure xlink:documents-embed :: {REQ-URI}+  -> (listof SXML-TREE)
;
; Both `xlink:documents' and `xlink:documents-embed' accept one or more
; strings as their arguments. Each string supplied denotes the URI of the
; requested document to be loaded. The requested document(s) are loaded
; and are represented in SXML. All XLink links declared in these document(s)
; are represented as a set of SXLink arcs. If any XLink links refer to XLink
; linkbases [<a href="http://www.w3.org/TR/xlink/#xlg">XLink</a>],
; these linkbases are additionally loaded, for additional SXLink arcs
; declared there.
;
; The starting resource for each SXLink arc is determined:
; 1. For each SXML document loaded, the function `xlink:document' adds all
;    SXLink arcs whose starting resource is located within this document, to
;    the auxiliary list of its document node (*TOP*).
; 2. The function 'xlink:documents-embed' embeds each SXLink arc into its
;    starting resource-node, via auxiliary list of that node. For text nodes
;    serving for starting resources, their SXLink arcs are stored in the
;    auxiliary list of the document node (*TOP*), since SXML text nodes do
;    not support their own auxiliary lists.
;
; Supported URI formats:
;  + local file
;  + http:// schema
;
; Supported document formats: XML and HTML. In the case of HTML,
; <A> hyperlinks are considered as XLink simple links.
;
; Result: (listof SXML-TREE)
; A particular SXML document can be located in this list using the
; function `xlink:find-doc'.
(define xlink:documents
  (xlink:load-linked-docs-with-params 'linkbases))
(define xlink:documents-embed
  (xlink:load-linked-docs-with-params 'linkbases 'embed))

;-------------------------------------------------
; Convenient function for getting a document by its URI

; procedure sxml:document :: REQ-URI [NAMESPACE-PREFIX-ASSIG] ->
;                             -> SXML-TREE
;
; Obtain a [possibly, remote] document by its URI
; Supported URI formats:  local file and HTTP schema
; Supported document formats:  XML and HTML
;
; REQ-URI - a string that contains the URI of the requested document
; NAMESPACE-PREFIX-ASSIG - is passed as-is to the SSAX parser: there it is
;  used for assigning certain user prefixes to certain namespaces.
;  NAMESPACE-PREFIX-ASSIG is an optional argument and has an effect for an
;  XML resource only. For an HTML resource requested, NAMESPACE-PREFIX-ASSIG
;  is silently ignored.
;
; Result: the SXML representation for the requested document
(define (sxml:document req-uri . namespace-prefix-assig)
  (if
   (string? req-uri)
   (case (ar:resource-type req-uri)
     ((#f)  ; resource doesn't exist
      (xlink:api-error "resource doesn't exist: " req-uri)
      #f)
     ((xml plain unknown)
      (let* ((port (open-input-resource req-uri))
             (doc (ssax:xml->sxml
                   port
                   (if (null? namespace-prefix-assig)
                       namespace-prefix-assig
                       (car namespace-prefix-assig)))))
        (close-input-port port)
        doc   ; DL: can also add URI: (xlink:set-uri req-uri doc)
        ))
     ((html)
      (let* ((port (open-input-resource req-uri))
             (doc (html->sxml port)))
        (close-input-port port)
        doc   ; DL: can also add URI: (xlink:set-uri req-uri doc)
        ))
     (else  ; unknown resource type
      (xlink:api-error "resource type not supported: " req-uri)
      #f))
   ; Otherwise: REQ-URI is not a string - producing an exception
   (exc:signal  ; relies on SRFI-12
    (make-property-condition
     'exn
     'message
     "sxml:document: expects type <string> as 1st argument"))))


;==========================================================================
; SXPath-related stuff

; Whether an SXLink arc
(define xlink:arc?
  (ntype-names??
   '(linkbase simple outbound inbound third-party local-to-local)))

;-------------------------------------------------
; Working with the administrative variable '*docs*

; Returns the value of the administrative SXPath variable '*docs*
; This variable stores linked-docs
(define (xlink:docs-variable var-binding)
  (cond
    ((assq '*docs* var-binding)
     => cdr)
    (else '())))

; Extends var-bindings with administative information about linked-docs
;  node - a single node or a nodeset
(define (xlink:add-docs-to-vars node var-binding)
  (if (assq '*docs* var-binding)  ; variable already exists
      var-binding
      (cons
       (cons '*docs*
             (filter
              (lambda (doc)
                (and (draft:top? doc) (xlink:get-uri doc)))
              (draft:reach-root (as-nodeset node))))
       var-binding)))  
  
;-------------------------------------------------
; Accessors to SXLink arcs that start from the given SXML node

; Returns SXLink arcs that are embedded into the node as aux list members
; Result: (listof sxlink-arc)
(define (xlink:node-embedded-arcs node)
  (if (draft:top? node)  ; the root node
      '()  ; no embedded arcs
      ((select-kids (ntype?? '*any*))
       ((select-kids (ntype?? 'sxlink))
        ((select-kids (ntype?? '@@)) node)))))

; Returns SXLink arcs that are specified at the top-level of the document and
; start from node
(define (xlink:node-arcs-on-top node document)
  (cond
    ((assq node (xlink:arcs-outgoing document))
     => cdr)
    (else '())))

; Returns all SXLink arcs (both embedded and specified at the top-level) that
; start from ther node
; The union of the two previous functions
(define (xlink:node-arcs node document)
  (append (xlink:node-embedded-arcs node)
          (xlink:node-arcs-on-top node document)))

;-------------------------------------------------
; Traversing SXLink arcs

; Traverse all SXLink arcs to their ending resources
;  sxlink-arcs ::= (listof sxlink-arc)
;  linked-docs ::= (listof document)
;  num-ancestors - number of ancestors required for ending resources
(define (xlink:traverse-arcs sxlink-arcs linked-docs num-ancestors)
  (let* ((arcs-to
          ((select-kids (ntype?? 'to)) sxlink-arcs))
         (req-docs
          (apply
           xlink:get-docs-with-respect-to-loaded
           (cons
            linked-docs
            (if
             (and num-ancestors (zero? num-ancestors))
             ((select-kids (ntype?? '*text*))
              ((select-kids (ntype?? 'uri))
               (filter  ; elements that have a <nodes> subelement
                (lambda (arc-to)
                  (null? ((select-kids (ntype?? 'nodes)) arc-to)))
                arcs-to)))
            ((select-kids (ntype?? '*text*))
             ((select-kids (ntype?? 'uri)) arcs-to)))))))
    ;(pp req-docs)
    (map-union
     (lambda (arc-to)
       (let ((nodes-nset
              ((select-kids (ntype?? 'nodes)) arc-to)))
         (if
          (and num-ancestors (zero? num-ancestors)
               (not (null? nodes-nset)))
          (cadar nodes-nset)
          ; otherwise we need the document and the XPointer node
          (let ((doc (xlink:find-doc
                      (car ((select-kids (ntype?? '*text*))
                            ((select-kids (ntype?? 'uri)) arc-to)))
                      req-docs))
                (xpointer-nset
                 ((select-kids (ntype?? '*text*))
                  ((select-kids (ntype?? 'xpointer)) arc-to))))
            ;(pp doc)            
            ;(display xpointer-nset)
            ;(newline)
            (cond
              ((not doc)  ; document couldn't be loaded
               '())
              ((null? xpointer-nset)
               ; no XPointer part => addresses the document element
               ((draft:child (ntype?? '*) num-ancestors)
                doc))
              (else
               (let ((impl
                      (draft:xpointer (car xpointer-nset)
                                      (if num-ancestors num-ancestors -1))))
                 (if
                  (not impl)  ; parser error
                  '()
                  (let ((res (impl doc)))
                    (if
                     (nodeset? res)
                     res
                     (begin
                       (xlink:api-error
                        "XPointer fragment identifier doesn't "
                        "select any nodeset: " (car xpointer-nset))
                       '())))))))))))
     arcs-to)))

;-------------------------------------------------
; Additional XPath axes

; XPath+XLink arc axis
; This axis returns all SXLink arcs that start from the context node
;  num-ancestors is dummy here, since SXLink arcs don't have ancestors
(define (xlink:axis-arc test-pred? . num-ancestors)
  (let ((this-axis
         (lambda (node)  ; not a nodeset
           (let ((root-node
                  (if (sxml:context? node)
                      (draft:list-last (sxml:context->ancestors-u node))
                      node)))
             (if (draft:top? root-node)
                 (xlink:node-arcs (sxml:context->node node) root-node)
                 (xlink:node-embedded-arcs (sxml:context->node node)))))))
    (lambda (node)   ; node or nodeset
      (filter test-pred?
              (if (nodeset? node)
                  (map-union this-axis node)
                  (this-axis node))))))

; XPath+XLink traverse axis
; This axis traverses from the context node
; The lambda produced additionally takes the var-binding. In var-binding, the
; linked-docs can be stored in the administrative variable '*docs*
(define (xlink:axis-traverse test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (get-arcs  ; returns SXLink arcs that start from a given node
          (lambda (node)  ; not a nodeset
            (let ((root-node
                   (if (sxml:context? node)
                       (draft:list-last (sxml:context->ancestors-u node))
                       node)))
             (if (draft:top? root-node)
                 (xlink:node-arcs (sxml:context->node node) root-node)
                 (xlink:node-embedded-arcs (sxml:context->node node)))))))
    ; node can be both a single node and a nodeset here
    (lambda (node var-binding)
      (filter
       (lambda (node)
         (test-pred? (sxml:context->node node)))       
       (xlink:traverse-arcs
        (if (nodeset? node)
            (map-union get-arcs node)
            (get-arcs node))
        (xlink:docs-variable var-binding)
        num-anc)))))

; XPath+XLink traverse-arc axis
; The axis traverses from the context node that is an SXLink arc
; The lambda produced additionally takes the var-binding. In var-binding, the
; linked-docs can be stored in the administrative variable '*docs*
(define (xlink:axis-traverse-arc test-pred? . num-ancestors)
  (let ((num-anc (if (null? num-ancestors) 0 (car num-ancestors))))
    (lambda (node var-binding)
      (filter
       (lambda (node)
         (test-pred? (sxml:context->node node)))       
       (xlink:traverse-arcs
        (filter xlink:arc?
                (draft:reach-root (as-nodeset node)))
        (xlink:docs-variable var-binding)
        num-anc)))))
