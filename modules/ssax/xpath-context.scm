
;;(include "../ssax/SSAX-code.sch")
;;(include "../libs/gambit/common.sch")
;;(include "../libs/gambit/myenv.sch")
;;(include "../libs/input-parse.sch")
;;(include "../multi-parser/id/http.sch")
;;(include "../multi-parser/id/srfi-12.sch")
;;(include "../html-prag/htmlprag.sch")
;;(include "../sxml-tools/sxml-tools.sch")

;; Context-based XPath implementation
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lisovsky@acm.org      Kirill Lisovsky
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; <nodeset> ::= ( <nodeset-member>* )
; <nodeset-member> ::= <node> | <context>
; <context> ::= ( *CONTEXT*  <node>  <ancestor>* )
; <node> - an SXML node (a context node)
; <ancestor>* - context node's parent, grandparent, grandgrandparent etc.
;
; A CONTEXT doesn't contain more ANCESTORs than actually required for
; evaluating the location path. This is achieved by means of an "intellectual"
; parsing of the location path. The number of ANCESTORs stored in the CONTEXT
; can differ for different path steps.

;=========================================================================
; Basic operations over context

; A fast however unsafe predicate
; Assumes that the 'node' provided is a pair
(define (sxml:context-u? node)
  (eq? (car node) '*CONTEXT*))

; Safer predicate
(define (sxml:context? node)
  (and (pair? node) (eq? (car node) '*CONTEXT*)))

;-------------------------------------------------
; Accessors

; Fast however unsafe accessors
; Assume that the argument is the proper context
(define sxml:context->node-u cadr)
(define sxml:context->ancestors-u cddr)
(define sxml:context->content-u cdr)

; Safe accessors
; Can be applied to both a context and an ordinary node
(define (sxml:context->node context)
  (if (sxml:context? context) (cadr context) context))

(define (sxml:context->ancestors context)
  (if (sxml:context? context) (cddr context) '()))

(define (sxml:context->content context)
  (if (sxml:context? context) (cdr context) (list context)))

; Given a context-set, converts it to a nodeset
(define (draft:contextset->nodeset obj)
  (if (nodeset? obj)
      (map sxml:context->node obj)
      obj))

;-------------------------------------------------
; Mutators

; Constructor
(define (draft:make-context node ancestors)
  (cons '*CONTEXT* (cons node ancestors)))

; A smarter constructor
; Makes context only when required, with the 'num-anc' required
(define (draft:smart-make-context node ancestors num-anc)
  (if
   (or (and num-anc (zero? num-anc))
       (null? ancestors))
   node  ; no need for context construction
   (cons '*CONTEXT*
         (cons node
               (draft:list-head ancestors num-anc)))))

; Provided a 'nodeset' of sibling nodes, wraps each into context
; If 'ancestors' is empty, keeps 'nodeset' unchanged
(define (draft:siblings->context-set nodeset ancestors)
  (if (null? ancestors)
      nodeset
      (map
       (lambda (node) (draft:make-context node ancestors))
       nodeset)))

;-------------------------------------------------
; Operations on num-ancestors
; Complexity results from #f as a value for num-ancestors (which means that the
; number of ancestors is infinite)

(define (draft:na+ na1 na2)
  (if
   (or (not na1) (not na2)) ; either argument is infinite
   #f
   (+ na1 na2)))

(define (draft:na-minus na value)
  (if (not na) na (- na value)))

; Minus, with the result that is always non-negative
(define (draft:na-minus-nneg na value)
  (cond
    ((not na) na)
    ((< (- na value) 0) 0)
    (else (- na value))))

(define (draft:na-max . na-lst)
  (cond
    ((null? na-lst) 0)
    ((member #f na-lst) #f)
    (else (apply max na-lst))))

(define (draft:na-min . na-lst)
  (if
   (null? na-lst) 0
   (let ((num-lst (filter (lambda (x) x) na-lst)))
     (if (null? num-lst) #f  ; all na-lst consists of #f
         (apply min num-lst)))))

(define (draft:na> na1 na2)
  (cond
   ((not na2) ; second argument in infinite
    #f)
   ((not na1) ; first argument is infinite
    #t)
   (else  ; niether argument is infinite
    (> na1 na2))))

(define (draft:na>= na1 na2)
  (cond
   ((not na2) ; second argument in infinite
    (not na1))  
   ((not na1) ; first argument is infinite
    #t)
   (else  ; niether argument is infinite
    (>= na1 na2))))


;=========================================================================
; Misc helpers

; Similar to R5RS 'list-tail' but returns the new list consisting of the first
; 'k' members of 'lst'
; If k>(length lst) or k=#f, lst is returned
; NOTE1: k=#f is used in this implementation to represent positive infinity
; NOTE2: Unless k=#f, the result is always a newly allocated list. This is the
;  main methodological difference between this function and R5RS 'list-tail'
(define (draft:list-head lst k)
  (letrec
      ((list-head
        (lambda (lst k)
          (if (or (null? lst) (zero? k))
              '()
              (cons (car lst) (list-head (cdr lst) (- k 1)))))))
    (if k
        (list-head lst k)
        lst)))

; Returns the last member of the list
; It is an error for the list to be empty
(define (draft:list-last lst)
  (if (null? (cdr lst))
      (car lst)
      (draft:list-last (cdr lst))))

; Constructs the (listof value), whose length is num
(define (draft:make-list value num)
  (if (= num 0)
      '()
      (cons value (draft:make-list value (- num 1)))))

; Similar to txp:signal-semantic-error, but returns #f
(define (draft:signal-semantic-error . text)
  (apply txp:signal-semantic-error text)
  #f)

; The top of the SXML document?
(define (draft:top? node)
  (and (pair? node) (eq? (car node) '*TOP*)))

; Removes eq duplicates from the nodeset
(define (draft:remove-eq-duplicates nodeset)
  (cond
    ((null? nodeset) nodeset)
    ((memq (car nodeset) (cdr nodeset))
     (draft:remove-eq-duplicates (cdr nodeset)))
    (else
     (cons (car nodeset) (draft:remove-eq-duplicates (cdr nodeset))))))

; Reaches the root of the root of the contextset
; Result: nodeset
(define (draft:reach-root contextset)
  (let ((nodeset (map
                  (lambda (node)
                    (if
                     (sxml:context? node)
                     (draft:list-last (sxml:context->ancestors-u node))
                     node))
                  contextset)))
    (if (or (null? nodeset) (null? (car nodeset)))  ; (length nodeset)<=1
        nodeset
        (draft:remove-eq-duplicates nodeset))))

; Recovers context for each node of the nodeset
; Context recovery is performed in its usual technique: searching from the
; root of the document. As a result, this function can be fairly slow.
; In this implementation, it is sometimes called after an XPath 'id' function,
; for location paths like  "id(name)/.."
; By nature of 'id-index', context is lost when we access elements by their
; ID. It may be a good idea to rework the structure of 'id-index' to make it
; more suitable for purposes of this context-based XPath implementation.
; A good news is that only a few elements are usually selected by XPath 'id'
; function, thus the overhead of searching from the root node might be
; acceptable in this case. 
(define (draft:recover-contextset nodeset root-node num-anc)
  (map
   (lambda (node)
     (draft:smart-make-context
      node
      (((sxml:ancestor (lambda (x) #t)) root-node) node)
      num-anc))
   nodeset))

;-------------------------------------------------
; For sxpath: handling a procedure as a location step

; Makes a context-set from a nodeset supplied, with the num-anc required
;  ancestors-set ::= (listof ancestors)
;  ancestors ::= (listof node)
; Members of the nodeset are known to be descendants-or-selves of
; (map car ancestors-set)
(define (draft:find-proper-context nodeset ancestors-set num-anc)
  (map
   (lambda (node)
     (if
      (sxml:context? node)  ; already a context
      node  ; nothing to be done  
      (let loop ((this-level ancestors-set)
                 (next-level '()))
        (if
         (null? this-level)  ; this level fully analyzed
         (if (null? next-level)  ; failed to find
             node
             (loop next-level '()))
         (let ((ancestors (car this-level)))
           (if
            (eq? node (car ancestors))  ; proper ancestors found
            (draft:make-context
             node
             (draft:list-head (cdr ancestors) num-anc))
            (loop (cdr this-level)
                  (append
                   (map
                    (lambda (n) (cons n ancestors))
                    ((sxml:child sxml:node?) (car ancestors)))
                   (map
                    (lambda (n) (cons n ancestors))
                    ((sxml:attribute (lambda (x) #t)) (car ancestors)))
                   next-level))))))))
   nodeset))


;=========================================================================
; XPath axes
; Implementation is based on the concept of context
; Compared to "general" SXPath, a new optional argument was added:
;  NUM-ANCESTORS - number of node's ancestors that will be required later in
;  the location path. For example, NUM-ANCESTORS=1 means that the node's parent
;  only must be remembered in the CONTEXT, grandparents will not be required

; Ancestor axis
(define (draft:ancestor test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (sxml:context? node)
             (let loop ((ancs-to-view (sxml:context->ancestors-u node))
                        (res '()))
               (cond
                 ((null? ancs-to-view)  ; processed everyone
                  (reverse res)  ; reverse document order required
                  )
                 ((test-pred? (car ancs-to-view))  ; can add it to result
                  (loop
                   (cdr ancs-to-view)
                   (cons
                    (draft:smart-make-context
                     (car ancs-to-view) (cdr ancs-to-view) num-anc)
                    res)))
                 (else  ; current node doesn't satisfy the predicate
                  (loop (cdr ancs-to-view) res))))
             '()  ; no ancestors
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Ancestor-or-self axis
(define (draft:ancestor-or-self test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (cond
              ((sxml:context? node)
               (let loop ((ancs-to-view (sxml:context->content-u node))
                          (res '()))
                 (cond
                   ((null? ancs-to-view)  ; processed everyone
                    (reverse res)  ; reverse document order required
                    )
                   ((test-pred? (car ancs-to-view))  ; can add it to result
                    (loop
                     (cdr ancs-to-view)
                     (cons
                      (draft:smart-make-context
                       (car ancs-to-view) (cdr ancs-to-view) num-anc)
                      res)))
                   (else  ; current node doesn't satisfy the predicate
                    (loop (cdr ancs-to-view) res)))))
              ; ordinary SXML node
              ((test-pred? node)  ; satisfies the predicate
               (list node))
              (else
               '())))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Attribute axis
; Borrows much from draft:child
(define (draft:attribute test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (cond
              ((not (pair? node)) '())   ; no attributes
              ; (car node) is always a symbol
              ((sxml:context-u? node)  ; a context node
               (draft:siblings->context-set
                ((sxml:filter test-pred?)
                 (sxml:attr-list (sxml:context->node-u node)))
                (draft:list-head (sxml:context->content-u node) num-anc)))
              (else  ; an ordinary node, and is a pair
               (draft:siblings->context-set
                ((sxml:filter test-pred?) (sxml:attr-list node))
                (draft:list-head (list node) num-anc)))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))  

; Child axis
(define (draft:child test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (cond
              ((not (pair? node)) '())   ; no children
              ; (car node) is always a symbol
              ((sxml:context-u? node)  ; a context node
               (draft:siblings->context-set
                ((select-kids test-pred?) (sxml:context->node-u node))
                (draft:list-head (sxml:context->content-u node) num-anc)))
              ; an ordinary node, and is a pair
              ((memq (car node) '(*PI* *COMMENT* *ENTITY*))
               '())
              (else
               (draft:siblings->context-set
                ((sxml:filter test-pred?) (cdr node))  ; like in 'select-kids'
                (draft:list-head (list node) num-anc)))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Descendant axis
(define (draft:descendant test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (draft:child sxml:node? num-anc))
         (this-axis
          (lambda (node)  ; not a nodeset                        
            (let rpt ((res '())
                      (more (child node)))
              (if (null? more)
                  (reverse res)
                  (rpt
                   (if (test-pred? (sxml:context->node (car more)))
                       (cons (car more) res)
                       res)
                   (append (child (car more)) (cdr more))))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))
                        
; Descendant-or-self axis
(define (draft:descendant-or-self test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (child (draft:child sxml:node? num-anc))
         (this-axis
          (lambda (node)  ; not a nodeset                        
            (let rpt ((res '())
                      (more (list node)))
              (if (null? more)
                  (reverse res)
                  (rpt
                   (if (test-pred? (sxml:context->node (car more)))
                       (cons (car more) res)
                       res)
                   (append (child (car more)) (cdr more))))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Following axis
(define (draft:following test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (draft:descendant-or-self test-pred? num-anc))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (sxml:context? node)
             (let loop ((curr-node (sxml:context->node-u node))
                        (ancs-to-view (sxml:context->ancestors-u node))
                        (res '()))
               (if
                (null? ancs-to-view)  ; processed everyone                 
                res
                (loop
                 (car ancs-to-view)
                 (cdr ancs-to-view)
                 (append
                  res
                  (descend
                   (draft:siblings->context-set
                    (cond
                       ((memq curr-node
                        (cdr  ; parent is an element => cdr gives its children
                         (car ancs-to-view)))
                        => cdr)
                       (else  ; curr-node is an attribute node
                        ((select-kids sxml:node?) (car ancs-to-view))))
                     (draft:list-head ancs-to-view num-anc)))))))
             '()  ; no following members
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Following-sibling axis
(define (draft:following-sibling test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (and (sxml:context? node)                  
                  (not (null? (sxml:context->ancestors-u node))))
             (cond
               ((memq (sxml:context->node-u node)
                      (cdr  ; parent is an element => cdr gives its children
                       (car (sxml:context->ancestors-u node))))
                => (lambda (foll-siblings)
                     (draft:siblings->context-set
                      ((sxml:filter test-pred?) (cdr foll-siblings))
                      (draft:list-head
                       (sxml:context->ancestors-u node) num-anc))))
               (else  ; no following siblings
                '()))
             '()  ; no parent => no siblings
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Namespace axis
; Borrows much from draft:child
(define (draft:namespace test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (cond
              ((not (pair? node)) '())   ; no namespaces
              ; (car node) is always a symbol
              ((sxml:context-u? node)  ; a context node
               (draft:siblings->context-set
                ((sxml:filter test-pred?)
                 (sxml:ns-list (sxml:context->node-u node)))
                (draft:list-head (sxml:context->content-u node) num-anc)))
              (else  ; an ordinary node, and is a pair
               (draft:siblings->context-set
                ((sxml:filter test-pred?) (sxml:ns-list node))
                (draft:list-head (list node) num-anc)))))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Parent axis
(define (draft:parent test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (and (sxml:context? node)
                  (not (null? (sxml:context->ancestors-u node)))
                  (test-pred? (car (sxml:context->ancestors-u node))))
             (draft:smart-make-context
              (car (sxml:context->ancestors-u node))
              (cdr (sxml:context->ancestors-u node))
              num-anc)
             '()  ; no parent
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Preceding axis
(define (draft:preceding test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (descend (draft:descendant-or-self test-pred? num-anc))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (sxml:context? node)
             (let loop ((curr-node (sxml:context->node-u node))
                        (ancs-to-view (sxml:context->ancestors-u node))                        
                        (to-descend '()))
               (cond
                 ((null? ancs-to-view)  ; processed everyone
                  (map-union
                   (lambda (node) (reverse (descend node)))
                   to-descend))
                 ((memq curr-node
                        (reverse
                         ((select-kids sxml:node?)
                          (car ancs-to-view))))
                  => (lambda (prec-siblings)
                       (loop
                        (car ancs-to-view)
                        (cdr ancs-to-view)
                        (append
                         to-descend
                         (draft:siblings->context-set
                           (cdr prec-siblings)
                           (draft:list-head ancs-to-view num-anc))))))
                 (else  ; no preceding siblings
                  (loop (car ancs-to-view)
                        (cdr ancs-to-view)
                        to-descend))))
             '()  ; no preceding members
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Preceding-sibling axis
(define (draft:preceding-sibling test-pred? . num-ancestors)
  (let* ((num-anc (if (null? num-ancestors) 0 (car num-ancestors)))
         (this-axis
          (lambda (node)  ; not a nodeset
            (if
             (and (sxml:context? node)                  
                  (not (null? (sxml:context->ancestors-u node))))
             (cond
               ((memq (sxml:context->node-u node)
                      (reverse
                       (cdr  ; parent is an element => cdr gives its children
                        (car (sxml:context->ancestors-u node)))))
                => (lambda (prec-siblings)
                     (draft:siblings->context-set
                      ((sxml:filter test-pred?) (cdr prec-siblings))
                      (draft:list-head
                       (sxml:context->ancestors-u node) num-anc))))
               (else  ; no preceding siblings
                '()))
             '()  ; no parent => no siblings
             ))))
    (lambda (node)   ; node or nodeset
      (if (nodeset? node)
          (map-union this-axis node)
          (this-axis node)))))

; Self axis
; num-ancestors is not used here
(define (draft:self test-pred? . num-ancestors)
  (sxml:filter
   (lambda (node) (test-pred? (sxml:context->node node)))))


;==========================================================================
; XPath Core Function Library

;-------------------------------------------------
; 4.1 Node Set Functions

; last()
(define (draft:core-last num-anc)
  (lambda (nodeset position+size var-binding)
    (cdr position+size)))
  
; position()
(define (draft:core-position num-anc)
  (lambda (nodeset position+size var-binding)
    (car position+size)))

; count(node-set)
(define (draft:core-count num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (let ((res (arg-func nodeset position+size var-binding)))
      (cond
        ((nodeset? res) (length res))
        (else
         (sxml:xpointer-runtime-error
          "count() function - an argument is not a nodeset")
         0)))))

; id(object)
(define (draft:core-id num-anc arg-func) 
  (lambda (nodeset position+size var-binding)    
    (let* ((root-node (draft:reach-root nodeset))
           (id-nset ((sxml:child (ntype?? 'id-index))
                     ((sxml:child (ntype?? '@@)) root-node))))
      (if
       (null? id-nset)  ; no id-index
       '()  ; ID function returns an empty nodeset
       (let ((res ((sxml:id (cdar id-nset))  ; implemented in "sxpath-ext.scm"
                   (draft:contextset->nodeset
                    (arg-func nodeset position+size var-binding)))))
         (if (and num-anc (zero? num-anc))  ; no ancestors required
             res
             (draft:recover-contextset res root-node num-anc)))))))

; local-name(node-set?)
(define (draft:core-local-name num-anc . arg-func)  ; optional argument 
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((nodeset (draft:contextset->nodeset nodeset)))
          (cond
            ((null? nodeset) "")
            ((not (pair? (car nodeset))) "")  ; no name
            (else
             (let ((name (symbol->string (caar nodeset))))
               (cond
                 ((string-rindex name #\:)
                  => (lambda (pos)
                       (substring name (+ pos 1) (string-length name))))
                 (else  ; a NCName
                  name)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)          
          (let ((obj
                 (draft:contextset->nodeset
                  (func nodeset position+size var-binding))))
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")              
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring
                          name (+ pos 1) (string-length name))))
                   (else  ; a NCName
                    name))))))))))

; namespace-uri(node-set?)
(define (draft:core-namespace-uri num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((nodeset (draft:contextset->nodeset nodeset)))
          (cond
            ((null? nodeset) "")
            ((not (pair? (car nodeset))) "")  ; no name
            (else
             (let ((name (symbol->string (caar nodeset))))
               (cond
                 ((string-rindex name #\:)
                  => (lambda (pos)
                       (substring name 0 pos)))
                 (else  ; a NCName
                  "")))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)          
          (let ((obj
                 (draft:contextset->nodeset
                  (func nodeset position+size var-binding))))           
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (let ((name (symbol->string (caar obj))))
                 (cond
                   ((string-rindex name #\:)
                    => (lambda (pos)
                         (substring name 0 pos)))
                   (else ""))))))))))

; name(node-set?)
(define (draft:core-name num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((nodeset (draft:contextset->nodeset nodeset)))
          (cond
            ((null? nodeset) "")
            ((not (pair? (car nodeset))) "")  ; no name
            (else
             (symbol->string (caar nodeset))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)          
          (let ((obj
                 (draft:contextset->nodeset
                  (func nodeset position+size var-binding))))        
            (cond
              ((null? obj) "")  ; an empty nodeset
              ((not (nodeset? obj))
               (sxml:xpointer-runtime-error
                "NAME function - an argument is not a nodeset")
               "")
              ((not (pair? (car obj))) "")  ; no name
              (else
               (symbol->string (caar obj)))))))))


;-------------------------------------------------
; 4.2 String Functions

; string(object?)
(define (draft:core-string num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (sxml:string
         (draft:contextset->nodeset nodeset)))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (sxml:string
           (draft:contextset->nodeset
            (func nodeset position+size var-binding)))))))

; concat(string, string, string*)
(define (draft:core-concat num-anc . arg-func-lst)
  (lambda (nodeset position+size var-binding)
    (apply
     string-append
     (map
      (lambda (f)
        (sxml:string
         (draft:contextset->nodeset
          (f nodeset position+size var-binding))))
      arg-func-lst))))

; starts-with(string, string)
(define (draft:core-starts-with num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let ((str1 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func1 nodeset position+size var-binding))))
          (str2 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func2 nodeset position+size var-binding)))))
      (string-prefix? str2 str1))))

; contains(string, string)
(define (draft:core-contains num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let ((str1 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func1 nodeset position+size var-binding))))
          (str2 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func2 nodeset position+size var-binding)))))
      (if (substring? str2 str1) #t #f)  ; must return a boolean
      )))
  
; substring-before(string, string)
(define (draft:core-substring-before num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let* ((str1 (sxml:string
                  (draft:contextset->nodeset
                   (arg-func1 nodeset position+size var-binding))))
           (str2 (sxml:string
                  (draft:contextset->nodeset
                   (arg-func2 nodeset position+size var-binding))))
           (pos (substring? str2 str1)))
      (if (not pos)  ; STR1 doesn't contain STR2
          ""
          (substring str1 0 pos)))))

; substring-after(string, string)
(define (draft:core-substring-after num-anc arg-func1 arg-func2)
  (lambda (nodeset position+size var-binding)
    (let* ((str1 (sxml:string
                  (draft:contextset->nodeset
                   (arg-func1 nodeset position+size var-binding))))
           (str2 (sxml:string
                  (draft:contextset->nodeset
                   (arg-func2 nodeset position+size var-binding))))
           (pos (substring? str2 str1)))
      (if
       (not pos)  ; STR1 doesn't contain STR2
       ""
       (substring
        str1 (+ pos (string-length str2)) (string-length str1))))))

; substring(string, number, number?)
(define (draft:core-substring num-anc arg-func1 arg-func2 . arg-func3)
  (if (null? arg-func3)  ; no third argument supplied
      (lambda (nodeset position+size var-binding)
        (let ((str (sxml:string
                    (draft:contextset->nodeset
                     (arg-func1 nodeset position+size var-binding))))
              (num1 (sxml:number
                     (draft:contextset->nodeset
                      (arg-func2 nodeset position+size var-binding)))))
          (let ((len (string-length str))
                (start (- (inexact->exact (round num1)) 1)))
            (if (> start len)
                ""
                (substring str (if (< start 0) 0 start) len)))))
      (let ((arg-func3 (car arg-func3)))
        (lambda (nodeset position+size var-binding)
          (let ((str (sxml:string
                      (draft:contextset->nodeset
                       (arg-func1 nodeset position+size var-binding))))
                (num1 (sxml:number
                       (draft:contextset->nodeset
                        (arg-func2 nodeset position+size var-binding))))
                (num2 (sxml:number
                       (draft:contextset->nodeset
                        (arg-func3 nodeset position+size var-binding)))))
            (let* ((len (string-length str))
                   (start (- (inexact->exact (round num1)) 1))
                   (fin (+ start (inexact->exact (round num2)))))
              (if (or (> start len) (< fin 0) (< fin start))
                  ""
                  (substring str
                             (if (< start 0) 0 start)
                             (if (> fin len) len fin)))))))))

; string-length(string?)
(define (draft:core-string-length num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (string-length
         (sxml:string (draft:contextset->nodeset nodeset))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (string-length
           (sxml:string
            (draft:contextset->nodeset
             (func nodeset position+size var-binding))))))))

; normalize-space(string?)
(define (draft:core-normalize-space num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (let rpt ((src (string-split
                        (sxml:string (draft:contextset->nodeset nodeset))
                        sxml:whitespace))
                  (res '()))
          (cond
            ((null? src)
             (apply string-append (reverse res)))
            ((= (string-length (car src)) 0)  ; empty string
             (rpt (cdr src) res))
            ((null? res)
             (rpt (cdr src) (cons (car src) res)))
            (else
             (rpt (cdr src) (cons (car src) (cons " " res)))))))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (let rpt ((src (string-split
                          (sxml:string
                           (draft:contextset->nodeset
                            (func nodeset position+size var-binding)))
                          sxml:whitespace))
                    (res '()))
            (cond
              ((null? src)
               (apply string-append (reverse res)))
              ((= (string-length (car src)) 0)  ; empty string
               (rpt (cdr src) res))
              ((null? res)
               (rpt (cdr src) (cons (car src) res)))
              (else
               (rpt (cdr src) (cons (car src) (cons " " res))))))))))

; translate(string, string, string)
(define (draft:core-translate num-anc arg-func1 arg-func2 arg-func3)
  (lambda (nodeset position+size var-binding)    
    (let ((str1 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func1 nodeset position+size var-binding))))
          (str2 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func2 nodeset position+size var-binding))))
          (str3 (sxml:string
                 (draft:contextset->nodeset
                  (arg-func3 nodeset position+size var-binding)))))
      (let ((alist
             (let while ((lst2 (string->list str2))
                         (lst3 (string->list str3))
                         (alist '()))
               (cond
                 ((null? lst2) (reverse alist))
                 ((null? lst3)
                  (append
                   (reverse alist)
                   (map
                    (lambda (ch) (cons ch #f))
                    lst2)))
                 (else
                  (while
                   (cdr lst2)
                   (cdr lst3)
                   (cons (cons (car lst2) (car lst3)) alist)))))))
        (let rpt ((lst1 (string->list str1))
                  (res '()))
          (cond
            ((null? lst1) (list->string (reverse res)))
            ((assoc (car lst1) alist)
             => (lambda (pair)
                  (if (cdr pair)
                      (rpt (cdr lst1) (cons (cdr pair) res))
                      (rpt (cdr lst1) res))))
            (else
             (rpt (cdr lst1) (cons (car lst1) res)))))))))
  

;-------------------------------------------------
; 4.3 Boolean Functions

; boolean(object)
(define (draft:core-boolean num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (sxml:boolean
     (arg-func nodeset position+size var-binding))))

; not(boolean)
(define (draft:core-not num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (not (sxml:boolean 
          (arg-func nodeset position+size var-binding)))))

; true()
(define (draft:core-true num-anc)
  (lambda (nodeset position+size var-binding) #t))

; false()
(define (draft:core-false num-anc)
  (lambda (nodeset position+size var-binding) #f))

; lang(string)
(define (draft:core-lang num-anc arg-func)
  (lambda (nodeset position+size var-binding)    
    (let ((arg (sxml:string
                (draft:contextset->nodeset
                 (arg-func nodeset position+size var-binding))))
          (lng
           ((draft:child (ntype?? '*text*))
            ((draft:attribute (ntype?? 'xml:lang))
             ((draft:ancestor-or-self (lambda (x) #t))
              (car nodeset)  ; context-node = (car nodeset)
              )))))
      (and (not (null? lng))
           (or (string-ci=? arg (car lng))
               (string-prefix-ci? (string-append arg "-") (car lng)))))))       
  

;-------------------------------------------------
; 4.4 Number Functions

; number(object?)
(define (draft:core-number num-anc . arg-func)  ; optional argument
  (if (null? arg-func)  ; no argument supplied
      (lambda (nodeset position+size var-binding)
        (sxml:number (draft:contextset->nodeset nodeset)))
      (let ((func (car arg-func)))
        (lambda (nodeset position+size var-binding)
          (sxml:number
           (draft:contextset->nodeset
            (func nodeset position+size var-binding)))))))

; sum(node-set)
(define (draft:core-sum num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (let ((res (arg-func nodeset position+size var-binding)))
      (cond
        ((nodeset? res)
         (apply +
                (map
                 (lambda (node)
                   (sxml:number
                    (sxml:string-value (sxml:context->node node))))
                 res)))
        (else
         (sxml:xpointer-runtime-error
          "SUM function - an argument is not a nodeset")
         0)))))

; floor(number)
(define (draft:core-floor num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (inexact->exact
     (floor (sxml:number
             (draft:contextset->nodeset
              (arg-func nodeset position+size var-binding)))))))

; ceiling(number)
(define (draft:core-ceiling num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (inexact->exact
     (ceiling (sxml:number
               (draft:contextset->nodeset
                (arg-func nodeset position+size var-binding)))))))

; round(number)
(define (draft:core-round num-anc arg-func)
  (lambda (nodeset position+size var-binding)
    (inexact->exact
     (round (sxml:number
             (draft:contextset->nodeset
              (arg-func nodeset position+size var-binding)))))))


;=========================================================================
; XPath AST processing
; AST is considered to be properly formed
       
; {5} <AxisSpecifier> ::= (axis-specifier  <AxisName> )
; {6} <AxisName> ::= (ancestor)
;                    | (ancestor-or-self)
;                    | (attribute)
;                    | (child)
;                    | (descendant)
;                    | (descendant-or-self)
;                    | (following)
;                    | (following-sibling)
;                    | (namespace)
;                    | (parent)
;                    | (preceding)
;                    | (preceding-sibling)
;                    | (self)
;                    | (arc)  ; the following 3 are added by SXLink
;                    | (traverse)
;                    | (traverse-arc)
; Returns:  (list lambda num-ancestors pass-vars?)
;  pass-vars? - a boolean: whether var-bindings must be passed to the axis
(define (draft:ast-axis-specifier op num-anc)
  (if
   (not (eq? (car op) 'axis-specifier))
   (draft:signal-semantic-error "not an AxisSpecifier - " op)
   (case (caadr op)  ; AxisName
     ((ancestor)
      (list draft:ancestor #f #f))
     ((ancestor-or-self)
      (list draft:ancestor-or-self #f #f))
     ((attribute)
      (list draft:attribute (draft:na-minus-nneg num-anc 1) #f))
     ((child)
      (list draft:child (draft:na-minus-nneg num-anc 1) #f))
     ((descendant)
      (list draft:descendant (draft:na-minus-nneg num-anc 1) #f))
     ((descendant-or-self)
      (list draft:descendant-or-self num-anc #f))
     ((following)
      (list draft:following #f #f))
     ((following-sibling)
      (list draft:following-sibling (draft:na-max num-anc 1) #f))
     ((namespace)
      (list draft:namespace (draft:na-minus-nneg num-anc 1) #f))
     ((parent)
      (list draft:parent (draft:na+ num-anc 1) #f))
     ((preceding)
      (list draft:preceding #f #f))
     ((preceding-sibling)
      (list draft:preceding-sibling (draft:na-max num-anc 1) #f))
     ((self)
      (list draft:self num-anc #f))
     ((arc)
      (list xlink:axis-arc #f #f))
     ((traverse)
      (list xlink:axis-traverse #f #t))
     ((traverse-arc)
      (list xlink:axis-traverse-arc #f #t))
     (else
      (draft:signal-semantic-error "unknown AxisName - " op)))))


; {7} <NodeTest> ::= (node-test (*))
;                    | (node-test (namespace-uri  <String> ))
;                    | (node-test (namespace-uri  <String> )?
;                                 (local-name  <String> ))
;                    | (node-test (comment))
;                    | (node-test (text))
;                    | (node-test (pi <String>? ))
;                    | (node-test (point))
;                    | (node-test (range))
; + added by sxpath native syntax:
;                    | (node-test (equal?  <SXML-node> ))
;                    | (node-test (eq?  <SXML-node> ))
;                    | (node-test (names  <String>+ ))
;                    | (node-test (not-names  <String>+ ))s
(define (draft:ast-node-test op)
  (if
   (not (eq? (car op) 'node-test))
   (draft:signal-semantic-error "not an NodeTest - " op)
   (case (caadr op)  ; NodeTest name
     ((*)
      (ntype?? '*))
     ((namespace-uri)
      (cond
        ((= (length op) 2)  ; NodeTest in the form of prefix:*
         (ntype-namespace-id?? (cadadr op)))
        ((eq? (caaddr op) 'local-name)
         (ntype?? (string->symbol
                   (string-append (cadadr op) ":" (cadr (caddr op))))))
        (else
         (draft:signal-semantic-error "improper QName NodeTest - " op))))
     ((local-name)      
      (ntype?? (string->symbol (cadadr op))))
     ((comment)
      (ntype?? '*COMMENT*))
     ((text)
      (ntype?? '*text*))
     ((pi)
      (if (= (length (cadr op)) 2)  ; PI target supplied
          (let ((target (string->symbol (cadadr op))))
            (lambda (node)
              (and (pair? node)
                   (eq? (car node) '*PI*)
                   (equal? (cadr node) target))))
          (lambda (node)
            (and (pair? node) (eq? (car node) '*PI*)))))
     ((node) sxml:node?)
     ((point)
      (draft:signal-semantic-error
       "point() NodeTest is not supported by this implementation"))
     ((range)
      (draft:signal-semantic-error
       "range() NodeTest is not supported by this implementation"))
     ((equal?)
      (node-equal? (cadadr op)))
     ((eq?)
      (node-eq? (cadadr op)))
     ((names)
      (ntype-names?? (cdadr op)))
     ((not-names)
      (sxml:complement (ntype-names?? (cdadr op))))
     (else
      (draft:signal-semantic-error "unknown NodeTest - " op)))))

;-------------------------------------------------
; In this section, each function accepts 2 arguments
;  op - S-expression which represents the operation
;  num-anc - how many ancestors are required in the context after that
;            operation
; and returns either #f, which signals of a semantic error, or
;  (cons (lambda (nodeset position+size var-binding) ...)
;        num-anc-it-requires)
;  position+size - the same to what was called 'context' in TXPath-1  
  
; {1} <LocationPath> ::= <RelativeLocationPath>
;                        | <AbsoluteLocationPath>
(define (draft:ast-location-path op num-anc)
  (case (car op)
    ((absolute-location-path)
     (draft:ast-absolute-location-path op num-anc))
    ((relative-location-path)
     (draft:ast-relative-location-path op num-anc))
    (else
     (draft:signal-semantic-error "improper LocationPath - " op))))

; {2} <AbsoluteLocationPath> ::= (absolute-location-path  <Step>* )
(define (draft:ast-absolute-location-path op num-anc)
  (cond
    ((not (eq? (car op) 'absolute-location-path))
     (draft:signal-semantic-error "not an AbsoluteLocationPath - " op))
    ((null? (cdr op))  ; no Steps
     (cons
      (lambda (nodeset position+size var-binding)
        (draft:reach-root nodeset))
      #f))
    (else
     (and-let*
      ((steps-res (draft:ast-step-list (cdr op) num-anc)))
      (cons
       (if
        (null? (cdar steps-res))  ; only a single step
        (let ((step-impl (caar steps-res)))
          (lambda (nodeset position+size var-binding)
            (step-impl
             (draft:reach-root nodeset) position+size var-binding)))
        (let ((converters (car steps-res)))
          (lambda (nodeset position+size var-binding)
            (let rpt ((nset (draft:reach-root nodeset))
                      (fs converters))
              (if (null? fs)
                  nset
                  (rpt ((car fs) nset position+size var-binding)
                       (cdr fs)))))))
       #f)))))

; {3} <RelativeLocationPath> ::= (relative-location-path  <Step>+ )
(define (draft:ast-relative-location-path op num-anc)
  (if
   (not (eq? (car op) 'relative-location-path))
   (draft:signal-semantic-error "not a RelativeLocationPath - " op)
   (and-let*
    ((steps-res (draft:ast-step-list (cdr op) num-anc)))
    (cons
     (if
      (null? (cdar steps-res))  ; only a single step
      (caar steps-res)
      (let ((converters (car steps-res)))
        (lambda (nodeset position+size var-binding)
          (let rpt ((nset nodeset)
                    (fs converters))
            (if (null? fs)
                nset
                (rpt ((car fs) nset position+size var-binding)
                     (cdr fs)))))))
     (cdr steps-res)))))

; {4} <Step> ::= (step  <AxisSpecifier> <NodeTest> <Predicate>* )
;                | (range-to  (expr <Expr>)  <Predicate>* )
(define (draft:ast-step op num-anc)
  (cond
    ((eq? (car op) 'range-to)
     (draft:signal-semantic-error "range-to function not implemented"))
    ((eq? (car op) 'filter-expr)  ; can be produced by sxpath
     (draft:ast-filter-expr op num-anc))
    ((eq? (car op) 'lambda-step)  ; created by sxpath
     (cons
      (let ((proc (cadr op)))
        (if
         (and num-anc (zero? num-anc))  ; no ancestors required
         (lambda (node position+size var-binding)
           (proc (draft:contextset->nodeset (as-nodeset node))
                 var-binding))
         (lambda (node position+size var-binding)
           (draft:find-proper-context
            (proc (draft:contextset->nodeset (as-nodeset node))
                  var-binding)
            (append
             (map sxml:context->content (as-nodeset node))
             (apply append   ; nodes that can be obtained through var values
                    (map
                     (lambda (pair)
                       (if (nodeset? (cdr pair))
                           (map sxml:context->content (cdr pair))
                           '()))
                     var-binding)))
            num-anc))))
      num-anc))
    ((eq? (car op) 'step)
     (if
      (null? (cdddr op))  ; no Predicates
      (and-let*
       ((axis-lst (draft:ast-axis-specifier (cadr op) num-anc))
        (ntest (draft:ast-node-test (caddr op))))
       (let ((axis ((car axis-lst) ntest num-anc)))
         (cons
          (if (caddr axis-lst)  ; var-binding is to be passed
              (lambda (nodeset position+size var-binding)
                (axis nodeset var-binding))
              (lambda (nodeset position+size var-binding)
                (axis nodeset)))
          (cadr axis-lst))))
      (and-let*
       ((preds-res (draft:ast-predicate-list (cdddr op) 0))
        (axis-lst (draft:ast-axis-specifier
                   (cadr op) (draft:na-max num-anc (cdr preds-res))))
        (ntest (draft:ast-node-test (caddr op))))
       (let ((axis ((car axis-lst)
                    ntest (draft:na-max num-anc (cdr preds-res))))
             (pred-impl-lst (car preds-res)))
         (cons
          (if
           (caddr axis-lst)  ; variables are to be passed to the axis
           (lambda (nodeset position+size var-binding)
             (map-union
              (lambda (node)
                (let loop ((nset (axis node var-binding))
                           (preds pred-impl-lst))
                  (if
                   (null? preds)
                   nset
                   (loop ((car preds) nset position+size var-binding)
                         (cdr preds)))))
              nodeset))
           (lambda (nodeset position+size var-binding)
             (map-union
              (lambda (node)
                (let loop ((nset (axis node))
                           (preds pred-impl-lst))
                  (if
                   (null? preds)
                   nset
                   (loop ((car preds) nset position+size var-binding)
                         (cdr preds)))))
              nodeset)))
          (cadr axis-lst))))))
    (else
     (draft:signal-semantic-error "not a Step - " op))))

; {4a} ( <Step>+ )
; Returns (cons (listof step-impl) num-anc) or #f
(define (draft:ast-step-list step-lst num-anc)
  (let loop ((steps-to-view (reverse step-lst))
             (res-lst '())
             (num-anc num-anc))
    (if
     (null? steps-to-view)  ; everyone processed
     (cons res-lst num-anc)
     (and-let*
      ((step-res (draft:ast-step (car steps-to-view) num-anc)))
      (loop
       (cdr steps-to-view)
       (cons (car step-res) res-lst)
       (cdr step-res))))))

; {8} <Predicate> ::= (predicate  <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for Predicates
(define (draft:ast-predicate op num-anc)
  (if
   (not (eq? (car op) 'predicate))
   (draft:signal-semantic-error "not an Predicate - " op)
   (and-let*
    ((expr-res (draft:ast-expr (cadr op) 0)))
    (let ((pred (car expr-res)))
      (cons
       (lambda (nodeset position+size var-binding)         
         (if
          (null? nodeset)  ; already empty
          nodeset  ; nothing to filter
          (let ((size (length nodeset)))  ; context size              
            (let loop ((nset nodeset)
                       (res '())
                       (pos 1))
              (if
               (null? nset)
               (reverse res)
               (let ((value (pred (list (car nset))
                                  (cons pos size)
                                  var-binding)))
                 (loop (cdr nset)
                       (if (if (number? value)
                               (= value pos)
                               (sxml:boolean value))
                           (cons (car nset) res)
                           res)
                       (+ pos 1))))))))
       (cdr expr-res))))))

; {8a} ( <Predicate>+ )
; Returns (cons (listof pred-impl) num-anc) or #f
; NOTE: num-anc is dummy here, since it is always 0 for Predicates
(define (draft:ast-predicate-list op-lst num-anc)  
  (let ((pred-res-lst
         (map
          (lambda (op) (draft:ast-predicate op 0))
          op-lst)))
    (if
     (member #f pred-res-lst)  ; error detected
     #f
     (cons
      (map car pred-res-lst)
      (apply draft:na-max (map cdr pred-res-lst))))))

; {9} <Expr> ::= <OrExpr>
;                | <AndExpr>
;                | <EqualityExpr>
;                | <RelationalExpr>
;                | <AdditiveExpr>
;                | <MultiplicativeExpr>
;                | <UnionExpr>
;                | <PathExpr>
;                | <FilterExpr>
;                | <VariableReference>
;                | <Literal>
;                | <Number>
;                | <FunctionCall>
;                | <LocationPath>
(define (draft:ast-expr op num-anc)
  (case (car op)
    ((or)
     (draft:ast-or-expr op num-anc))
    ((and)
     (draft:ast-and-expr op num-anc))
    ((= !=)
     (draft:ast-equality-expr op num-anc))
    ((< > <= >=)
     (draft:ast-relational-expr op num-anc))
    ((+ -)
     (draft:ast-additive-expr op num-anc))
    ((* div mod)
     (draft:ast-multiplicative-expr op num-anc))
    ((union-expr)
     (draft:ast-union-expr op num-anc))
    ((path-expr)
     (draft:ast-path-expr op num-anc))
    ((filter-expr)
     (draft:ast-filter-expr op num-anc))
    ((variable-reference)
     (draft:ast-variable-reference op num-anc))
    ((literal)
     (draft:ast-literal op num-anc))
    ((number)
     (draft:ast-number op num-anc))
    ((function-call)
     (draft:ast-function-call op num-anc))
    ((absolute-location-path)
     (draft:ast-absolute-location-path op num-anc))
    ((relative-location-path)
     (draft:ast-relative-location-path op num-anc))
    (else
     (draft:signal-semantic-error "unknown Expr - " op))))

; {10} <OrExpr> ::= (or <Expr> <Expr>+ )
; NOTE: num-anc is dummy here, since it is always 0 for OrExpr
(define (draft:ast-or-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (let rpt ((fs expr-impls))
          (cond
            ((null? fs) #f)
            ((sxml:boolean ((car fs) nodeset position+size var-binding)) #t)
            (else (rpt (cdr fs))))))
      (apply draft:na-max (map cdr expr-res-lst)))))))
     
; {11} <AndExpr> ::= (and <Expr> <Expr>+ )
; NOTE: num-anc is dummy here, since it is always 0 for AndExpr
(define (draft:ast-and-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (let rpt ((fs expr-impls))
          (cond
            ((null? fs) #t)
            ((not
              (sxml:boolean ((car fs) nodeset position+size var-binding)))
             #f)
            (else (rpt (cdr fs))))))
      (apply draft:na-max (map cdr expr-res-lst)))))))

; {12} <EqualityExpr> ::= (=  <Expr> <Expr> )
;                         | (!=  <Expr> <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for EqualityExpr
(define (draft:ast-equality-expr op num-anc)
  (and-let*
   ((left-lst (draft:ast-expr (cadr op) 0))
    (right-lst (draft:ast-expr (caddr op) 0)))
   (let ((cmp-op (cadr (assq (car op) `((= ,sxml:equal?)
                                        (!= ,sxml:not-equal?)))))
         (left (car left-lst))
         (right (car right-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (draft:contextset->nodeset
          (left nodeset position+size var-binding))
         (draft:contextset->nodeset
          (right nodeset position+size var-binding))))
      (draft:na-max (cdr left-lst) (cdr right-lst))))))

; {13} <RelationalExpr> ::= (<  <Expr> <Expr> )
;                           | (>  <Expr> <Expr> )
;                           | (<=  <Expr> <Expr> )
;                           | (>=  <Expr> <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for RelationalExpr
(define (draft:ast-relational-expr op num-anc)
  (and-let*
   ((left-lst (draft:ast-expr (cadr op) 0))
    (right-lst (draft:ast-expr (caddr op) 0)))
   (let ((cmp-op
          (sxml:relational-cmp
           (cadr (assq (car op) `((< ,<) (> ,>) (<= ,<=) (>= ,>=))))))
         (left (car left-lst))
         (right (car right-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (cmp-op
         (draft:contextset->nodeset
          (left nodeset position+size var-binding))
         (draft:contextset->nodeset
          (right nodeset position+size var-binding))))
      (draft:na-max (cdr left-lst) (cdr right-lst))))))

; {14} <AdditiveExpr> ::= (+  <Expr> <Expr> )
;                         | (-  <Expr> <Expr>? )
; NOTE: num-anc is dummy here, since it is always 0 for AdditiveExpr
(define (draft:ast-additive-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((add-op (cadr (assq (car op) `((+ ,+) (- ,-)))))
           (expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (apply
         add-op
         (map
          (lambda (expr)
            (sxml:number
             (draft:contextset->nodeset
              (expr nodeset position+size var-binding))))
          expr-impls)))
      (apply draft:na-max (map cdr expr-res-lst)))))))

; {15} <MultiplicativeExpr> ::= (*  <Expr> <Expr> )
;                               | (div  <Expr> <Expr> )
;                               | (mod  <Expr> <Expr> )
; NOTE: num-anc is dummy here, since it is always 0 for MultiplicativeExpr
(define (draft:ast-multiplicative-expr op num-anc)
  (and-let*
   ((left-lst (draft:ast-expr (cadr op) 0))
    (right-lst (draft:ast-expr (caddr op) 0)))
   (let ((mul-op
          (cadr (assq (car op) `((* ,*) (div ,/) (mod ,remainder)))))
         (left (car left-lst))
         (right (car right-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (mul-op
         (sxml:number
          (draft:contextset->nodeset
           (left nodeset position+size var-binding)))
         (sxml:number
          (draft:contextset->nodeset
           (right nodeset position+size var-binding)))))
      (draft:na-max (cdr left-lst) (cdr right-lst))))))

; {16} <UnionExpr> ::= (union-expr  <Expr> <Expr>+ )
(define (draft:ast-union-expr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (let rpt ((res '())
                  (fs expr-impls))
          (if
           (null? fs)
           res
           (let ((nset ((car fs) nodeset position+size var-binding)))
             (rpt
              (append 
               res
               (cond
                 ((not (nodeset? nset))
                  (sxml:xpointer-runtime-error 
                   "expected - nodeset instead of " nset)
                  '())
                 (else nset)))
              (cdr fs))))))
      (apply draft:na-max (map cdr expr-res-lst)))))))

; {17} <PathExpr> ::= (path-expr  <FilterExpr> <Step>+ )
(define (draft:ast-path-expr op num-anc)
  (and-let*
    ((steps-res (draft:ast-step-list (cddr op) num-anc))
     (filter-lst (draft:ast-filter-expr (cadr op) (cdr steps-res))))
    (let ((init-impl (car filter-lst))
          (converters (car steps-res)))
      (cons       
        (lambda (nodeset position+size var-binding)
          (let ((nset
                 (init-impl nodeset position+size var-binding)))
            (let rpt ((nset 
                       (cond
                         ((nodeset? nset) nset)
                         (else
                          (sxml:xpointer-runtime-error 
                           "expected - nodeset instead of " nset)
                          '())))
                      (fs converters))
              (if (null? fs)
                  nset
                  (rpt ((car fs) nset position+size var-binding)
                       (cdr fs))))))
        (cdr filter-lst)))))

; {18} <FilterExpr> ::= (filter-expr (primary-expr  <Expr> )
;                                    <Predicate>* )
(define (draft:ast-filter-expr op num-anc)
  (cond
    ((not (eq? (car op) 'filter-expr))
     (draft:signal-semantic-error "not an FilterExpr - " op))
    ((not (eq? (caadr op) 'primary-expr))
     (draft:signal-semantic-error "not an PrimaryExpr - " (cadr op)))
    ((null? (cddr op))  ; no Predicates
     (draft:ast-expr (cadadr op) num-anc))
    (else
     (and-let*
       ((preds-res (draft:ast-predicate-list (cddr op) 0))
        (expr-lst (draft:ast-expr
                   (cadadr op) (draft:na-max num-anc (cdr preds-res)))))
       (let ((expr-impl (car expr-lst))
             (pred-impl-lst (car preds-res)))
         (cons
          (lambda (nodeset position+size var-binding)
            (let ((prim-res (expr-impl nodeset position+size var-binding)))              
              (let loop ((nset (cond
                                 ((nodeset? prim-res) prim-res)
                                 (else 
                                  (sxml:xpointer-runtime-error 
                                   "expected - nodeset instead of " prim-res)
                                  '())))
                         (preds pred-impl-lst))
                (if
                 (null? preds)
                 nset
                 (loop ((car preds) nset position+size var-binding)
                       (cdr preds))))))
            (cdr expr-lst)))))))     

; {19} <VariableReference> ::= (variable-reference  <String> )
(define (draft:ast-variable-reference op num-anc)
  (let ((name (string->symbol (cadr op))))
    (cons
     (lambda (nodeset position+size var-binding)
       (cond
         ((assoc name var-binding)
          => cdr)
         (else
          (sxml:xpointer-runtime-error "unbound variable - " name)
          '())))
     0)))

; {20} <Literal> ::= (literal  <String> )
(define (draft:ast-literal op num-anc)
  (let ((literal (cadr op)))
    (cons
     (lambda (nodeset position+size var-binding) literal)
     0)))
     
; {21} <Number> :: (number  <Number> )
(define (draft:ast-number op num-anc)
  (let ((number (cadr op)))
    (cons
     (lambda (nodeset position+size var-binding) number)
     0)))

; {22} <FunctionCall> ::= (function-call (function-name  <String> )
;                                        (argument  <Expr> )* )
(define (draft:ast-function-call op num-anc)
  (let ((core-alist
         ; (list fun-name min-num-args max-num-args na4res impl)
         `((last 0 0 0 ,draft:core-last)
           (position 0 0 0 ,draft:core-position)
           (count 1 1 0 ,draft:core-count)
           (id 1 1 #f ,draft:core-id)
           (local-name 0 1 0 ,draft:core-local-name)
           (namespace-uri 0 1 0 ,draft:core-namespace-uri)
           (name 0 1 0 ,draft:core-name)
           (string 0 1 0 ,draft:core-string)
           (concat 2 -1 0 ,draft:core-concat)
           (starts-with 2 2 0 ,draft:core-starts-with)
           (contains 2 2 0 ,draft:core-contains)
           (substring-before 2 2 0 ,draft:core-substring-before)
           (substring-after 2 2 0 ,draft:core-substring-after)
           (substring 2 3 0 ,draft:core-substring)
           (string-length 0 1 0 ,draft:core-string-length)
           (normalize-space 0 1 0 ,draft:core-normalize-space)
           (translate 3 3 0 ,draft:core-translate)
           (boolean 1 1 0 ,draft:core-boolean)
           (not 1 1 0 ,draft:core-not)
           (true 0 0 0 ,draft:core-true)
           (false 0 0 0 ,draft:core-false)
           (lang 1 1 #f ,draft:core-lang)
           (number 0 1 0 ,draft:core-number)
           (sum 1 1 0 ,draft:core-sum)
           (floor 1 1 0 ,draft:core-floor)
           (ceiling 1 1 0 ,draft:core-ceiling)
           (round 1 1 0 ,draft:core-round))))
    (cond
      ((not (eq? (caadr op) 'function-name))
       (draft:signal-semantic-error "not an FunctionName - " (cadr op)))
      ((assq (string->symbol (cadadr op)) core-alist)       
       => (lambda (description)  ; Core function found
            (cond
              ((< (length (cddr op)) (cadr description))
               (draft:signal-semantic-error
                "too few arguments for the Core Function call - "
                (cadadr op)))
              ((and (>= (caddr description) 0)
                    (> (length (cddr op)) (caddr description)))
               (draft:signal-semantic-error
                "too many arguments for the Core Function call - "
                (cadadr op)))
              (else  ; correct number of arguments
               (and-let*
                ((args-impl (draft:ast-function-arguments (cddr op))))
                (cons
                 ; Producing a function implementation
                 (apply (list-ref description 4) num-anc args-impl)
                 (list-ref description 3)))))))
           (else  ; function definition not found
            (draft:signal-semantic-error
             "function call to an unknown function - " (cadadr op))))))

; {22a} ( (argument  <Expr> )* )
; na-lst - number of ancestors required for each of the arguments
; Returns: (listof expr-impl) or #f
(define (draft:ast-function-arguments op-lst)
  (let ((arg-res-lst
         (map
          (lambda (op)
            (if
             (not (eq? (car op) 'argument))
             (draft:signal-semantic-error "not an Argument - " op)
             (draft:ast-expr (cadr op) 0)))
          op-lst)))
    (if
     (member #f arg-res-lst)  ; semantic error detected
     #f
     (map car arg-res-lst))))


;-------------------------------------------------
; Section dedicated to XPointer AST

; {25} <XPointer> ::= <ChildSeq>
;                     | <FullXPtr>
;                     | <Expr>
(define (draft:ast-xpointer op num-anc)    
  (case (car op)
    ((child-seq)
     (draft:ast-child-seq op num-anc))
    ((full-xptr)
     (draft:ast-full-xptr op num-anc))
    (else
     (draft:ast-expr op num-anc))))

; {26} <ChildSeq> ::= (child-seq (name  <String> ))
;                     | (child-seq (name  <String> )?
;                                  (number  <Number> )+ )
(define (draft:ast-child-seq op num-anc)
  (if
   (eq? (caadr op) 'name)
   (and-let*
    ((numbers-res (draft:ast-number-list (cddr op) num-anc)))
    (let ((id-value (cadadr op))
          (converters (car numbers-res))
          (num-ancestors (cdr numbers-res)))
      (cons
       (lambda (nodeset position+size var-binding)
         (let* ((root-node (draft:reach-root nodeset))
                (id-nset ((sxml:child (ntype?? 'id-index))
                          ((sxml:child (ntype?? '@@)) root-node))))
           (if
            (null? id-nset)  ; no id-index
            '()
            (let ((nd (sxml:lookup id-value (cdar id-nset))))
              (if (not nd)
                  '()
                  (let rpt ((nset 
                             (if (and num-ancestors (zero? num-ancestors))
                                 (list nd)
                                 (draft:recover-contextset
                                  (list nd) root-node num-ancestors)))
                            (fs converters))
                    (if (null? fs)
                        nset
                        (rpt ((car fs) nset) (cdr fs)))))))))
       #f)))
   (and-let*
    ((numbers-res (draft:ast-number-list (cdr op) num-anc)))
    (let ((converters (car numbers-res)))
      (cons
       (lambda (nodeset position+size var-binding)
         (let ((child-seq-impl
                (lambda (node)
                  (let rpt ((nset nodeset) (fs converters))
                    (if (null? fs)
                        nset
                        (rpt ((car fs) nset) (cdr fs)))))))
           (if (nodeset? nodeset)
               (map-union child-seq-impl nodeset)
               (child-seq-impl nodeset))))
       (cdr numbers-res))))))
    
; {26a} ( (number  <Number> )+ )
; Returns (cons (listof sxpath-converter) num-anc) or #f
(define (draft:ast-number-list number-lst num-anc)
  (let loop ((to-view (reverse number-lst))
             (res-lst '())
             (num-anc num-anc))
    (cond
      ((null? to-view)  ; everyone processed
       (cons res-lst num-anc))
      ((not (eq? (caar to-view) 'number))
       (draft:signal-semantic-error "not an Number - " (car to-view)))
      (else       
       (loop
        (cdr to-view)        
        (cons (draft:child (ntype?? '*) num-anc)
              (cons (node-pos (cadar to-view))
                    res-lst))
        (draft:na-minus-nneg num-anc 1))))))

; {27} <FullXPtr> ::= (full-xptr  <Expr> <Expr>+ )
(define (draft:ast-full-xptr op num-anc)
  (let ((expr-res-lst
         (map
          (lambda (expr) (draft:ast-expr expr 0))
          (cdr op))))
    (if
     (member #f expr-res-lst)  ; error detected
     #f
     (let ((expr-impls (map car expr-res-lst)))
     (cons
      (lambda (nodeset position+size var-binding)
        (let rpt ((fs expr-impls))
          (if (null? fs)
              '()
              (let ((nset ((car fs) nodeset position+size var-binding)))
                (if (null? nset)
                    (rpt (cdr fs))
                    nset)))))        
      (apply draft:na-max (map cdr expr-res-lst)))))))


;=========================================================================
; Highest level API functions
; xpath-string - an XPath location path (a string)
; ns+na - can contain 'ns-binding' and/or 'num-ancestors' and/or none of them
; ns-binding - declared namespace prefixes (an optional argument)
;  ns-binding ::= (listof (prefix . uri))
;  prefix - a symbol
;  uri - a string
; num-ancestors - number of ancestors required for resulting nodeset. Can
;  generally be omitted and is than defaulted to 0, which denotes a _usual_
;  nodeset. If a negative number, this signals that all ancestors should be
;  remembered in the context
;
; Returns: (lambda (nodeset position+size var-binding) ...)
; position+size - the same to what was called 'context' in TXPath-1
; var-binding - XPath variable bindings (an optional argument)
;  var-binding = (listof (var-name . value))
;  var-name - (a symbol) a name of a variable
;  value - its value. The value can have the following type: boolean, number,
;  string, nodeset. NOTE: a node must be represented as a singleton nodeset

; Given a list of arguments, returns
;  (values ns-binding num-anc)
(define (draft:arglist->ns+na arglst)
  (let loop ((arglst arglst)
             (ns-binding '())
             (num-anc 0))
    (cond
      ((null? arglst) (values ns-binding num-anc))
      ((pair? (car arglst))
       (loop (cdr arglst) (car arglst) num-anc))
      ((number? (car arglst))
       (loop (cdr arglst) ns-binding
             (if (negative? (car arglst)) #f (car arglst))))
      (else
       (loop (cdr arglst) ns-binding num-anc)))))
  
; Helper for constructing several highest-level API functions
(define (draft:api-helper grammar-parser ast-parser)
  (lambda (xpath-string . ns+na)
    (call-with-values
     (lambda () (draft:arglist->ns+na ns+na))
     (lambda (ns-binding num-anc)
       (and-let*
        ((ast (grammar-parser xpath-string ns-binding))
         (impl-lst (ast-parser ast num-anc)))
        (let ((res (car impl-lst)))
          (lambda (node . var-binding)
            ((if (and num-anc (zero? num-anc))
                 draft:contextset->nodeset
                 (lambda (x) x))             
             (res (as-nodeset node) (cons 1 1)
                  ;(xlink:add-docs-to-vars
                  ; node
                  (if (null? var-binding)
                      var-binding (car var-binding))
                  ; )
                  )))))))))

(define draft:xpath (draft:api-helper txp:xpath->ast draft:ast-location-path))
(define draft:xpointer (draft:api-helper txp:xpointer->ast draft:ast-xpointer))
(define draft:xpath-expr (draft:api-helper txp:expr->ast draft:ast-expr))
(define draft:sxpath (draft:api-helper txp:sxpath->ast draft:ast-expr))

; Aliases
(define txpath-with-context draft:xpath)
(define txpath/c draft:xpath)
(define sxpath-with-context draft:sxpath)
(define sxpath/c draft:sxpath)
