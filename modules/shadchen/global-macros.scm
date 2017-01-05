(define *match-fail* (string->symbol "match-fail-e1aa3b7e7ce9731266013c178de842b5"))

;; Evaluate expressions as though in a `begin` at macro-expansion time
;; and at run time.
(define-macro (at-expand-time-and-runtime #!rest exprs)
   (eval `(begin ,@exprs))
   `(begin ,@exprs))

;; Evaluate expressions as thought in a `begin` at macro-expansion time.
(define-macro (at-expand-time #!rest exprs)
  (eval `(begin ,@exprs))
  `(begin #f))

(at-expand-time-and-runtime

 ;; Contains custom pattern matchers. 
 (define *custom-matchers* (make-table test: eq?))

 (define (ensure-code o)
   (if (##source? o) (##source-code o) o))

 ;; Utility to add a pattern matcher expander. 
 (define (add-pattern-matcher name fun)
   (table-set! *custom-matchers* name fun))

 ;; Given a symbol/name and arguments, return a new pattern matching
 ;; expression by calling the correct expander.
 (define (expand-custom-matcher name #!rest args)
   (apply (table-ref *custom-matchers* name) args))

 ;; Returns #t when SYMBOL denotes a custom pattern matcher.
 (define (custom-pattern? symbol)
   (if (table-ref *custom-matchers* symbol) #t #f)))

;; Add a custom pattern matcher.  Patterns are similar to macros in
;; that they take arguments and must return a pattern expression.
(define-macro (define-pattern name-and-args #!rest body)
  `(at-expand-time-and-runtime
	(add-pattern-matcher ',(car name-and-args)
						 (lambda ,(cdr name-and-args) ,@body))))

(at-expand-time-and-runtime 

 ;; Given the sub-patterns from a list pattern matching expression,
 ;; return all the implied bound symbols.
 (define (list-pattern-bound-symbols sub-patterns #!optional (acc '()))
   (cond ((eq? sub-patterns '()) acc)
		 ((and (= 2 (length sub-patterns))
			   (eq? '... (cadr sub-patterns)))
		  (append (pattern-bound-symbols (car sub-patterns)) acc))
		 (else (list-pattern-bound-symbols (cdr sub-patterns) (append acc (pattern-bound-symbols (car sub-patterns)))))))

 ;; Given any pattern expression, return a list of the symbols it binds on success.
 (define (pattern-bound-symbols pattern) 
   (define (symbol< s1 s2)
	 (string<? (symbol->string s1)
			   (symbol->string s2)))
   (define (pattern-bound-symbols* pattern #!optional (acc (list)))
	 (cond
	  ((eq? pattern #t) acc)
	  ((eq? pattern #f) acc)
	  ((symbol? pattern) (cons pattern acc))
	  ((or (number? pattern) (keyword? pattern) (string? pattern)) acc)
	  ((list? pattern)
	   (let ((sigil (car pattern))
			 (pattern-body (cdr pattern)))
		 (cond 
		  ((eq? sigil 'let)
		   (append acc (map car pattern-body)))
		  ((eq? sigil 'quote) acc)
		  ((eq? sigil 'list)
		   (append acc (list-pattern-bound-symbols pattern-body)))
		  ((eq? sigil 'and)
		   (apply append acc (map pattern-bound-symbols* pattern-body)))
		  ((eq? sigil 'or)
		   (pattern-bound-symbols* (car pattern-body) acc))
		  ((or 
			(eq? sigil 'call)
			(eq? sigil 'call*)
			(eq? sigil '?))
		   (let ((predicate-expr (car pattern-body))
				 (patterns (cdr pattern-body)))
			 (pattern-bound-symbols* `(and ,@patterns) acc)))
		  (else 
		   (if (custom-pattern? sigil) 
			   (pattern-bound-symbols* (apply expand-custom-matcher sigil pattern-body) acc)
			   (error (string-append " Unrecognized pattern name: " (symbol->string sigil))))))))))
   (pattern-bound-symbols* pattern))

 ;; Return #t when S1 and S2, lists of symbols, are set-equivalent.
 (define (sets-equal? s1 s2 #!key (test eq?))
   (define (list->truth-table l)
	 (let ((tbl (make-table test: test)))
	   (let sets-equal-loop ((rest l))
		 (if (eq? '() rest)
			 tbl
			 (begin (table-set! tbl (car rest) #t)
					(sets-equal-loop (cdr rest)))))))
   (define (all-keys-in-table? keys tbl)	 
	 (if (eq? '() keys) #t
		 (let ((key (car keys))
			   (rest (cdr keys)))
		   (if (table-ref tbl key #f)
			   (all-keys-in-table? rest tbl)
			   #f))))
   (and (all-keys-in-table? s1 (list->truth-table s2))
		(all-keys-in-table? s2 (list->truth-table s1))))

 ;; Return #t when all of the sets, lists of symbols, in SETS are
 ;; set-equivalent.
 (define (all-sets-equal? #!rest sets) 
   (define (all-sets-equal?* set1 set2 #!rest sets)
	 (let ((these-two (sets-equal? set1 set2))) 
	   (if (eq? '() sets) these-two		
		   (if these-two (apply all-sets-equal?* set1 sets) #f))))
   (if (> (length sets) 1) (apply all-sets-equal?* sets)
	   #t)))

(##define-syntax backwards
  (lambda (form-src)
    (let ((body (reverse (cdr (##source-code form-src)))))
      (##sourcify
        `(begin ,@body)
        form-src))))

(##define-syntax match1-or-fail
  (lambda (form-src) ;;(match1-or-fail expr form #!rest body)
    (let* ((code (cdr (ensure-code form-src)))
	   (expr (car code))
	   (form (car (cdr code)))
	   (form-naked (##desourcify (car (cdr code))))
	   (body (cdr (cdr code))))
      (newline)
      (##sourcify
       (cond 
	((eq? form-naked #t) `(if (eq? ,expr #t) (begin ,@body) *match-fail*))
	((eq? form-naked #f) `(if (eq? ,expr #f) (begin ,@body) *match-fail*))
	((symbol? form-naked)
	 `(let ((,form ,expr)) ,@body))
	((string? form-naked)
	 `(if (equal? ,form ,expr) (begin ,@body) *match-fail*))
	((number? form-naked)
	 `(if (= ,form ,expr) (begin ,@body) *match-fail*))
	((keyword? form-naked)
	 `(if (eq? ,form ,expr) (begin ,@body) *match-fail*))
	((list? form-naked)
	 (let* ((pattern-head (car (##desourcify form)))
		(pattern-head-symbol (car form-naked))
		(pattern-body (cdr (##desourcify form))))
	   (cond 
	    ((eq? 'let pattern-head-symbol)
	     `(let (,@pattern-body) ,@body))
	    ((eq? 'quote pattern-head-symbol)
	     `(if (equal? ,expr ,form)
		  (begin ,@body)
		  *match-fail*))
	    ((eq? 'and pattern-head-symbol)
	     (let ((terms (cdr (##desourcify form)))
		   (value-name (gensym 'match-value)))
	       (if (eq? '() terms)
		   `(begin ,@body)
		   `(let ((,value-name ,expr))
		      (match1-or-fail ,value-name ,(cadr (##desourcify form))
				      (match1-or-fail ,value-name (and ,@(cddr (##desourcify form))) ,@body))))))
	    ((eq? 'or pattern-head-symbol)
	     (let ((terms (cdr (##desourcify form)))
		   (raw-terms (##desourcify (cdr (##desourcify form))))
		   (value-name (gensym 'match-value))
		   (result-name (gensym 'match-result-value)))
	       (if (not (apply all-sets-equal? (map pattern-bound-symbols raw-terms)))
		   (error "shadchen: or patterns must all bind identical symbols."))
	       (cond 
		((= 0 (length terms))
		 `(begin ,@body))
		((= 1 (length terms))
		 `(match1-or-fail ,expr ,(car (##desourcify terms)) ,@body))
		(else
		 (let* ((bound-symbols (pattern-bound-symbols (car terms))))
			`(let* ((,value-name ,expr)
				(,result-name (match1-or-fail ,value-name ,(car (##desourcify terms)) (list ,@bound-symbols))))
			   (if (eq? *match-fail* ,result-name)
			       (begin 
				 (match1-or-fail ,value-name (or ,@(cdr (##desourcify terms))) ,@body))
			       (match1-or-fail ,result-name (list ,@bound-symbols) ,@body))))))))
	    ((eq? 'list pattern-head-symbol)
	     (let ((terms (cdr (##desourcify form)))
		   (value-name (gensym 'match-value)))
	       (cond
		((eq? '() terms)
		 `(let ((,value-name ,expr))
		    (if (eq? '() ,value-name) (begin ,@body) *match-fail*)))
		((and (= 2 (length terms))
		      (eq? '... (##desourcify (cadr terms))))
		 (let ((pattern (car terms)))
		   `(match1-or-fail ,expr ,pattern ,@body)))
		(else (let ((sub-pattern (car terms))
			    (first-value-name (gensym 'first-value)))
			`(let ((,value-name ,expr))
			   (if (or (not (list? ,value-name))
				   (eq? '() ,value-name)) 
			       *match-fail*
			       (let ((,first-value-name (car ,value-name)))
				 (match1-or-fail ,first-value-name ,sub-pattern 
						 (match1-or-fail (cdr ,value-name) (list ,@(cdr terms)) ,@body))))))))))
	    ((eq? '? pattern-head-symbol)
	     (let* ((terms (cdr (ensure-code form)))
		    (predicate-expr (car terms))
		    (patterns (cdr (cdr (ensure-code form))))
		    (value-name (gensym 'match-value)))
	       `(let ((,value-name ,expr))
		  (if (,predicate-expr ,value-name)
		      (match1-or-fail ,value-name (and ,@patterns) ,@body)
		      *match-fail*))))
	    ((eq? 'call pattern-head-symbol)
	     (let* ((terms (cdr (ensure-code form)))
		    (transform-expr (car terms))
		    (patterns (cdr terms)))
	       `(match1-or-fail (,transform-expr ,expr) (and ,@patterns) ,@body)))
	    ((eq? 'call* pattern-head-symbol)
	     (let* ((terms (cdr (ensure-code form)))
		    (transform-expr (car terms))
		    (patterns (cdr terms))
		    (value-name (gensym 'match-value)))
	       `(let ((,value-name (,transform-expr ,expr)))
		  (if (eq? ,value-name *match-fail*) *match-fail*
		      (match1-or-fail ,value-name (and ,@patterns) ,@body)))))
	    (else 
	     (if (custom-pattern? pattern-head-symbol)
		 `(match1-or-fail ,expr ,(apply expand-custom-matcher pattern-head pattern-body) ,@body)
		 (error (string-append "Unknown pattern expander " (symbol->string pattern-head)))))))))
       form-src))))

(##define-syntax match-helper
  (lambda (form-src)
      (let* ((code (cdr (ensure-code form-src)))
	     (whole-expression (car code))
	     (value (car (cdr code)))
	     (sub-expressions (cdr (cdr code))))
	(cond
	 ((eq? '() sub-expressions)
	  (##sourcify `(error "Match failed:" ,value 'in 'expression: ',whole-expression) form-src))
	 (else
	  (let* ((sub-expression (car sub-expressions))
		 (pattern (car (ensure-code sub-expression)))
		 (sub-body (cdr (ensure-code sub-expression)))
		 (sub-expressions (cdr sub-expressions))
		 (match-result (gensym 'match-result)))
	    (##sourcify
	     `(let ((,match-result (match1-or-fail ,value ,pattern ,@sub-body)))
		(if (eq? *match-fail* ,match-result)
		    (match-helper ,whole-expression ,value ,@sub-expressions)
		    ,match-result))
	     form-src)))))))

(##define-syntax match
  (lambda (form-src)
    (let* ((code (cdr (ensure-code form-src)))
	   (value (car code))
	   (sub-expressions (cdr code))
	   (value-name (gensym 'value)))
      (##sourcify
       `(let ((,value-name ,value)) 
	  (match-helper (match ,value ,@sub-expressions)
			,value-name
			,@sub-expressions))
       form-src))))

;; Match against a vector (semantics identical to list)
(define-pattern (vector #!rest patterns)
  `(and (? vector?)
		(call vector->list (list ,@patterns))))

;; match against the car and cdr of a pair.
(define-pattern (pair cr cd)
  `(and (? pair?)
		(call (lambda (pair)
				(list (car pair) (cdr pair)))
			  (list ,cr ,cd))))
