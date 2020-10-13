;; Scheme primitives implemented in Scheme.
;; The quasiquote, and a few others, are from Darius Bacon <djello@well.com>
;; (But then, he started with my PAIP code, and modified it.)
;; - Peter Norvig

;;;;;;;;;;;;;;;; Extensions: new names for old procedures

(define call/cc    call-with-current-continuation)
(define first 	   car)
(define second     cadr)
(define third      caddr)
(define rest 	   cdr)
(define set-first! set-car!)
(define set-rest!  set-cdr!)

;;;;;;;;;;;;;;;; Standard Scheme Macros

(define or
  (macro args
    (if (null? args)
	#f
	(cons 'cond (map list args)))))

(define and
  (macro args
    (cond ((null? args) #t)
	  ((null? (rest args)) (first args))
	  (else (list 'if (first args) (cons 'and (rest args)) #f)))))

(define quasiquote
  (macro (x)
    (define (constant? exp)
      (if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))
    (define (combine-skeletons left right exp)
      (cond
       ((and (constant? left) (constant? right))
	(if (and (eqv? (eval left) (car exp))
		 (eqv? (eval right) (cdr exp)))
	    (list 'quote exp)
	    (list 'quote (cons (eval left) (eval right)))))
       ((null? right) (list 'list left))
       ((and (pair? right) (eq? (car right) 'list))
	(cons 'list (cons left (cdr right))))
       (else (list 'cons left right))))
    (define (expand-quasiquote exp nesting)
      (cond
       ((vector? exp)
	(list 'apply 'vector (expand-quasiquote (vector->list exp) nesting)))
       ((not (pair? exp))
	(if (constant? exp) exp (list 'quote exp)))
       ((and (eq? (car exp) 'unquote) (= (length exp) 2))
	(if (= nesting 0)
	    (second exp)
	    (combine-skeletons ''unquote
			       (expand-quasiquote (cdr exp) (- nesting 1))
			       exp)))
       ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
	(combine-skeletons ''quasiquote
			   (expand-quasiquote (cdr exp) (+ nesting 1))
			   exp))
       ((and (pair? (car exp))
	     (eq? (caar exp) 'unquote-splicing)
	     (= (length (car exp)) 2))
	(if (= nesting 0)
	    (list 'append (second (first exp))
		  (expand-quasiquote (cdr exp) nesting))
	    (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
			       (expand-quasiquote (cdr exp) nesting)
			       exp)))
       (else (combine-skeletons (expand-quasiquote (car exp) nesting)
				(expand-quasiquote (cdr exp) nesting)
				exp))))
    (expand-quasiquote x 0)))

(define let
  (macro (bindings . body)
    (define (named-let name bindings body)
      `(let ((,name #f))
	 (set! ,name (lambda ,(map first bindings) . ,body))
	 (,name . ,(map second bindings))))
    (if (symbol? bindings)
	(named-let bindings (first body) (rest body))
	`((lambda ,(map first bindings) . ,body) . ,(map second bindings)))))

(define let*
  (macro (bindings . body)
    (if (null? bindings) `((lambda () . ,body))
	`(let (,(first bindings))
	   (let* ,(rest bindings) . ,body)))))

(define letrec
  (macro (bindings . body)
    (let ((vars (map first bindings))
	  (vals (map second bindings)))
    `(let ,(map (lambda (var) `(,var #f)) vars)
       ,@(map (lambda (var val) `(set! ,var ,val)) vars vals)
       . ,body))))

(define case
  (macro (exp . cases)
    (define (do-case case)
      (cond ((not (pair? case)) (error "bad syntax in case" case))
	    ((eq? (first case) 'else) case)
	    (else `((member __exp__ ',(first case)) . ,(rest case)))))
    `(let ((__exp__ ,exp)) (cond . ,(map do-case cases)))))

(define do
  (macro (bindings test-and-result . body)
    (let ((variables (map first bindings))
	  (inits (map second bindings))
	  (steps (map (lambda (clause)
			(if (null? (cddr clause))
			    (first clause)
			    (third clause)))
		      bindings))
	  (test (first test-and-result))
	  (result (rest test-and-result)))
      `(letrec ((__loop__
		 (lambda ,variables
		   (if ,test
		       (begin . ,result)
		       (begin
			 ,@body
			 (__loop__ . ,steps))))))
	 (__loop__ . ,inits)))))

(define delay
  (macro (exp)
    (define (make-promise proc)
      (let ((result-ready? #f)
	    (result #f))
	(lambda ()
	  (if result-ready?
	      result
	      (let ((x (proc)))
		(if result-ready?
		    result
		    (begin (set! result-ready? #t)
			   (set! result x)
			   result)))))))
    `(,make-promise (lambda () ,exp))))

;;;;;;;;;;;;;;;; Extensions

(define time
  (macro (exp . rest) `(time-call (lambda () ,exp) . ,rest)))
