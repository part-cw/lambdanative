;; 		   My Standard Scheme "Prelude"
;;
;; This version of the prelude contains several forms and procedures
;; that are specific to a Gambit-C 3.0 system.
;; See myenv-scm.scm, myenv-bigloo.scm, etc. for versions
;; of this prelude that are tuned to other Scheme systems.
;;
;; $Id: myenv.scm,v 1.12 2001/03/14 18:21:42 oleg Exp oleg $



;==============================================================================
; Assertion tests

; assert truth of an expression (or a sequence of expressions)
; if there is more than one expression, they're 'AND'ed
 
(define-macro (assert . x)
                  (if (null? (cdr x))
                    `(or ,@x (error "failed assertion" ',@x))
                    `(or (and ,@x) (error "failed assertion" '(,@x)))))

;==============================================================================
; Some useful increment/decrement operators
; Note, ##fixnum prefix is Gambit-specific, it means that the
; operands assumed FIXNUM (as they ought to be anyway).
; This perfix could be safely removed: it'll leave the code just as
; correct, but more portable (and less efficient)

; Mutable increment
(define-macro (++! x) `(set! ,x (##fixnum.+ 1 ,x)))

; Read-only increment
(define-macro (++ x) `(##fixnum.+ 1 ,x))

; Mutable decrement
(define-macro (--! x) `(set! ,x (##fixnum.- ,x 1)))

; Read-only decrement
(define-macro (-- x) `(##fixnum.- ,x 1))

;==============================================================================
; Some useful control operators

; if condition is true, execute stmts in turn
; and return the result of the last statement
; otherwise, return #f
(define-macro (when condition . stmts)
  `(and ,condition (begin ,@stmts)))
  

; if condition is false execute stmts in turn
; and return the result of the last statement
; otherwise, return #t
; This primitive is often called 'unless'
(define-macro (whennot condition . stmts)
  `(or ,condition (begin ,@stmts)))


; Execute a sequence of forms and return the
; result of the _first_ one. Like PROG1 in Lisp.
; Typically used to evaluate one or more forms with
; side effects and return a value that must be
; computed before some or all of the side effects
; happen.
(define-macro (begin0 form . forms)
  (let ((var (gensym)))
    `(let ((,var ,form)) ,@forms ,var)))

; Prepend an ITEM to a LIST, like a Lisp macro PUSH
; an ITEM can be an expression, but ls must be a VAR
(define-macro (push! item ls)
  `(set! ,ls (cons ,item ,ls)))

; Is str the empty string?
; string-null? str -> bool
; See Olin Shiver's Underground String functions
(define-macro (string-null? str) `(zero? (string-length ,str)))


;==============================================================================
; Support for multiple-values and let-values* form
; Multiple values are not present natively in Gambit.
; What follows is an _approximation_: it is not very good in case
; of continuations captured while evaluating an argument expression of
; values. Note that the only meaningful way to use 'values' procedure is
; in conjunction with call-with-values or let-values*

; The next 2 functions are now supported natively in Gambit 4
;
;(define values list)
;
;(define (call-with-values producer consumer)
;  (apply consumer (producer)))
;
;; Like let* but allowing for multiple-value bindings
;(define-macro (let-values* bindings . body)
;  (if (null? bindings) (cons 'begin body)
;      (apply (lambda (vars initializer)
;	 (let ((cont 
;		(cons 'let-values* 
;		      (cons (cdr bindings) body))))
;	   (cond
;	    ((not (pair? vars))		; regular let case, a single var
;	     `(let ((,vars ,initializer)) ,cont))
;	    ((null? (cdr vars))		; single var, see the prev case
;	     `(let ((,(car vars) ,initializer)) ,cont))
;	    ((null? (cddr vars))	; two variables
;	     (let ((val (gensym)))
;	       `(let* ((,val ,initializer)
;		       (,(car vars) (car ,val))
;		       (,(cadr vars) (cadr ,val))) ,cont)))
;	   (else			; the most generic case
;	    `(apply (lambda ,vars ,cont) ,initializer)))))
;       (car bindings))))



;==============================================================================
; assoc-primitives with a default clause
; If the search in the assoc list fails, the
; default action argument is returned. If this
; default action turns out to be a thunk,
; the result of its evaluation is returned.
; If the default action is not given, an error
; is signaled

(define-macro (assq-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assq key '" ,key "' in a list " ,alist)
          (let ((defact-symb (gensym)))
	    `(let ((,defact-symb ,(car default-action-arg)))
               (if (procedure? ,defact-symb) (,defact-symb) ,defact-symb))))))
    `(or (assq ,key ,alist) ,default-action)))

(define-macro (assv-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assv key '" ,key "' in a list " ,alist)
          (let ((defact-symb (gensym)))
	    `(let ((,defact-symb ,(car default-action-arg)))
               (if (procedure? ,defact-symb) (,defact-symb) ,defact-symb))))))
    `(or (assv ,key ,alist) ,default-action)))

(define-macro (assoc-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assoc key '" ,key "' in a list " ,alist)
          (let ((defact-symb (gensym)))
	    `(let ((,defact-symb ,(car default-action-arg)))
               (if (procedure? ,defact-symb) (,defact-symb) ,defact-symb))))))
    `(or (assoc ,key ,alist) ,default-action)))


;==============================================================================
; Convenience macros to avoid quoting of symbols
; being deposited/looked up in the environment
(define-macro (env.find key) `(%%env.find ',key))
(define-macro (env.demand key) `(%%env.demand ',key))
(define-macro (env.bind key value) `(%%env.bind ',key ,value))

;==============================================================================
; Implementation of SRFI-0
; Only feature-identifiers srfi-0 and gambit
; assumed predefined
(define-macro (cond-expand . clauses)
  (define feature-ids '(gambit srfi-0))
  (define (feature-req-satisfies? fr) ; does feature-request satisfies?
    (cond
     ((memq fr feature-ids) #t)
     ((not (pair? fr)) #f)
     ((eq? 'and (car fr))
      (let loop ((clauses (cdr fr)))
	(or (null? clauses)
	    (and (feature-req-satisfies? (car clauses))
		 (loop (cdr clauses))))))
     ((eq? 'or (car fr))
      (let loop ((clauses (cdr fr)))
	(and (pair? clauses)
	     (or (feature-req-satisfies? (car clauses))
		 (loop (cdr clauses))))))
     ((eq? 'not (car fr))
      (not (feature-req-satisfies? (and (pair? (cdr fr)) (cadr fr)))))
     (else #f)))
  (let loop ((clauses clauses))
    (if (null? clauses) '(error "Unfulfilled cond-expand")
	(let* ((feature-req (if (pair? (car clauses)) (caar clauses)
				(error "<cond-expand clause> is not a list")))
	       (cmd-or-defs* (cons 'begin (cdar clauses))))
	  (cond
	   ((and (eq? 'else feature-req) (null? (cdr clauses)))
	    cmd-or-defs*)
	   ((feature-req-satisfies? feature-req)
	    cmd-or-defs*)
	   (else (loop (cdr clauses))))))))


;==============================================================================
; DL: taken from previous versions of "myenv.scm"

; DL: aliases for ++ and --
(define-macro (inc x) `(##fixnum.+ 1 ,x))
(define-macro (dec x) `(##fixnum.- ,x 1))
