package LNjScheme;

/** Holds a string representation of some Scheme code in <TT>CODE</tt>.
 * A string is better than a file because with no files, its easier to
 * compress everything in the classes.jar file. For editing convenience,
 * the following two perl convert from normal text to this Java quoted
 * format and back again:
 * <pre>
 * perl -pe 's/"/\\"/g; s/(\s*)(.*?)(\s*)$/\1"\2\\n" +\n/'
 * perl -pe 's/\\"/"/g; s/^(\s*)"/\1/; s/\\n" [+]//'
 * </pre>
 * @author Peter Norvig, peter@norvig.com http://www.norvig.com
 * Copyright 1998 Peter Norvig, see http://www.norvig.com/license.html  **/
public class SchemePrimitives {

  public static final String CODE =
"(define call/cc    call-with-current-continuation)\n" +
"(define first 	   car)\n" +
"(define second     cadr)\n" +
"(define third      caddr)\n" +
"(define rest 	   cdr)\n" +
"(define set-first! set-car!)\n" +
"(define set-rest!  set-cdr!)\n" +

//;;;;;;;;;;;;;;;; Standard Scheme Macros

"(define or\n" +
  "(macro args\n" +
    "(if (null? args)\n" +
	"#f\n" +
	"(cons 'cond (map list args)))))\n" +

"(define and\n" +
  "(macro args\n" +
    "(cond ((null? args) #t)\n" +
	  "((null? (rest args)) (first args))\n" +
	  "(else (list 'if (first args) (cons 'and (rest args)) #f)))))\n" +

"(define quasiquote\n" +
  "(macro (x)\n" +
    "(define (constant? exp)\n" +
      "(if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))\n" +
    "(define (combine-skeletons left right exp)\n" +
      "(cond\n" +
       "((and (constant? left) (constant? right))\n" +
	"(if (and (eqv? (eval left) (car exp))\n" +
		 "(eqv? (eval right) (cdr exp)))\n" +
	    "(list 'quote exp)\n" +
	    "(list 'quote (cons (eval left) (eval right)))))\n" +
       "((null? right) (list 'list left))\n" +
       "((and (pair? right) (eq? (car right) 'list))\n" +
	"(cons 'list (cons left (cdr right))))\n" +
       "(else (list 'cons left right))))\n" +
    "(define (expand-quasiquote exp nesting)\n" +
      "(cond\n" +
       "((vector? exp)\n" +
	"(list 'apply 'vector (expand-quasiquote (vector->list exp) nesting)))\n" +
       "((not (pair? exp))\n" +
	"(if (constant? exp) exp (list 'quote exp)))\n" +
       "((and (eq? (car exp) 'unquote) (= (length exp) 2))\n" +
	"(if (= nesting 0)\n" +
	    "(second exp)\n" +
	    "(combine-skeletons ''unquote\n" +
			       "(expand-quasiquote (cdr exp) (- nesting 1))\n" +
			       "exp)))\n" +
       "((and (eq? (car exp) 'quasiquote) (= (length exp) 2))\n" +
	"(combine-skeletons ''quasiquote\n" +
			   "(expand-quasiquote (cdr exp) (+ nesting 1))\n" +
			   "exp))\n" +
       "((and (pair? (car exp))\n" +
	     "(eq? (caar exp) 'unquote-splicing)\n" +
	     "(= (length (car exp)) 2))\n" +
	"(if (= nesting 0)\n" +
	    "(list 'append (second (first exp))\n" +
		  "(expand-quasiquote (cdr exp) nesting))\n" +
	    "(combine-skeletons (expand-quasiquote (car exp) (- nesting 1))\n" +
			       "(expand-quasiquote (cdr exp) nesting)\n" +
			       "exp)))\n" +
       "(else (combine-skeletons (expand-quasiquote (car exp) nesting)\n" +
				"(expand-quasiquote (cdr exp) nesting)\n" +
				"exp))))\n" +
    "(expand-quasiquote x 0)))\n" +

"\n" +
"(define let\n" +
  "(macro (bindings . body)\n" +
    "(define (named-let name bindings body)\n" +
      "`(let ((,name #f))\n" +
	 "(set! ,name (lambda ,(map first bindings) . ,body))\n" +
	 "(,name . ,(map second bindings))))\n" +
    "(if (symbol? bindings)\n" +
	"(named-let bindings (first body) (rest body))\n" +
	"`((lambda ,(map first bindings) . ,body) . ,(map second bindings)))))\n" +

"(define let*\n" +
  "(macro (bindings . body)\n" +
    "(if (null? bindings) `((lambda () . ,body))\n" +
	"`(let (,(first bindings))\n" +
	   "(let* ,(rest bindings) . ,body)))))\n" +

"(define letrec\n" +
  "(macro (bindings . body)\n" +
    "(let ((vars (map first bindings))\n" +
	  "(vals (map second bindings)))\n" +
    "`(let ,(map (lambda (var) `(,var #f)) vars)\n" +
       ",@(map (lambda (var val) `(set! ,var ,val)) vars vals)\n" +
       ". ,body))))\n" +

"(define case\n" +
  "(macro (exp . cases)\n" +
    "(define (do-case case)\n" +
      "(cond ((not (pair? case)) (error \"bad syntax in case\" case))\n" +
	    "((eq? (first case) 'else) case)\n" +
	    "(else `((member __exp__ ',(first case)) . ,(rest case)))))\n" +
    "`(let ((__exp__ ,exp)) (cond . ,(map do-case cases)))))\n" +

"(define do\n" +
  "(macro (bindings test-and-result . body)\n" +
    "(let ((variables (map first bindings))\n" +
	  "(inits (map second bindings))\n" +
	  "(steps (map (lambda (clause)\n" +
			"(if (null? (cddr clause))\n" +
			    "(first clause)\n" +
			    "(third clause)))\n" +
		      "bindings))\n" +
	  "(test (first test-and-result))\n" +
	  "(result (rest test-and-result)))\n" +
      "`(letrec ((__loop__\n" +
		 "(lambda ,variables\n" +
		   "(if ,test\n" +
		       "(begin . ,result)\n" +
		       "(begin\n" +
			 ",@body\n" +
			 "(__loop__ . ,steps))))))\n" +
	 "(__loop__ . ,inits)))))\n" +

"(define delay\n" +
  "(macro (exp)\n" +
    "(define (make-promise proc)\n" +
      "(let ((result-ready? #f)\n" +
	    "(result #f))\n" +
	"(lambda ()\n" +
	  "(if result-ready?\n" +
	      "result\n" +
	      "(let ((x (proc)))\n" +
		"(if result-ready?\n" +
		    "result\n" +
		    "(begin (set! result-ready? #t)\n" +
			   "(set! result x)\n" +
			   "result)))))))\n" +
    "`(,make-promise (lambda () ,exp))))\n" +

//;;;;;;;;;;;;;;;; Extensions

"(define time\n" +
  "(macro (exp . rest) `(time-call (lambda () ,exp) . ,rest)))\n"
;
}
