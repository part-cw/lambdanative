(include "./shadchen.scm")

(define (display-nl #!rest args)
  (display args)
  (newline))

(define-macro (test name predicate expected expr)
  (let ((predicate-name (gensym 'predicate))
		(test-result-name (gensym 'test-result))
		(expected-name (gensym 'expected))
		(expr-name (gensym 'expr))
		(name-name (gensym 'name)))
	`(let ((,expr-name ,expr)
		   (,expected-name ,expected)
		   (,predicate-name ,predicate)
		   (,name-name ,name))
	   (if (,predicate-name ,expected-name ,expr-name)
		   (display-nl "success: ",name-name )
		   (display-nl "FAIL: expected " ,expected-name " got " ,expr-name " which are not equal under " ,predicate)))))

(test "Intentionally fails"
	  equal?
	  '(a b c)
	  '(x y z))

(test "Let inside match expression" 
	  equal? 10 
	  (match 'x
			 ((let (y 10)) y)))

(test "pattern-bound-symbols returns correct symbols for let"
	  sets-equal? 
      '(x y)
	  (pattern-bound-symbols '(let (x 10) (y 11))))

(test "pattern-bound-symbols returns correct symbols for `define-pattern` style matches"
	  sets-equal?
	  '(x y)
	  (pattern-bound-symbols '(vector x y)))

(test "pattern-bound-symbols returns correct symbols for complex match"
	  sets-equal?
	  '(x y)
	  (pattern-bound-symbols '(and (? vector?)
								   (call vector->list (list x y)))))

(test "default values for match via or and let"
	  equal?
	  '(10 11)
	  (match (list 1 2)
			 ((or (vector x y) (let (x 10) (y 11)))
			  (list x y))))

(test "default values not bound in case of success in or/let pattern"
	  equal? 
	  '(1 2)
	  (match (vector 1 2)
			 ((or (vector x y) (let (x 10) (y 11)))
			  (list x y))))

(test "false match"
	  equal?
	  matched:
	  (match #f
			 ((or #f #t)
			  matched:)))
