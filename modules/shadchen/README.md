Shadchen
--------

Now for Gambit-c/Lambdanative!

Shadchen is a simple, extensible pattern matcher now for Gambit-c, Common Lisp and Emacs Lisp.  A very similar matcher is built into Gazelle, a Lisp for Javascript.  Once you get used to programming with a pattern matcher it is hard to go back.

Shadchen provides basic matching support for Scheme data structures and is _extensible_, in that the application programmer can declare and use custom patterns to express arbitrary matches.

Example Usage
-------------

    (match 'x
     (('x 'matched-literal-x)))      
    
    (define (reverse3 seq)       
     (match seq
      ((list a b c) (list c b a))
      ((vector a b c) (list c b a))))      

    ;; or equivalently:        

    (define (reverse3 seq)       
     (match seq 		 
      ((or (vector a b c) (list a b c)) (list c b a)))) 


Documentation
-------------

### match 

    (match <expr> ((<pattern1> <body1> ...) ...))

match evaluates expression `<expr>` and then attempts to match that value against each of the patterns `<pattern1>` through `<patternN>`.  When a matching pattern is encountered, the `<bodyK>` associated with that pattern is evaluated and the value of the last expression is returned.  No further matches are attempted, and no other bodies execute.

If no patterns match, an error is thrown indicating the value and forms which failed to produce a match.

### define-pattern

    (define-pattern (<name> <arg1> ...) <body1> ...)

Define a new pattern.  User defined patterns must expand to a pattern expression defined by other user defined patterns or primitive types.  The result must, therefore, be an s-expression and so patterns are similar to macro definitions.  

For instance, here is the pattern definition for matching against cons cells:

    (define-pattern (pair cr cd)
      `(and (? pair?)
            (call (lambda (pair)
                    (list (car pair) (cdr pair)))
                  (list ,cr ,cd))))


The result of evaluating the body of this expression is a new pattern expression.

### Built-in patterns

`<symbol>` 

A symbol matches any expression and binds the value to that symbol.

eg: `(match 10 ((x x))) -> 10`.

`<string-literal>`

A string literal matches exactly that string and binds no values.

`<numeric-literal>` 

A literal number matches anything to which it is `=`

`<keyword-literal>`

Matches exactly that keyword. 

`(list sub-pattern1 <and-N-more-sub-patterns>>)`

Matches a list of length `N` if each sub-pattern matches the associated element of the list.  Binds those bindings implied by all sub-patterns.

`(list sub-pattern1 <and-N-more-sub-patterns> sub-pattern-N ...)`

Matches a list with at least N-1 elements, and matches <sub-pattern-N> against the umatched tail of the list.  Can match if the tail is empty.  Binds exactly the bindings implied by the sub-patterns.

`(vector <patterns...>)`

Exactly the same as `list` matching.

`(? predicate <pattern1> ...)`

Matches if `predicate` is true on the value and all patterns also match.  Binds the bindings of the patterns.

`(call function <pattern1> ...)`

Transforms the value to be matched via function and then matches the patterns.  Binds the implied bindings of the sub-patterns. 

`(call* function <pattern1> ...)`

As in `call` except that if function returns `*match-fail*` the match fails.  This allows you to drop out of the pattern matcher and place arbitrary modification and matching.  A common pattern is to use `call*` to both detect match failures and transform a value in preparation for subsequent matches.  For instance, we could write a match against `cons` cells like this:

   (define-pattern (cons-cell car-pattern cdr-pattern)
    `(call* (lambda (object)
             (if (pair? object) (list (car object) (cdr object))
                 *match-fail*))
            (list ,car-pattern ,cdr-pattern)))

`(or sub-patterns ...)`

Matches any of the `sub-patterns`.  Given the nature of pattern matching, each sub-pattern must result in the same set of bound values.  This is checked statically, and it is a macro-expansion time error to try to `or` with disjoint patterns.

`(and sub-patterns)` 

Matches ALL of the `sub-patterns` or fails.  Sub patterns do not need to bind the same symbols (and in general will not).  

`(let bindings ...)`

Always succeeds and binds the implied values/name pairs in bindings, like a let expression.

eg:
    
    `(match (some-value) ((let (x 10) (y 11)) (list x y)))`

returns `(10 11)`, regardless of what `(some-value)` evaluates to.  A combination of `or` and `let` can allow you to provide default bindings in the case of what would otherwise be a failed match.
