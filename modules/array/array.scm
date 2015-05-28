;;;;"array.scm" Arrays for Scheme
; Copyright (C) 2001, 2003, 2005, 2006 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;@code{(require 'array)} or @code{(require 'srfi-63)}
;;@ftindex array


(##namespace ("array#"))
(##include "~~lib/gambit#.scm")
(##include "array#.scm")
(##namespace ("" log-error))

(define (has-duplicates? lst)
  (cond ((null? lst) #f)
        ((member (car lst) (cdr lst)) #t)
        (else (has-duplicates? (cdr lst)))))

(define (position obj lst)
  (define pos (lambda (n lst)
                (cond ((null? lst) #f)
                      ((eqv? obj (car lst)) n)
                      (else (pos (+ 1 n) (cdr lst))))))
  (pos 0 lst))

(define (every pred lst . rest)
  (cond ((null? rest)
         (let mapf ((lst lst))
           (or (null? lst)
               (and (pred (car lst)) (mapf (cdr lst))))))
        (else (let mapf ((lst lst) (rest rest))
                (or (null? lst)
                    (and (apply pred (car lst) (map car rest))
                         (mapf (cdr lst) (map cdr rest))))))))

(define (notevery pred . ls) (not (apply every pred ls)))

(include "record.scm")
(include "mulapply.scm")

(define array:rtd
  (make-record-type "array"
		    '(dimensions
		      scales		;list of dimension scales
		      offset		;exact integer
		      store		;data
		      )))

(define array:dimensions
  (let ((dimensions (record-accessor array:rtd 'dimensions)))
    (lambda (array)
      (cond ((vector? array) (list (vector-length array)))
	    ((string? array) (list (string-length array)))
	    (else (dimensions array))))))

(define array:scales
  (let ((scales (record-accessor array:rtd 'scales)))
    (lambda (obj)
      (cond ((string? obj) '(1))
	    ((vector? obj) '(1))
	    (else (scales obj))))))

(define array:store
  (let ((store (record-accessor array:rtd 'store)))
    (lambda (obj)
      (cond ((string? obj) obj)
	    ((vector? obj) obj)
	    (else (store obj))))))

(define array:offset
  (let ((offset (record-accessor array:rtd 'offset)))
    (lambda (obj)
      (cond ((string? obj) 0)
	    ((vector? obj) 0)
	    (else (offset obj))))))

(define array:construct
  (record-constructor array:rtd '(dimensions scales offset store)))

;;@args obj
;;Returns @code{#t} if the @1 is an array, and @code{#f} if not.
(define array?
  (let ((array:array? (record-predicate array:rtd)))
    (lambda (obj) (or (string? obj) (vector? obj) (array:array? obj)))))

;;@noindent
;;@emph{Note:} Arrays are not disjoint from other Scheme types.
;;Vectors and possibly strings also satisfy @code{array?}.
;;A disjoint array predicate can be written:
;;
;;@example
;;(define (strict-array? obj)
;;  (and (array? obj) (not (string? obj)) (not (vector? obj))))
;;@end example

;;@body
;;Returns @code{#t} if @1 and @2 have the same rank and dimensions and the
;;corresponding elements of @1 and @2 are @code{equal?}.

;;@body
;;@0 recursively compares the contents of pairs, vectors, strings, and
;;@emph{arrays}, applying @code{eqv?} on other objects such as numbers
;;and symbols.  A rule of thumb is that objects are generally @0 if
;;they print the same.  @0 may fail to terminate if its arguments are
;;circular data structures.
;;
;;@example
;;(equal? 'a 'a)                             @result{}  #t
;;(equal? '(a) '(a))                         @result{}  #t
;;(equal? '(a (b) c)
;;        '(a (b) c))                        @result{}  #t
;;(equal? "abc" "abc")                       @result{}  #t
;;(equal? 2 2)                               @result{}  #t
;;(equal? (make-vector 5 'a)
;;        (make-vector 5 'a))                @result{}  #t
;;(equal? (make-array (A:fixN32b 4) 5 3)
;;        (make-array (A:fixN32b 4) 5 3))    @result{}  #t
;;(equal? (make-array '#(foo) 3 3)
;;        (make-array '#(foo) 3 3))          @result{}  #t
;;(equal? (lambda (x) x)
;;        (lambda (y) y))                    @result{}  @emph{unspecified}
;;@end example
(define (equal? obj1 obj2)
  (cond ((eqv? obj1 obj2) #t)
	((or (pair? obj1) (pair? obj2))
	 (and (pair? obj1) (pair? obj2)
	      (equal? (car obj1) (car obj2))
	      (equal? (cdr obj1) (cdr obj2))))
	((and (string? obj1) (string? obj2))
	 (string=? obj1 obj2))
	((and (vector? obj1) (vector? obj2))
	 (and (equal? (vector-length obj1) (vector-length obj2))
	      (do ((idx (+ -1 (vector-length obj1)) (+ -1 idx)))
		  ((or (negative? idx)
		       (not (equal? (vector-ref obj1 idx)
				    (vector-ref obj2 idx))))
		   (negative? idx)))))
	((and (array? obj1) (array? obj2))
	 (and (equal? (array:dimensions obj1) (array:dimensions obj2))
	      (letrec ((rascan
			(lambda (dims idxs)
			  (if (null? dims)
			      (equal? (apply array-ref obj1 idxs)
				      (apply array-ref obj2 idxs))
			      (do ((res #t (rascan (cdr dims) (cons idx idxs)))
				   (idx (+ -1 (car dims)) (+ -1 idx)))
				  ((or (not res) (negative? idx)) res))))))
		(rascan (reverse (array:dimensions obj1)) '()))))
	(else #f)))

;;@body
;;Returns the number of dimensions of @1.  If @1 is not an array, 0 is
;;returned.
(define (array-rank obj)
  (if (array? obj) (length (array:dimensions obj)) 0))

;;@args array
;;Returns a list of dimensions.
;;
;;@example
;;(array-dimensions (make-array '#() 3 5))
;;   @result{} (3 5)
;;@end example
(define array-dimensions array:dimensions)

;;@args prototype k1 @dots{}
;;
;;Creates and returns an array of type @1 with dimensions @2, @dots{}
;;and filled with elements from @1.  @1 must be an array, vector, or
;;string.  The implementation-dependent type of the returned array
;;will be the same as the type of @1; except if that would be a vector
;;or string with rank not equal to one, in which case some variety of
;;array will be returned.
;;
;;If the @1 has no elements, then the initial contents of the returned
;;array are unspecified.  Otherwise, the returned array will be filled
;;with the element at the origin of @1.
(define (make-array prototype . dimensions)
  (define prot (array:store prototype))
  (define pdims (array:dimensions prototype))
  (define onedim? (eqv? 1 (length dimensions)))
  (define tcnt (apply * dimensions))
  (let ((initializer
	 (if (zero? (apply * pdims)) '()
	     (list
	      (apply array-ref
		     prototype
		     (map (lambda (x) 0) pdims))))))
    (cond ((and onedim? (string? prot))
	   (apply make-string (car dimensions) initializer))
	  ((and onedim? (vector? prot))
	   (apply make-vector (car dimensions) initializer))
	  (else
	   (let ((store
		  (if (string? prot)
		      (apply make-string tcnt initializer)
		      (apply make-vector tcnt initializer))))
	     (define (loop dims scales)
	       (if (null? dims)
		   (array:construct dimensions (cdr scales) 0 store)
		   (loop (cdr dims)
			 (cons (* (car dims) (car scales)) scales))))
	     (loop (reverse dimensions) '(1)))))))
;;@args prototype k1 @dots{}
;;@0 is an alias for @code{make-array}.
(define create-array make-array)

;;@args array mapper k1 @dots{}
;;@0 can be used to create shared subarrays of other
;;arrays.  The @var{mapper} is a function that translates coordinates in
;;the new array into coordinates in the old array.  A @var{mapper} must be
;;linear, and its range must stay within the bounds of the old array, but
;;it can be otherwise arbitrary.  A simple example:
;;
;;@example
;;(define fred (make-array '#(#f) 8 8))
;;(define freds-diagonal
;;  (make-shared-array fred (lambda (i) (list i i)) 8))
;;(array-set! freds-diagonal 'foo 3)
;;(array-ref fred 3 3)
;;   @result{} FOO
;;(define freds-center
;;  (make-shared-array fred (lambda (i j) (list (+ 3 i) (+ 3 j)))
;;                     2 2))
;;(array-ref freds-center 0 0)
;;   @result{} FOO
;;@end example
(define (make-shared-array array mapper . dimensions)
  (define odl (array:scales array))
  (define rank (length dimensions))
  (define shape
    (map (lambda (dim) (if (list? dim) dim (list 0 (+ -1 dim)))) dimensions))
  (do ((idx (+ -1 rank) (+ -1 idx))
       (uvt (if (zero? rank)
		'()
		(append (cdr (vector->list (make-vector rank 0))) '(1)))
	    (append (cdr uvt) '(0)))
       (uvts '() (cons uvt uvts)))
      ((negative? idx)
       (let ((ker0 (apply + (map * odl (apply mapper uvt)))))
	 (array:construct
	  (map (lambda (dim) (+ 1 (- (cadr dim) (car dim)))) shape)
	  (map (lambda (uvt) (- (apply + (map * odl (apply mapper uvt))) ker0))
	       uvts)
	  (apply +
		 (array:offset array)
		 (map * odl (apply mapper (map car shape))))
	  (array:store array))))))

;;@args rank proto list
;;@3 must be a rank-nested list consisting of all the elements, in
;;row-major order, of the array to be created.
;;
;;@0 returns an array of rank @1 and type @2 consisting of all the
;;elements, in row-major order, of @3.  When @1 is 0, @3 is the lone
;;array element; not necessarily a list.
;;
;;@example
;;(list->array 2 '#() '((1 2) (3 4)))
;;                @result{} #2A((1 2) (3 4))
;;(list->array 0 '#() 3)
;;                @result{} #0A 3
;;@end example
(define (list->array rank proto lst)
  (define dimensions
    (do ((shp '() (cons (length row) shp))
	 (row lst (car lst))
	 (rnk (+ -1 rank) (+ -1 rnk)))
	((negative? rnk) (reverse shp))))
  (let ((nra (apply make-array proto dimensions)))
    (define (l2ra dims idxs row)
      (cond ((null? dims)
	     (apply array-set! nra row (reverse idxs)))
	    ((if (not (eqv? (car dims) (length row)))
		 (log-error 'list->array
			     'non-rectangular 'array dims dimensions))
	     (do ((idx 0 (+ 1 idx))
		  (row row (cdr row)))
		 ((>= idx (car dims)))
	       (l2ra (cdr dims) (cons idx idxs) (car row))))))
    (l2ra dimensions '() lst)
    nra))

;;@args array
;;Returns a rank-nested list consisting of all the elements, in
;;row-major order, of @1.  In the case of a rank-0 array, @0 returns
;;the single element.
;;
;;@example
;;(array->list #2A((ho ho ho) (ho oh oh)))
;;                @result{} ((ho ho ho) (ho oh oh))
;;(array->list #0A ho)
;;                @result{} ho
;;@end example
(define (array->list ra)
  (define (ra2l dims idxs)
    (if (null? dims)
	(apply array-ref ra (reverse idxs))
	(do ((lst '() (cons (ra2l (cdr dims) (cons idx idxs)) lst))
	     (idx (+ -1 (car dims)) (+ -1 idx)))
	    ((negative? idx) lst))))
  (ra2l (array:dimensions ra) '()))

;;@args vect proto dim1 @dots{}
;;@1 must be a vector of length equal to the product of exact
;;nonnegative integers @3, @dots{}.
;;
;;@0 returns an array of type @2 consisting of all the elements, in
;;row-major order, of @1.  In the case of a rank-0 array, @1 has a
;;single element.
;;
;;@example
;;(vector->array #(1 2 3 4) #() 2 2)
;;                @result{} #2A((1 2) (3 4))
;;(vector->array '#(3) '#())
;;                @result{} #0A 3
;;@end example
(define (vector->array vect prototype . dimensions)
  (define vdx (vector-length vect))
  (if (not (eqv? vdx (apply * dimensions)))
      (log-error 'vector->array vdx '<> (cons '* dimensions)))
  (let ((ra (apply make-array prototype dimensions)))
    (define (v2ra dims idxs)
      (cond ((null? dims)
	     (set! vdx (+ -1 vdx))
	     (apply array-set! ra (vector-ref vect vdx) (reverse idxs)))
	    (else
	     (do ((idx (+ -1 (car dims)) (+ -1 idx)))
		 ((negative? idx) vect)
	       (v2ra (cdr dims) (cons idx idxs))))))
    (v2ra dimensions '())
    ra))

;;@args array
;;Returns a new vector consisting of all the elements of @1 in
;;row-major order.
;;
;;@example
;;(array->vector #2A ((1 2)( 3 4)))
;;                @result{} #(1 2 3 4)
;;(array->vector #0A ho)
;;                @result{} #(ho)
;;@end example
(define (array->vector ra)
  (define dims (array:dimensions ra))
  (let* ((vdx (apply * dims))
	 (vect (make-vector vdx)))
    (define (ra2v dims idxs)
      (if (null? dims)
	  (let ((val (apply array-ref ra (reverse idxs))))
	    (set! vdx (+ -1 vdx))
	    (vector-set! vect vdx val))
	  (do ((idx (+ -1 (car dims)) (+ -1 idx)))
	      ((negative? idx) vect)
	    (ra2v (cdr dims) (cons idx idxs)))))
    (ra2v dims '())
    vect))

(define (array:in-bounds? array indices)
  (do ((bnds (array:dimensions array) (cdr bnds))
       (idxs indices (cdr idxs)))
      ((or (null? bnds)
	   (null? idxs)
	   (not (integer? (car idxs)))
	   (not (< -1 (car idxs) (car bnds))))
       (and (null? bnds) (null? idxs)))))

;;@args array index1 @dots{}
;;Returns @code{#t} if its arguments would be acceptable to
;;@code{array-ref}.
(define (array-in-bounds? array . indices)
  (array:in-bounds? array indices))

;;@args array k1 @dots{}
;;Returns the (@2, @dots{}) element of @1.
(define (array-ref array . indices)
  (define store (array:store array))
  (or (array:in-bounds? array indices)
      (log-error 'array-ref 'bad-indices indices))
  ((if (string? store) string-ref vector-ref)
   store (apply + (array:offset array) (map * (array:scales array) indices))))

;;@args array obj k1 @dots{}
;;Stores @2 in the (@3, @dots{}) element of @1.  The value returned
;;by @0 is unspecified.
(define (array-set! array obj . indices)
  (define store (array:store array))
  (or (array:in-bounds? array indices)
      (log-error 'array-set! 'bad-indices indices))
  ((if (string? store) string-set! vector-set!)
   store (apply + (array:offset array) (map * (array:scales array) indices))
   obj))

;;@noindent
;;These functions return a prototypical uniform-array enclosing the
;;optional argument (which must be of the correct type).  If the
;;uniform-array type is supported by the implementation, then it is
;;returned; defaulting to the next larger precision type; resorting
;;finally to vector.

(define (make-prototype-checker name pred? creator)
  (lambda args
    (case (length args)
      ((1) (if (pred? (car args))
	       (creator (car args))
	       (log-error name 'incompatible 'type (car args))))
      ((0) (creator))
      (else (log-error name 'wrong 'number 'of 'args args)))))

(define (integer-bytes?? n)
  (lambda (obj)
    (and (integer? obj)
	 (exact? obj)
	 (or (negative? n) (not (negative? obj)))
	 (do ((num obj (quotient num 256))
	      (n (+ -1 (abs n)) (+ -1 n)))
	     ((or (zero? num) (negative? n))
	      (zero? num))))))

;;@defun A:floC128b z
;;@defunx A:floC128b
;;Returns an inexact 128.bit flonum complex uniform-array prototype.
;;@end defun
(define A:floC128b (make-prototype-checker 'A:floC128b complex? vector))
;;@defun A:floC64b z
;;@defunx A:floC64b
;;Returns an inexact 64.bit flonum complex uniform-array prototype.
;;@end defun
(define A:floC64b (make-prototype-checker 'A:floC64b complex? vector))
;;@defun A:floC32b z
;;@defunx A:floC32b
;;Returns an inexact 32.bit flonum complex uniform-array prototype.
;;@end defun
(define A:floC32b (make-prototype-checker 'A:floC32b complex? vector))
;;@defun A:floC16b z
;;@defunx A:floC16b
;;Returns an inexact 16.bit flonum complex uniform-array prototype.
;;@end defun
(define A:floC16b (make-prototype-checker 'A:floC16b complex? vector))

;;@defun A:floR128b x
;;@defunx A:floR128b
;;Returns an inexact 128.bit flonum real uniform-array prototype.
;;@end defun
(define A:floR128b (make-prototype-checker 'A:floR128b real? vector))
;;@defun A:floR64b x
;;@defunx A:floR64b
;;Returns an inexact 64.bit flonum real uniform-array prototype.
;;@end defun
(define A:floR64b (make-prototype-checker 'A:floR64b real? vector))
;;@defun A:floR32b x
;;@defunx A:floR32b
;;Returns an inexact 32.bit flonum real uniform-array prototype.
;;@end defun
(define A:floR32b (make-prototype-checker 'A:floR32b real? vector))
;;@defun A:floR16b x
;;@defunx A:floR16b
;;Returns an inexact 16.bit flonum real uniform-array prototype.
;;@end defun
(define A:floR16b (make-prototype-checker 'A:floR16b real? vector))

;;@defun A:floR128d q
;;@defunx A:floR128d
;;Returns an exact 128.bit decimal flonum rational uniform-array prototype.
;;@end defun
(define A:floR128d (make-prototype-checker 'A:floR128d real? vector))
;;@defun A:floR64d q
;;@defunx A:floR64d
;;Returns an exact 64.bit decimal flonum rational uniform-array prototype.
;;@end defun
(define A:floR64d (make-prototype-checker 'A:floR64d real? vector))
;;@defun A:floR32d q
;;@defunx A:floR32d
;;Returns an exact 32.bit decimal flonum rational uniform-array prototype.
;;@end defun
(define A:floR32d (make-prototype-checker 'A:floR32d real? vector))

;;@defun A:fixZ64b n
;;@defunx A:fixZ64b
;;Returns an exact binary fixnum uniform-array prototype with at least
;;64 bits of precision.
;;@end defun
(define A:fixZ64b (make-prototype-checker 'A:fixZ64b (integer-bytes?? -8) vector))
;;@defun A:fixZ32b n
;;@defunx A:fixZ32b
;;Returns an exact binary fixnum uniform-array prototype with at least
;;32 bits of precision.
;;@end defun
(define A:fixZ32b (make-prototype-checker 'A:fixZ32b (integer-bytes?? -4) vector))
;;@defun A:fixZ16b n
;;@defunx A:fixZ16b
;;Returns an exact binary fixnum uniform-array prototype with at least
;;16 bits of precision.
;;@end defun
(define A:fixZ16b (make-prototype-checker 'A:fixZ16b (integer-bytes?? -2) vector))
;;@defun A:fixZ8b n
;;@defunx A:fixZ8b
;;Returns an exact binary fixnum uniform-array prototype with at least
;;8 bits of precision.
;;@end defun
(define A:fixZ8b (make-prototype-checker 'A:fixZ8b (integer-bytes?? -1) vector))

;;@defun A:fixN64b k
;;@defunx A:fixN64b
;;Returns an exact non-negative binary fixnum uniform-array prototype with at
;;least 64 bits of precision.
;;@end defun
(define A:fixN64b (make-prototype-checker 'A:fixN64b (integer-bytes?? 8) vector))
;;@defun A:fixN32b k
;;@defunx A:fixN32b
;;Returns an exact non-negative binary fixnum uniform-array prototype with at
;;least 32 bits of precision.
;;@end defun
(define A:fixN32b (make-prototype-checker 'A:fixN32b (integer-bytes?? 4) vector))
;;@defun A:fixN16b k
;;@defunx A:fixN16b
;;Returns an exact non-negative binary fixnum uniform-array prototype with at
;;least 16 bits of precision.
;;@end defun
(define A:fixN16b (make-prototype-checker 'A:fixN16b (integer-bytes?? 2) vector))
;;@defun A:fixN8b k
;;@defunx A:fixN8b
;;Returns an exact non-negative binary fixnum uniform-array prototype with at
;;least 8 bits of precision.
;;@end defun
(define A:fixN8b (make-prototype-checker 'A:fixN8b (integer-bytes?? 1) vector))

;;@defun A:bool bool
;;@defunx A:bool
;;Returns a boolean uniform-array prototype.
;;@end defun
(define A:bool (make-prototype-checker 'A:bool boolean? vector))
