#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; vector-map 
(define (vector-map fcn v1 . vn)
  (if (null? vn)
    (let ((len (vector-length v1)))
      (vector:map1! fcn (make-vector len) v1 len))
    (let ((len (vector:smallest-length vn (vector-length v1))))
      (vector:map2+! fcn (make-vector len) (append (list v1) vn) len))
  ))

(define (vector:map1! fcn target v1 len)
  (if (zero? len)
    target
    (let ((i (fx- len 1)))
      (vector-set! target i (fcn (vector-ref v1 i)))
      (vector:map1! fcn target v1 (fx- len 1))
    )
  ))

(define (vector:map2+! fcn target vn len)
  (if (zero? len)
    target
    (let ((i (fx- len 1)))
      (vector-set! target i (apply fcn (map (lambda (v) (vector-ref v i)) vn)))
      (vector:map2+! fcn target vn (fx- len 1))
    )
  ))

(define vector:smallest-length
  (letrec ((loop (lambda (vector-list len)
    (if (null? vector-list)
      len
      (loop (cdr vector-list)
            (min (vector-length (car vector-list)) len))
    ))))
    loop
  ))

;; vector-foldr [faster than using loop when compiled]
(define (vector-foldr fcn start v1)
  (let ((len (vector-length v1)))
   (vector:foldr1 fcn start v1 len)
  ))

(define (vector:foldr1 fcn start v1 len)
  (if (zero? len)
    start
    (let ((i (fx- len 1)))
      (vector:foldr1 fcn (fcn (vector-ref v1 i) start) v1 (fx- len 1))
    )
  ))

;; misc vector operations 
(define (vector-scale vec k) (vector-map (lambda (x) (* k x)) vec))
(define (vector-offset vec k) (vector-map (lambda (x) (+ k x)) vec))
(define (vector-sum vec) (vector-foldr + 0 vec))
(define (vector-product vec) (vector-foldr * 1 vec))
(define (vector-mean vec) (/ (vector-sum vec) (vector-length vec)))

;; Dot product for vectors
(define (vector-dot v1 v2)
  (vector-sum (vector-map * v1 v2)))

;; Diff for vectors
(define (vector-diff vec)
  (let ((len (vector-length vec)))
    (vector-map - (subvector vec 1 len) (subvector vec 0 (fx- len 1)))
  ))

;; Unit tests for vectors
(unit-test "vector-operations" "Vector operations"
  (lambda () (and
    (equal? (vector-scale (vector 1 2 3 4 5) 5) (vector 5 10 15 20 25))
    (equal? (vector-offset (vector 10 20 30 40 50) 10) (vector 20 30 40 50 60))
    (equal? (vector-sum (vector 1 2 3 4 5 6 7 8 9)) 45)
    (equal? (vector-product (vector 1 2 3 4 5)) 120)
    (equal? (vector-mean (vector 1 2 3 4 5)) 3)
  )))
(unit-test "vector-primitives" "Vector primitives"
  (lambda () (and
    (equal? (vector-map + (vector 1 2 3 4) (vector 4 3 2 1)) (vector 5 5 5 5))
    (equal? (vector-foldr / 1 (vector 2 2)) 1)
    (equal? (vector-map * (vector 9 6 3) (vector 1 2 3)) (list->vector (map * (list 9 6 3) (list 1 2 3))))
    (equal? (vector-foldr / 5 (vector 4 3 2 1)) (foldr / 5 (list 4 3 2 1)))
  )))
(unit-test "vector-dot" "Vector dot products"
  (lambda () (and
    (equal? (vector-dot (vector 1 2 3 4) (vector 1 2 3 4)) 30)
    (equal? (vector-dot (vector 9 3 1 4) (vector 1 8 3 4)) (list-dot (list 9 3 1 4) (list 1 8 3 4)))
  )))
(unit-test "vector-diff" "Vector diff"
  (lambda () (and
    (equal? (vector-diff (vector 1 2 3 5)) (vector 1 1 2))
    (equal? (vector-diff (vector 9 3 1 4 1 8 3 4)) (list->vector (list-diff (list 9 3 1 4 1 8 3 4))))
  )))

;;eof
