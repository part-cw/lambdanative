#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2013, University of British Columbia
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
;; various extra math functions

;; hyperbolics

(define sinh (lambda (x) (/ (- (exp x) (exp (- x))) 2.0)))

(define cosh (lambda (x) (/ (+ (exp x) (exp (- x))) 2.0)))

(define tanh (lambda (x) (/ (- (exp x) (exp (- x))) (+ (exp x) (exp (- x))))))

(define coth (lambda (x) (/ (+ (exp x) (exp (- x))) (- (exp x) (exp (- x))))))

(define sech (lambda (x) (/ 2.0 (+ (exp x) (exp (- x))))))

(define csch (lambda (x) (/ 2.0 (- (exp x) (exp (- x))))))

(define asinh (lambda (x) (log (+ x (sqrt (+ (* x x) 1.))))))

(define acosh (lambda (x) (log (+ x (sqrt (- (* x x) 1.))))))

(define atanh (lambda (x) (* 0.5 (log (/ (+ 1. x) (- 1. x))))))

(define acoth (lambda (x) (* 0.5 (log (/ (+ x 1.) (- x 1.))))))

(define asech (lambda (x) (log (+ (/ 1. x) (sqrt (- (/ 1. (* x x)) 1.))))))

(define acsch (lambda (x) (log (+ (/ 1. x) (sqrt (+ (/ 1. (* x x)) 1.))))))

;; trigonometrics

(define cot (lambda (x) (* +1.0i (/ (+ (exp (* +1.0i x)) (exp (* +1.0i x -1.0)))
                                      (- (exp (* +1.0i x)) (exp (* +1.0i x -1.0)))))))
(define sec (lambda (x) (/ 2. (+ (exp (* +1.0i x)) (exp (* +1.0i x -1.0))))))

(define csc (lambda (x) (/ 2. (- (exp (* +1.0i x)) (exp (* +1.0i x -1.0))))))

(define acsc (lambda (x) (asin (/ 1. x))))

(define asec (lambda (x) (acos (/ 1. x))))

(define acot (lambda (x) (atan (/ 1. x))))

;; misc stuff

;;; logarithms logb: log base b
(define (logb x b)  (/ (log x) (log b)))

(define (log10 x)   (/ (log x) (log 10)))

(define (lg x)      (/ (log x) (log 2)))

(define (10log10 x) (* 10 (log10 x)))

(define (20log10 x) (* 20 (log10 x)))

;;; sigmoid function
(define (logsig x)  (/ 1 (+ 1 (exp (- x)))))

;;; sign function
(define (sign x)
  (cond
    ((positive? x)  1)
    ((negative? x) -1)
    (else 0)
  ))

;;; inverse
(define (inverse x)  (/ 1 x))

;;; square
(define (square x)  (* x x))

;;;sinus cardinal
(define (sinc x)
  (if (= x 0.) 1.
      (/ (sin (* 3.1415927 x)) (* 3.1415927 x))))

;;; exact rounding

(define (exact-round x)   (inexact->exact (round x)))

(define (exact-floor x)   (inexact->exact (floor x)))

(define (exact-ceiling x) (inexact->exact (ceiling x)))

;;; float modulo
(define (fmodulo x y)
  (* y (- (/ x y) (floor (/ x y)))))

;;; floor relative to multiples of a float (complements fmodulo:
;;; ffloor+fmodulo = x)
(define (ffloor x y)   (* y (floor (/ x y))))

;;; fceiling = ffloor + y
(define (fceiling x y) (* y (ceiling (/ x y))))

(define (pos-lin x)
  (cond ((> x 0) x)
        (else
         (if (inexact? x) 0. 0))))

;;; lambda float modulo
(define (lambda-fmodulo y)
  (lambda (x)
    (* y
       (- (/ x y)
          (floor (/ x y))))))

(define (round-decimal num place) (/ (round (* num (expt 10 place))) (expt 10 place)))

;; eof
