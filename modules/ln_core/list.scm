#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2020, University of British Columbia
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
;; list related extras

;; Make a list of length n, where each element is set to elem
(define (make-list n #!optional (elem 0))
  (if (zero? n) '()
      (cons elem (make-list (- n 1) elem))))

;; mar 2010: changed implementation
;;(define (list-head l n)
;;  (let ((len (length l)))
;;    (if (>= n len) l
;;      (reverse (list-tail (reverse l) (- len n))))))
(define (list-head l k)
  (if (or (= k 0) (null? l)) '()
      (cons (car l) (list-head (cdr l) (- k 1)))))


(define (list-keep lst cnd)
  (let loop ((l lst))
    (if (not (pair? l)) l
      (let ((head (car l))(tail (cdr l)))
        (if (cnd head)
          (let ((new-tail (loop tail)))
            (if (eq? tail new-tail) l (cons head new-tail))
          )
          (loop tail)
        )
      )
    )
  ))

;; mapS behaves like map, except for stripping empty lists from the returned list
;; Usage: (maps FunctionToBeCalledForEachElement ListToWorkOn)
(define (maps fcn lst)
  (list-keep (map fcn lst) (lambda (l) (not (and (list? l) (= (length l) 0))))))

;; do a continuous lookup
(define (list-interpolate lst x0)
  (let* ((x (max (min x0 1.) 0.))
        (n (- (length lst) 1))
        (idx1 (macro-fix (floor (* x n))))
        (idx2 (macro-fix (ceiling (* x n))))
        (v1 (list-ref lst idx1))
        (v2 (list-ref lst idx2)))
   (+ v1 (* (- (* x n) idx1) (- v2 v1)))))

;; resample a list by linear interpolation
(define (list-resample lst n)
  (let loop ((x 0.)(i 0)(nlst '()))
    (if (= i n) nlst
      (loop (+ x (/ 1. (- n 1.))) (+ i 1)
         (append nlst (list
           (list-interpolate lst x)))))))

(define (sublist lst start end)
  (list-head (list-tail lst start) (- end start)))

(define (list-remove-duplicates lst)
    (do ((a '() (if (member (car lst) a) a (cons (car lst) a)))
         (lst lst (cdr lst)))
      ((null? lst) (reverse a))))

(define (list-delete-item lst item)
  (cond
    ((fx= (length lst) 0) lst)
    ((equal? item (car lst)) (cdr lst))
    (else (cons (car lst) (list-delete-item (cdr lst) item)))
  )
)

(define (list-insert-item lst item)
  (if (member item lst) lst
    (append lst (list item))))

(define (list-set! lst pos item)
  (cond ((or (< pos 0) (null? lst)) #f)
        ((= pos 0) (set-car! lst item))
	(else (list-set! (cdr lst) (- pos 1) item))
  )
)

(define (list-pos lst element)
  (if (and (not (null? lst)) (member element lst))
    (fx- (length lst) (length (member element lst)))
    #f
  ))

;; New list filled with n elements containing s, s+1, s+2, ... s+n-1
(define (make-list-natural s n)
  (if (zero? n)
    '()
    (cons s (make-list-natural (+ s 1) (- n 1)))
  ))

;; New list filled with n elements containing s, s+i, s+2*i, ... s+(n-1)*i
(define (make-list-increment s n i)
  (if (zero? n)
    '()
    (cons s (make-list-increment (+ s i) (- n 1) i)))
)

;; eof
