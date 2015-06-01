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

;; list-of-list operations
(define (listlist? obj)
  (if (list? obj)
    (let loop ((o obj)(r #t))
      (if (= (length o) 0) r
        (loop (cdr o) (if (list? (car o)) r #f)))) #f))

(define (listlist-map m ll)
 (if (listlist? ll)
   (let loop ((a ll)(b '()))
     (if (= (length a) 0) b
       (loop (cdr a) (append b (list
         (map m (car a)))))))))

(define (listlist-apply f ll)
  (if (listlist? ll) (apply f (listlist-flatten ll))))

(define (listlist-transpose lst)
  (let loop ((lst2  lst)
             (tlst '()))
    (if (null? (car lst2)) (reverse tlst)
      (loop (map cdr lst2) (cons (map car lst2) tlst)))))

(define (listlist-flatten tree)
  (cond ((null? tree)
         '())
        ((not (pair? tree))
         (list tree))
        ((pair? (car tree))
         (append (listlist-flatten (car tree)) (listlist-flatten (cdr tree))))
        (else
         (cons (car tree) (listlist-flatten (cdr tree))))))


(define (listlist-add m1 m2)
  (if (list? (car m1))
    (map (lambda (row1 row2) (map + row1 row2)) m1 m2)
    (map + m1 m2)
  ))

(define (listlist-subtract m1 m2)
  (if (list? (car m1))
    (map (lambda (row1 row2) (map - row1 row2)) m1 m2)
    (map - m1 m2)
  ))

(define (listlist-scale m n)
  (if (list? (car m))
    (map (lambda (row) (map (lambda (x) (* x n)) row)) m)
    (map (lambda (l) (* l n)) m)
  ))

(define (listlist-multiply m1 m2)
  (cond ((number? m1) (listlist-scale m2 m1))
         ((number? m2) (listlist-scale m1 m2))
         (else
           (listlist-transpose (map (lambda (row) (map (lambda (col)
             (list-dot row col)) m1)) (listlist-transpose m2)))
         )
  ))

(define (listlist:removecolumn m n)
  (map (lambda (row) (append (list-head row (fx- n 1)) (list-tail row n))) m))

(define (listlist:deleterowcol m row col)
  (let loop ((i 1) (res (list)))
    (if (fx> i (length m))
      res
      (loop (fx+ i 1) (if (fx= i row) res (append res (let ((r (list-ref m (fx- i 1))))
        (list (append (list-head r (fx- col 1)) (list-tail r col)))))))
    )
  ))

(define (listlist-determinant m)
  (if (fx= (length m) (length (car m)))
    (if (fx= (length m) 2)
      (- (* (caar m) (cadadr m)) (* (cadar m) (caadr m)))
      (let loop ((i 1)(res 0))
        (if (fx> i (length m))
          res
          (loop (fx+ i 1) (+ res (* (list-ref (car m) (fx- i 1)) (if (odd? i) 1 -1)
                                    (listlist-determinant (listlist:removecolumn (cdr m) i)))))
        )
      ))
    #f
  ))

(define (listlist-cofactor m)
  (let loopr ((row 1) (resr (list)))
    (if (fx> row (length m))
      resr
      (loopr (fx+ row 1)
             (append resr
                (let loopc ((col 1) (resc (list)))
                  (if (fx> col (length (car m)))
                    (list resc)
                    (loopc (fx+ col 1) (append resc (list
                      (* (if (odd? row) (if (odd? col) 1 -1) (if (odd? col) -1 1))
                         (listlist-determinant (listlist:deleterowcol m row col))))
                    ))
                ))
             )
      )
    )
  ))

(define (listlist-inverse m)
  (let ((det (listlist-determinant m)))
    (if (= det 0) #f
      (if (fx= (length m) 2)
        (listlist-scale (list (list (cadadr m) (* (cadar m) -1)) (list (* (caadr m) -1) (caar m)))
                        (/ 1 det))
        (listlist-scale (listlist-transpose (listlist-cofactor m)) (/ 1 det))
      )
    )
  ))

(define (listlist-ref m row col)
  (list-ref (list-ref m row) col)
)

(define (listlist-set! m row col val)
  (let ((r (list-ref m row)))
    (list-set! r col val)
    (list-set! m row r)
    m
  )
)

;;
;; unit tests for List of List (matrix) functions
;; -----------------

(unit-test "listlist-add" "Matrix addition"
  (lambda() (and
    (equal? (listlist-add '(1 2 3) '(4 5 6)) '(5 7 9))
    (equal? (listlist-add '((1 2 3) (4 5 6)) '((1 2 3) (4 5 6))) '((2 4 6) (8 10 12)))
    (equal? (listlist-add '(1) '(1)) '(2))
    (equal? (listlist-add '((1) (3)) '((1) (3))) '((2) (6)))
  )))

(unit-test "listlist-subtract" "Matrix subtraction"
  (lambda () (and
    (equal? (listlist-subtract '(1 2 3) '(1 2 3)) '(0 0 0))
    (equal? (listlist-subtract '((1 2 3) (4 5 6)) '((1 2 3) (4 5 6))) '((0 0 0) (0 0 0)))
    (equal? (listlist-subtract '(9) '(9)) '(0))
    (equal? (listlist-subtract '((1) (1)) '((2) (2))) '((-1) (-1)))
  )))

(unit-test "listlist-scale" "Matrix scaling"
  (lambda() (and
    (equal? (listlist-scale '(1 2 3) 2) '(2 4 6))
    (equal? (listlist-scale '(1 1 1 1 1) 5) '(5 5 5 5 5))
    (equal? (listlist-scale '((1 2 3)) 2) '((2 4 6)))
    (equal? (listlist-scale '((1 0 -1) (2 2 2)) -2) '((-2 0 2)(-4 -4 -4)))
  )))

(unit-test "listlist-multiply" "Matrix multiplication"
  (lambda() (and
    (equal? (listlist-multiply '((1 2 3) (4 5 6)) '((7 8) (9 10) (11 12)))
            '((58 64) (139 154)))
    (equal? (listlist-multiply '((1 2) (3 4)) '((2 0) (1 2))) '((4 4) (10 8)))
    (equal? (listlist-multiply '((2 0) (1 2)) '((1 2) (3 4))) '((2 4) (7 10)))
    (equal? (listlist-multiply '((1 2 3) (4 5 6) (7 8 9)) '((1 0 0) (0 1 0) (0 0 1)))
            '((1 2 3) (4 5 6) (7 8 9)))
    (equal? (listlist-multiply '((1 2)) '((3) (4))) '((11)))
  )))

(unit-test "listlist-determinant" "Matrix determinant"
  (lambda() (and
    (equal? (listlist-determinant '((-2 2 -3) (-1 1 3) (2 0 -1))) 18)
    (equal? (listlist-determinant '((3 8) (4 6))) -14)
    (equal? (listlist-determinant '((1 1 1) (1 1 1) (1 1 1))) 0)
    (equal? (listlist-determinant '((1 2 3) (1 0 2) (1 2 1))) 4)
    (equal? (listlist-determinant '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))) 0)
    (equal? (listlist-determinant '((1 2 3 4) (2 1 4 3) (3 4 1 2) (4 3 2 1))) 0)
    (equal? (listlist-determinant '((1 2 3 0) (2 1 4 3) (3 4 1 2) (0 3 2 1))) 80)
  )))

(unit-test "listlist-inverse" "Matrix inverse"
  (lambda() (and
    (equal? (listlist-inverse '((4 7) (2 6))) '((3/5 -7/10) (-1/5 2/5)))
    (equal? (listlist-inverse '((1 3 3) (1 4 3) (1 3 4)))
            '((7 -3 -3) (-1 1 0) (-1 0 1)))
    (equal? (listlist-inverse '((3 0 2) (2 0 -2) (0 1 1)))
            '((1/5 1/5 0) (-1/5 3/10 1) (1/5 -3/10 0)))
    (equal? (listlist-inverse '((1 2 0 0) (0 0 3 0) (4 0 5 1) (0 5 0 0)))
            '((1 0 0 -2/5) (0 0 0 1/5) (0 1/3 0 0) (-4 -5/3 1 8/5)))
    (equal? (listlist-inverse '((1 1) (1 1))) #f)
  )))
(unit-test "listlist-ref" "Matrix value lookup"
  (lambda () (and
    (equal? (listlist-ref '((4 7) (2 6)) 0 0) 4)
    (equal? (listlist-ref '((4 7) (2 6)) 1 1) 6)
    (equal? (listlist-ref '((1 2 3)) 0 1) 2)
    (equal? (listlist-ref '((1 2 3) (4 5 6) (7 8 9)) 0 2) 3)
    (equal? (listlist-ref '((1 2 3) (4 5 6) (7 8 9)) 2 0) 7)
  )))
(unit-test "listlist-set!" "Matrix value setting"
  (lambda () (and
    (equal? (let ((m '((1 2) (3 4)))) (listlist-set! m 0 0 5)) '((5 2) (3 4)))
    (equal? (let ((m '((1 2) (3 4)))) (listlist-set! m 1 0 5)) '((1 2) (5 4)))
    (equal? (let ((m '((1 2 3) (4 5 6) (7 8 9)))) (listlist-set! m 1 1 55))
            '((1 2 3) (4 55 6) (7 8 9)))
  )))
;;eof
