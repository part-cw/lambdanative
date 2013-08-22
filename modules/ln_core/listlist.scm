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
  (if (listlist? ll) (apply f (flatten ll))))

(define (transpose lst)
  (let loop ((lst2  lst)
             (tlst '()))
    (if (null? (car lst2)) (reverse tlst)
      (loop (map cdr lst2) (cons (map car lst2) tlst)))))

(define (flatten tree)
  (cond ((null? tree) 
         '())
        ((not (pair? tree))
         (list tree))
        ((pair? (car tree))
         (append (flatten (car tree)) (flatten (cdr tree))))
        (else
         (cons (car tree) (flatten (cdr tree))))))

;; eof
