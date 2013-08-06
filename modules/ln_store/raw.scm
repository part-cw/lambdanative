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

;; temporary storage for raw binary data
(define (store-raw-append store id data)
  (let ((t (store:rawtable store)))
     (let ((appendit  (member id (table-ref t 'IdList '())))
           (olddata   (table-ref t id #f)))
       (table-set! t id (if (and appendit (u8vector? olddata)) (u8vector-append olddata data) data))
       (if (not appendit) 
         (let ((oldlist (table-ref t 'IdList '())))
            (table-set! t 'IdList (append oldlist (list id))))))))

(define (store-raw-clear! store . auxid)
  (let* ((t (store:rawtable store))
         (idlist (table-ref t 'IdList '()))
         (id (if (fx= (length auxid) 1) (car auxid) #f)))
    (table-set! t 'IdList
       (if id
         (let loop ((ids idlist)(out '()))
           (if (fx= (length ids) 0)
             out
             (loop (cdr ids) (append out (if (equal? id (car ids)) '() (list (car ids)))))
           )
         )
         '()
       ))
  ))

(define (store-raw-dispatch store . category)
  (let ((c (if (fx= (length category) 1) (car category) "raw"))
        (tdata (store:rawtable store)))
    (let loop ((ids (table-ref tdata 'IdList '())))
      (if (= (length ids) 0) (store-raw-clear! store)
         (let ((data (table-ref tdata (car ids) #f)))
            (if (u8vector? data) (store-set! store (car ids) data c))
            (loop (cdr ids)))))))

(define (store-raw-length store id)
  (let* ((t (store:rawtable store))
        (appendit  (member id (table-ref t 'IdList '())))
        (data   (table-ref t id #f)))
     (if (and appendit (u8vector? data)) (u8vector-length data) 0)))

;; eof
