#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2016, University of British Columbia
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

;; logical storing of data and instances

(include "data.scm")
(include "waveform.scm")
(include "event.scm")
(include "raw.scm")
(include "fifo.scm")
(include "file.scm")
(include "instance.scm")
(include "ln_store-tests.scm")

(define store:list (make-table))

(define (store? name)
  (and (string? name) (table-ref store:list name #f)))

(define (store-list) (map car (table->list store:list)))

(define (make-store name)
  (if (not (table-ref store:list name #f)) (begin
    (table-set! store:list name
      (vector (make-table init: #f) ;; data
              (make-table init: #f) ;; category
              (make-table init: #f) ;; wdata
              (make-table init: #f) ;; wscaling
              (make-table init: #f) ;; instances
              (make-table init: #f) ;; raw
        ))
      (store-set! name "StoreID" name)))
  name)

(define (destroy-store! name) (table-set! store:list name))

;; Export/import data functions
(define (store-export-data)
  (map (lambda (l)
    (let ((name (car l))
          (vec (cdr l)))
      (list name (table-copy (vector-ref vec 0)) (table-copy (vector-ref vec 1)))
    )) (table->list store:list)))

(define (store-import-data data)
  (for-each (lambda (d)
    (let* ((name (make-store (car d)))
           (t (table-ref store:list name)))
      (vector-set! t 0 (cadr d))
      (vector-set! t 1 (caddr d))
    )) data))

;; Internal structure references
(define (store:datatable name)
  (let ((p (table-ref store:list name #f)))
    (if p (vector-ref p 0) #f)))

(define (store:categorytable name)
  (let ((p (table-ref store:list name #f)))
    (if p (vector-ref p 1) #f)))

(define (store:wdatatable name)
  (let ((p (table-ref store:list name #f)))
    (if p (vector-ref p 2) #f)))

(define (store:wscalingtable name)
  (let ((p (table-ref store:list name #f)))
    (if p (vector-ref p 3) #f)))

(define (store:instancetable name)
  (let ((p (table-ref store:list name #f)))
    (if p (vector-ref p 4) #f)))

(define (store:rawtable name)
  (let ((p (table-ref store:list name #f)))
    (if p (vector-ref p 5) #f)))

;; eof
