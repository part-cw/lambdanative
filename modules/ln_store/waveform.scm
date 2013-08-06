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

;; temporary storage for waveform data (lists or f32vectors)
(define (store-waveform-append store id data)
  (let ((t (store:wdatatable store)))
     (let ((appendit  (member id (table-ref t 'IdList '())))
           (olddata   (table-ref t id #f)))
       (table-set! t id
         (if appendit
           (cond
            ((list? olddata) (append olddata data))
            ((f32vector? olddata) (f32vector-append olddata data))
            (else data)
           )
           data
         ))

       (if (not appendit)
         (let ((oldlist (table-ref t 'IdList '())))
            (table-set! t 'IdList (append oldlist (list id))))))))

;; set waveform scaling with lst being (imin imax omin omax)
;; [imin imax] = input range
;; [omin omax] = output range
(define (store-waveform-scale store id lst)
  (let ((t (store:wscalingtable store)))
     (table-set! t id lst)))

(define (store-waveform-clear store . auxid)
  (let* ((t (store:wdatatable store))
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
         '()))))

(define (waveform:scale data scale)
  (let ((i_min (car scale))
        (i_max (cadr scale))
        (o_min (caddr scale))
        (o_max (cadddr scale)))
    (if (= o_max o_min)
      data
      (map (lambda (v) (flo (+ o_min (/ (* (- o_max o_min) (- v i_min)) (- i_max i_min))))) data)
    )))

(define (waveform:f32scale data scale)
  (let* ((i_min (exact->inexact (car scale)))
         (i_max (exact->inexact (cadr scale)))
         (o_min (exact->inexact (caddr scale)))
         (o_max (exact->inexact (cadddr scale)))
         (o_delta (fl- o_max o_min))
         (i_delta (fl- i_max i_min))
         (datalen (f32vector-length data)))
    (if (not (fl= o_max o_min))
       (let loop ((n 0))
         (if (fx< n datalen)
            (let ((v (f32vector-ref data n)))
              (f32vector-set! data n (fl+ o_min (fl/ (fl* o_delta (fl- v i_min)) i_delta)))
              (loop (fx+ n 1))))))
    (f32vector->list data)))

;; dispatch waveforms with scaling
(define (store-waveform-dispatch store . category)
  (let ((c (if (fx= (length category) 1) (car category) "waveform"))
        (tdata (store:wdatatable store))
        (tscaling (store:wscalingtable store)))
    (store-clear! store (map car (store-listcat store c)))
    (let loop ((ids (table-ref tdata 'IdList '())))
      (if (= (length ids) 0)
        (store-waveform-clear store)
        (let ((scaling (table-ref tscaling (car ids) (list 0 0 0 0)))
              (data (table-ref tdata (car ids) #f)))
          (if (list? data) (store-set! store (car ids) (waveform:scale data scaling) c))
          (if (f32vector? data) (store-set! store (car ids) (waveform:f32scale data scaling) c))
          (loop (cdr ids))
        )))
  ))

;; eof