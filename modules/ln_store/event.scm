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

;; maintain a store event list

(define event:debuglevel 0)
(define (event:log level . x) (if (fx>= event:debuglevel level) (apply log-system x)))

(define event:grayperiod 60.)

;; ignore duplicate events for a period of time
;; NOTE: priority 0,1,4 event are passed without graylisting
(define (event:graylisted? store estr now)
  (let loop ((gl (store-ref store "EventGrayList" '()))(ngl '())(res #f))
    (if (fx= (length gl) 0)
      (begin
        (store-set! store "EventGrayList" (append ngl (if (not res) (list (list now estr)) '())))
        res
      )
      (let* ((gentry (car gl)) (gtime (car gentry)) (gstr (cadr gentry)))
        (loop (cdr gl) (append ngl (if (< (- now gtime) event:grayperiod) (list gentry) '()))
               (if (string=? gstr estr) #t res))))))

(define (store-event-add store priority id . payload)
  (define (payload->string p)
    (cond
      ((number? p)
        (float->string p 10)
      )
      ((string? p)
        p
      )
      (else (object->string p))))
  (event:log 1 "store-event-add " store " " priority " " id)
  (let ((el (store-ref store "EventList" '()))
        (estr (let loop ((s (append (list id) payload))(r ""))
           (if (= (length s) 0) r
             (loop (cdr s) (string-append r (payload->string (car s)) (if (fx= (length s) 1) "" ":")))
           ))))
    (if (or (fx= priority 0) (fx= priority 1) (fx= priority 4) (not (event:graylisted? store estr ##now)))
       (begin
         (event:log 1 "store-event-add event accepted")
         (store-set! store "EventList" (append (list (list ##now estr priority)) el) "event")
       )
      )))

(define (store-event-clear! store)
  (store-set! store "EventList" '())
  (store-clearcat! store "event"))

(define (store-event-listnew store . tstamp)
  (let ((t (if (fx= (length tstamp) 1) (car tstamp) 0.)))
    (let loop ((es (store-ref store "EventList" '()))(result '()))
      (if (or (fx= (length es) 0) (>= t (car (car es)))) result
        (loop (cdr es) (append result (list (car es))))))))

;; eof
