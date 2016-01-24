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

;; share stores between local processes using fifos
;; NOTE: fifos are not files, and should be created with the mkfifo command!

(define fifo:readdelay 0.1)

(define fifo:loglevel 0)
(define (fifo:log level . x) 
  (if (fx>= fifo:loglevel level) (apply log-system (append (list "fifo: ") x))))

(define (fifo:write name data)
  (let ((p (open-file name)))
    (if (port? p) (begin
      (with-exception-catcher (lambda (e) #f)
        (lambda ()
          (fifo:log 2 "write: " data) 
          (write data p)
          (force-output p) 
          (close-port p)
          #t)
        )) #f)))

(define fifo:table (make-table))

(define (fifo:read name)
  (let* ((p0 (table-ref fifo:table name #f))
         (p (if p0 p0 (open-input-file name))))
    (if (port? p) (begin
      (table-set! fifo:table name p)
      (input-port-timeout-set! p 0)
      (let ((data (with-exception-catcher (lambda (e) 
               (fifo:log 0 "read exception: " name ": " (exception->string e)) '())
               (lambda () (read-all p)))))
        (if (> (length data) 0) (fifo:log 2 "read: " data))
;;        (close-port p)
        data)) #f)))

(define (fifo:reader! name proc pause)
  (thread-start! (make-safe-thread (lambda ()
    (let loop ()
      (for-each (lambda (data) (apply proc data)) (fifo:read name))
      (thread-sleep! pause)
      (loop))
  ))))

(define (store-fifo-import! store name)
  (fifo:reader! name 
    (lambda (k . v) 
      (let ((kl (string-length k)))
        (fifo:log 1 "import: " k " " v)
        (if (and (fx> kl 5) (string=? (substring k (fx- kl 5) kl) "Alarm"))
          (begin 
            (fifo:log 0 "import: alarm: " k " " v)
            (apply store-event-add (append (list store) (car v))) 
          )
          (begin
            (if (null? v)
              (store:clearlocal! store k)
              (apply store:setlocal! (append (list store k) v))
            )
          ))
      )) fifo:readdelay)) 

(define (store-fifo-export store name parameters)
  (store:setlocal! store "use_fifo_export" #t)
  (for-each (lambda (p)
    (let* ((listlabel (string-append p ":fifoexportlist"))
           (curlist (store-ref store listlabel '())))
      (store:setlocal! store listlabel (append curlist (list name)))
    )
  ) parameters))

(define (store:setfifo! store id val . category)
  (apply store:setlocal! store id val category)
  (for-each (lambda (f)
    (fifo:write f (list id val))
  )(store-ref store (string-append id ":fifoexportlist") '())))

;; eof
