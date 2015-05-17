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

;; RUPI - Remote Universal Programming Interface
;; This uses compression+encryption to communicate over tcp/ip
(define rupi:debuglevel 1)
(define rupi:thread-num 0)
(define rupi:error #f)
(define (rupi:log level . x) (if (fx>= rupi:debuglevel level) (apply log-system x)))

(define (rupi:safecall x . y)
  (with-exception-catcher
    (lambda (e) (rupi:log 1 "rupi:safecall : exception: " (exception->string e)) #f)
    (lambda () (apply x y))))

;; We use FastLZ for compression of data
(define rupi:compress u8vector-compress)
(define rupi:decompress u8vector-decompress)

;; Declarations of available encryption algorithms
(define rupi:magic (list (string->u8vector "RupiB"))) ;; 5 characters [Rupi+Encrytiontype]
(define rupi:encode (list u8vector-encrypt-blowfish))
(define rupi:decode (list u8vector-decrypt-blowfish))
(define rupi:setkey (list u8vector-setkey-blowfish))

;; The default uses Blowfish for encryption
(define rupi:magicidx 0)

;; Length conversion functions
(define (rupi:length->data l)
  ;; We only support length < 16,777,215 so the fourth byte is left for future use
  (if (fx>= l 16777215)
    (begin (rupi:log 1 "rupi:cmd: Data too long " l) 0)
    (u8vector (bitwise-and l #xff)
      (bitwise-and (arithmetic-shift l -8) #xff)
      (bitwise-and (arithmetic-shift l -16) #xff)
      0)))

(define (rupi:data-length sl)
  ;; Prevent heap overflows with bad packages (length > 16,777,215)
  (if (and (fx= (u8vector-length sl) 4) (fx= (u8vector-ref sl 3) 0))
    (+ (u8vector-ref sl 0)
       (arithmetic-shift (u8vector-ref sl 1) 8)
       (arithmetic-shift (u8vector-ref sl 2) 16))
    0))

(define (rupi:write keyidx data0 port magicidx)
  (let* ((data ((list-ref rupi:encode rupi:magicidx) keyidx (rupi:compress data0)))
         (l (u8vector-length data))
         (lv (rupi:length->data l)))
    (write-subu8vector (list-ref rupi:magic magicidx) 0 5 port)
    (write-subu8vector lv 0 4 port)
    (write-subu8vector data 0 l port)))

(define (rupi:read keyidx port)
  (let ((magic (u8vector 0 0 0 0 0)))
     (read-subu8vector magic 0 5 port)
     (let ((ridx (list-pos rupi:magic magic)))
       (if ridx
         (let ((lv (u8vector 0 0 0 0)))
           (read-subu8vector lv 0 4 port)
           (let* ((l (rupi:data-length lv))
                  (data (make-u8vector l)))
             (if (fx>= l 2)
               (begin
                 (read-subu8vector data 0 l port)
                 (list ridx (rupi:decompress ((list-ref rupi:decode ridx) keyidx data)))
               )
               #f
             )
           )
         )
         #f
       )
     )))

(define (rupi:writeobj port obj keyidx magicidx)
  (rupi:write keyidx (object->u8vector obj) port magicidx))

(define (rupi:readobj port keyidx)
  (let ((rr (rupi:read keyidx port)))
    (if (and rr (not (null? rr)) (u8vector? (cadr rr)))
      (list (car rr) (u8vector->object (cadr rr)))
      #f
    )
  )
)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%
;; client

(define rupi:mutex (make-mutex))
(define (rupi:grab!) (mutex-lock! rupi:mutex))
(define (rupi:release!) (mutex-unlock! rupi:mutex))

(define (rupi-client keyidx key addr port)
  (let ((t (make-table)))
    (table-set! t "ClientAddr" addr)
    (table-set! t "ClientPort" port)
    (table-set! t "ClientIdx" keyidx)
    ((list-ref rupi:setkey rupi:magicidx) keyidx key)
    t))

(define (rupi-valid? t) (and (table? t) (table-ref t "CmdPort" #f)))

(define (rupi:cmd timeout t cmd . args)
  (rupi:grab!)
  (set! rupi:error #f)
  (let ((res (with-exception-catcher
     (lambda (e)
       (rupi:log 1 "rupi:cmd: exception (1): " (exception->string e))
       (let ((p (table-ref t "CmdPort" #f)))
         (if p (rupi:safecall close-port p)))
       (table-set! t "CmdPort" #f)
       (set! rupi:error #t)
       #f
     )
     (lambda ()
      (let* ((clientaddr (table-ref t "ClientAddr"))
             (clientport (table-ref t "ClientPort"))
             (clientidx  (table-ref t "ClientIdx"))
             (cmdport    (table-ref t "CmdPort" #f))
             (p (if cmdport cmdport (open-tcp-client (list server-address: clientaddr port-number: clientport)))))
       (if (port? p) (begin
          (table-set! t "CmdPort" p)
          ;; horrible hack to get rid of any stale data on input
          (input-port-timeout-set! p 0.001 (lambda () #f))
          (let loop () (if (rupi:safecall rupi:readobj p clientidx) (loop)))
    (input-port-timeout-set! p timeout (lambda () (set! rupi:error #t) #f))
          (output-port-timeout-set! p timeout (lambda () (set! rupi:error #t) #f))
          (rupi:writeobj p (append (list cmd) args) clientidx rupi:magicidx)
          (force-output p)
          (with-exception-catcher
            (lambda (e) (rupi:log 1 "rupi:cmd: exception (2): " (exception->string e))
                        (rupi:safecall close-port p)
                        (table-set! t "CmdPort" #f)
                        (set! rupi:error #t)
                        #f
            )
            (lambda () (let* ((data2 (rupi:readobj p clientidx))
                              (data1 (if data2 (cadr data2) #f))
                              (data (if data1 (cdr data1) #f))
                              (retcmd (if data1 (car data1) #f)))
               (if (equal? retcmd cmd) data #f)
            )))
    )))))))
   (rupi:release!)
   res))

(define rupi-cmd rupi-cmd-wait)
(define (rupi-cmd-wait . x) (apply rupi:cmd (append (list 1.) x)))
(define (rupi-cmd-nowait . x) (apply rupi:cmd (append (list 0.001) x)) #t)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%
;; server

(define (rupi-server store keyidx key addr port upiproc)
  (for-each (lambda (keyset) (keyset keyidx key)) rupi:setkey)
  (thread-start! (make-safe-thread (rupi:server store keyidx addr port upiproc) 'rupi-server)))

(define (rupi:serve port keyidx store upiproc)
  (with-exception-catcher
    (lambda (e)
       (rupi:log 1 "rupi:server: exception: " (exception->string e))
          (rupi:safecall close-port port) #f)
    (lambda ()
      (let loop ()
        (let* ((req0 (rupi:readobj port keyidx))
               (req (if (list? req0) (cadr req0) #f))
               (ridx (if (list? req0) (car req0) 0)))
    (if req (begin
            ;; Reply with the client's encryption algorithm.
            (rupi:writeobj port (append (list (car req)) (apply upiproc (append (list store) req))) keyidx ridx)
            (force-output port)
          (loop)))))
        (rupi:safecall close-port port)
    )
))

(define (rupi:server store keyidx addr port upiproc)
  (lambda ()
  (let ((accept-port (open-tcp-server
           (list server-address: addr port-number: port reuse-address: #t
         ))))
    (rupi:log 0 "rupi:server starting at " addr ":" port)
    (let loop ()
      (let ((connection (read accept-port)))
        (if (not (eof-object? connection)) (begin
          (rupi:log 1 "rupi:server thread number " rupi:thread-num)
          (set! rupi:thread-num (fx+ rupi:thread-num 1))
          (thread-start! (make-safe-thread (lambda () (rupi:serve connection keyidx store upiproc)) 'rupi:connection))
          (loop))
        )))
)))

;; unit tests
;; -----------------

(unit-test "rupi-u8vector" "1000 random u8vectors sent/received by rupi"
  (lambda ()
    (let* ((store "rupi-test")
           (rupi:key (random-u8vector 8))
           (rupi:addr (u8vector 127 0 0 1))
           (rupi:port (+ 8000 (random-integer 100)))
           (upi-cmd (lambda (st l) l))
           (rc (rupi-client 0 rupi:key rupi:addr rupi:port)))
      (rupi-server store 0 rupi:key rupi:addr rupi:port upi-cmd)
      (thread-sleep! 0.5)
      (let loop ((n 1000))
        (if (fx= n 0)
          #t
          (if (let* ((datalen (random-integer 100000))
                     (data (random-u8vector datalen)))
                (not (equal? data (rupi-cmd rc data))))
            #f
            (loop (fx- n 1)))
      ))
    )
  ))

(unit-test "rupi-list" "1000 random number lists sent/received by rupi"
  (lambda ()
    (let* ((store "rupi-test")
           (rupi:key (random-u8vector 8))
           (rupi:addr (u8vector 127 0 0 1))
           (rupi:port (+ 8000 (random-integer 100)))
           (upi-cmd (lambda (st l) l))
           (rc (rupi-client 0 rupi:key rupi:addr rupi:port)))
      (rupi-server store 0 rupi:key rupi:addr rupi:port upi-cmd)
      (thread-sleep! 0.5)
      (let loop ((n 1000))
        (if (fx= n 0)
          #t
          (if (let* ((datalen (random-integer 10000))
                     (data (make-list datalen (random-integer 32000))))
                (not (equal? data (rupi-cmd rc data))))
            #f
            (loop (fx- n 1)))
      ))
    )
  ))

(unit-test "rupi-bool" "1000 random booleans sent/received by rupi"
  (lambda ()
    (let* ((store "rupi-test")
           (rupi:key (random-u8vector 8))
           (rupi:addr (u8vector 127 0 0 1))
           (rupi:port (+ 8000 (random-integer 100)))
           (upi-cmd (lambda (st l) l))
           (rc (rupi-client 0 rupi:key rupi:addr rupi:port)))
      (rupi-server store 0 rupi:key rupi:addr rupi:port upi-cmd)
      (thread-sleep! 0.5)
      (let loop ((n 1000))
        (if (fx= n 0)
          #t
          (if (let ((data (list "Booleans" (fx= (random-integer 2) 1)
                                (fx= (random-integer 2) 1) (fx= (random-integer 2) 1))))
                (not (equal? data (rupi-cmd rc data))))
            #f
            (loop (fx- n 1)))
      ))
    )
  ))

;; eof
