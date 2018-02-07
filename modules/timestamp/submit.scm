#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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

;; Server settings for RFC 3161 compatible trusted timestamping
;; The default server declared here is run by the German National
;; Research and Education Network (DFN), which doesn't allow
;; commercial use. See https://www.pki.dfn.de/zeitstempeldienst/
(define timestamp:host "zeitstempel.dfn.de")
(define (timestamp-host-set! name) (set! timestamp:host name))
(define timestamp:url "/")
(define (timestamp-url-set! url) (set! timestamp:url url))

;; Make query to TTS server
(define (timestamp-tsr-request-raw tsq)
  (let* ((request-str (string-append "POST " timestamp:url " HTTP/1.0\r\n"
           "Host: " timestamp:host "\r\n"
           "User-Agent: " (system-appname) "/" (system-appversion) "\r\n"
           "Content-Type: application/timestamp-query\r\n"
           "Content-Length: " (number->string (u8vector-length tsq)) "\r\n\r\n"))
         (request-u8vec (string->u8vector request-str))
         (request-len (u8vector-length request-u8vec))
         (res (with-exception-catcher (lambda (e) #f)
           (lambda ()
             (let* ((port 80)
                    (p (open-tcp-client (list server-address: timestamp:host port-number: port))))
               (if (port? p) (begin
                 (input-port-timeout-set! p 2. (lambda () #f))
                 (output-port-timeout-set! p 1. (lambda () #f))
                 (write-subu8vector request-u8vec 0 request-len p)
                 (write-subu8vector tsq 0 (u8vector-length tsq) p)
                 (force-output p)
                 (with-exception-catcher (lambda (e) #f)
                   (lambda () (let ((data (read-all p read-char))) (close-port p) data)))
               ))
             )
           ))))
    (if res (list->u8vector (map char->integer res)) #f)))

(define-macro (V= idx match) `(fx= (u8vector-ref u8v ,idx) ,match))

;; seek to 0d-0a-0d-0a and trim
(define (timestamp-tsr-request-parse u8v)
  (if u8v (let ((len (u8vector-length u8v)))
    (if (> len 4) (let loop ((i 3))
      (if (= i len) #f
        (if (and (V= (- i 3) 13) (V= (- i 2) 10) (V= (- i 1) 13) (V= i 10))
          (subu8vector u8v (+ i 1) (u8vector-length u8v))
            (loop (+ i 1))))) #f)) #f))

(define  (timestamp-tsr-request tsq)
  (let ((res (timestamp-tsr-request-raw tsq)))
    (if res (timestamp-tsr-request-parse res))))

;; eof
