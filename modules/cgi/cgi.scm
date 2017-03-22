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

;; cgi server

(##namespace ("cgi#"))
(##include "~~lib/gambit#.scm")
(##include "cgi#.scm")
(##namespace ("" url-decode string-split))

(define (string-split-sane a b) (if (= (string-length a) 0) '("") (string-split a b)))

(define (cgi-parse-query s)
  (if (string? s) (map (lambda (p) 
     (let ((tmp (string-split-sane p #\=)))
       (if (> (length tmp) 1) (list (car tmp) (url-decode (cadr tmp) 0 (string-length (cadr tmp)) #t)) (list (car tmp) ""))
     )) (string-split-sane s #\&)) '()))

;; merge GET and POST arguments
(define (cgi-parse-all-queries cgi-env)
  (let* ((a (cgi-parse-query (cadr (assoc "QUERY_STRING" cgi-env))))
         (post? (string-ci=? "application/x-www-form-urlencoded" (car (string-split-sane (cadr (assoc "CONTENT_TYPE" cgi-env)) #\;))))
         (clen (if post? (string->number (cadr (assoc "CONTENT_LENGTH" cgi-env))) #f))
         (cstr (if (number? clen) (let ((s (make-string clen))) (read-substring s 0 clen) s) #f))
         (b (if cstr (cgi-parse-query cstr) '())))
    (append a b)))

(define (cgi:getenv s)
  (let ((id (if (string? s) s (symbol->string s))))
    (list id (with-exception-catcher (lambda (e) "") (lambda () (getenv id))))))

(define (cgi-build-environment)
  (list 
    (cgi:getenv 'SERVER_SOFTWARE)
    (cgi:getenv 'SERVER_NAME)
    (cgi:getenv 'GATEWAY_INTERFACE)
    (cgi:getenv 'SERVER_PROTOCOL)
    (cgi:getenv 'SERVER_PORT)
    (cgi:getenv 'REQUEST_METHOD)
    (cgi:getenv 'PATH_INFO)
    (cgi:getenv 'PATH_TRANSLATED)
    (cgi:getenv 'SCRIPT_NAME)
    (cgi:getenv 'QUERY_STRING)
    (cgi:getenv 'REMOTE_HOST)
    (cgi:getenv 'REMOTE_ADDR)
    (cgi:getenv 'AUTH_TYPE)
    (cgi:getenv 'REMOTE_USER)
    (cgi:getenv 'REMOTE_IDENT)
    (cgi:getenv 'CONTENT_TYPE)
    (cgi:getenv 'CONTENT_LENGTH)
    (cgi:getenv 'HTTP_REFERER)
    (cgi:getenv 'HTTP_ACCEPT)
    (cgi:getenv 'HTTP_ACCEPT_LANGUAGE)
    (cgi:getenv 'HTTP_USER_AGENT)
    (cgi:getenv 'HTTP_COOKIE)))

(define (cgi-serve proc) 
  (let ((port (current-output-port))
        (data (proc (cgi-build-environment))))
    (write-subu8vector data 0 (u8vector-length data) port)
    (force-output port)
    (close-port port)
  ))

;; eof
