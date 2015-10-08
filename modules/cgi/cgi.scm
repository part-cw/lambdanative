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

;; cgi server

(##namespace ("cgi#"))
(##include "~~lib/gambit#.scm")
(##include "cgi#.scm")
(##namespace ("" string-replace-char log-system url-decode string-split u8vector->string string-mapconcat))

(define (string-getenv s)
  (with-exception-catcher (lambda (e) "") (lambda () (getenv s))))

(define (parse-querystring s)
  (if (string? s) (map (lambda (p) 
     (let ((tmp (string-split p #\=)))
       (if (> (length tmp) 1) (list (car tmp) (url-decode (cadr tmp) 0 (string-length (cadr tmp)) #t)) (list (car tmp) ""))
     )) (string-split s #\&)) '()))

;; this is not fully working?
(define (parse-cookiestring s)
  (if (string? s) (map (lambda (p) (string-split p #\=))  (string-split s #\;)) '()))

(define-macro (cgi-env k)
 `(define ,k ""))

(define-macro (cgi-env-set! k)
 `(set! ,k (string-getenv ,(symbol->string k))))

(define-macro (cgi-clear! k)
 `(set! ,k ""))

(cgi-env SERVER_SOFTWARE)
(cgi-env SERVER_NAME)
(cgi-env GATEWAY_INTERFACE)
(cgi-env SERVER_PROTOCOL)
(cgi-env SERVER_PORT)
(cgi-env REQUEST_METHOD)
(cgi-env PATH_INFO)
(cgi-env PATH_TRANSLATED)
(cgi-env SCRIPT_NAME)
(cgi-env QUERY_STRING)
(cgi-env REMOTE_HOST)
(cgi-env REMOTE_ADDR)
(cgi-env AUTH_TYPE)
(cgi-env REMOTE_USER)
(cgi-env REMOTE_IDENT)
(cgi-env CONTENT_TYPE)
(cgi-env CONTENT_LENGTH)
(cgi-env HTTP_REFERER)
(cgi-env HTTP_ACCEPT)
(cgi-env HTTP_ACCEPT_LANGUAGE)
(cgi-env HTTP_USER_AGENT)
(cgi-env HTTP_COOKIE)

(define GET_VARS '())
(define POST_VARS '())
(define COOKIES '())

#|
;; -----------------------
;; error handling : dump traces straight to browser

;; try to output location of continuation
;; this only works if debug information is available
(define (cgi:traceidentify cont)
  (let ((locat (##continuation-locat cont)))
    (if locat
        (let* ((container (##locat-container locat))
               (file (##container->path container)))
          (if file
              (let* ((filepos (##position->filepos (##locat-position locat)))
                     (line (##fixnum.+ (##filepos-line filepos) 1))
                     (col (##fixnum.+ (##filepos-col filepos) 1)))
                (for-each display (list "trace: " file " line=" line " col=" col)))
              #f))
        #f)))

(define (cgi:trace thread)
  (with-output-to-string "" (lambda ()
    (let* ((capture (##thread-continuation-capture thread)))
       (let cloop ((cont (##continuation-first-frame capture #f))(n 0))
          (if cont (let ((locat (##continuation-locat cont)))
          (if (and locat (> n 1)) (cgi:traceidentify cont))
        (cloop (##continuation-next-frame cont #f)(fx+ n 1)))))
   ))))

(define (cgi:exception->string e)
  (let* ((str (with-output-to-string '() (lambda () (display-exception e (current-output-port)))))
         (tmp (string-split str #\newline)))
    (string-mapconcat (reverse tmp) ": ")))

(define (cgi:exception-handler e)
  (for-each display  (list
    "Content-Type: text/plain\n\n"
    (cgi:exception->string e) "\n"
    (cgi:trace (current-thread))
    "HALT\n"))
  (exit))

|#

;; -----------------------

(define (cgi-env-init)
  (cgi-env-set! SERVER_SOFTWARE)
  (cgi-env-set! SERVER_NAME)
  (cgi-env-set! GATEWAY_INTERFACE)
  (cgi-env-set! SERVER_PROTOCOL)
  (cgi-env-set! SERVER_PORT)
  (cgi-env-set! REQUEST_METHOD)
  (cgi-env-set! PATH_INFO)
  (cgi-env-set! PATH_TRANSLATED)
  (cgi-env-set! SCRIPT_NAME)
  (cgi-env-set! QUERY_STRING)
  (cgi-env-set! REMOTE_HOST)
  (cgi-env-set! REMOTE_ADDR)
  (cgi-env-set! AUTH_TYPE)
  (cgi-env-set! REMOTE_USER)
  (cgi-env-set! REMOTE_IDENT)
  (cgi-env-set! CONTENT_TYPE)
  (cgi-env-set! CONTENT_LENGTH)
  (cgi-env-set! HTTP_REFERER)
  (cgi-env-set! HTTP_ACCEPT)
  (cgi-env-set! HTTP_ACCEPT_LANGUAGE)
  (cgi-env-set! HTTP_USER_AGENT)
  (cgi-env-set! HTTP_COOKIE)
  (set! GET_VARS (parse-querystring (string-getenv "QUERY_STRING")))
  (set! COOKIES (parse-cookiestring (string-getenv "HTTP_COOKIE")))
  (let* ((content-length (string->number CONTENT_LENGTH))
         (buffer (if content-length (make-u8vector content-length) #f)))
    (if buffer (begin ;; POST data
       (read-subu8vector buffer 0 content-length (current-input-port))
       (set! POST_VARS (parse-querystring (u8vector->string buffer)))
       )))
)

(define (cgi-env-clear)
  (cgi-clear! SERVER_SOFTWARE)
  (cgi-clear! SERVER_NAME)
  (cgi-clear! GATEWAY_INTERFACE)
  (cgi-clear! SERVER_PROTOCOL)
  (cgi-clear! SERVER_PORT)
  (cgi-clear! REQUEST_METHOD)
  (cgi-clear! PATH_INFO)
  (cgi-clear! PATH_TRANSLATED)
  (cgi-clear! SCRIPT_NAME)
  (cgi-clear! QUERY_STRING)
  (cgi-clear! REMOTE_HOST)
  (cgi-clear! REMOTE_ADDR)
  (cgi-clear! AUTH_TYPE)
  (cgi-clear! REMOTE_USER)
  (cgi-clear! REMOTE_IDENT)
  (cgi-clear! CONTENT_TYPE)
  (cgi-clear! CONTENT_LENGTH)
  (cgi-clear! HTTP_REFERER)
  (cgi-clear! HTTP_ACCEPT)
  (cgi-clear! HTTP_ACCEPT_LANGUAGE)
  (cgi-clear! HTTP_USER_AGENT)
  (cgi-clear! HTTP_COOKIE)
  (set! GET_VARS '())
  (set! POST-VARS '())
  (set! COOKIES '())
)

(define (cgi-server init-proc session-proc close-proc)
  (if init-proc (init-proc))
  (cgi-env-init)
  (let ((outp (with-output-to-string "" (lambda ()
          (if session-proc (session-proc) (for-each display (list
             "Content-Type: text/plain\n\n"
             "No session hook?\n")))))))
    (if close-proc (close-proc))
    (display outp)))

;; eof
