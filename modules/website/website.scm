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

;; module to load and render websites from scheme

(##namespace ("website#"))
(##include "~~lib/gambit#.scm")
(##include "website#.scm")

(##namespace (""
  file->u8vector
  u8vector-compress u8vector-decompress
  pregexp-replace* sxml->xml
  string-mapconcat string-split string-downcase
  exception->string log:exception-handler
  log-error log-system
  make-safe-thread
  u8vector->base64-string
  system-directory system-pathseparator string-contains
))

(define (string-split-sane a b) (if (or (not (string? a)) (= (string-length a) 0)) '("") (string-split a b)))

(define website:debuglevel 0)
(define (website:log level . x)
   (if (>= website:debuglevel level) (apply log-system (append (list "website: ") x))))

(define website:compress u8vector-compress)
(define website:decompress u8vector-decompress)

(define (website:safecall x . y)
  (with-exception-catcher
    (lambda (e) (log-error "website:safecall : exception: " (exception->string e)) #f)
    (lambda () (apply x y))))

;; ------------------
;; recursively load a website

;; scan and load directory recursively
(define (website:scan-directory db path0 . fullpath0)
  (let* ((has-fullpath? (fx> (length fullpath0) 0))
         (path (if (or has-fullpath? (string-contains path0 (system-pathseparator)))
           path0
           (string-append (system-directory) (system-pathseparator) path0)))
         (fullpath (if has-fullpath? (car fullpath0) path)))
    (if (and (file-exists? path) (not (eq? #\~ (string-ref path (- (string-length path) 1)))))
      (let* ((i (file-info path))
             (t (file-info-type i)))
       (cond
         ((eq? t 'directory)
           (website:log 2 "=> scanning " path "..")
           (let loop ((fs (directory-files path)))
             (if (fx= (length fs) 0) #f (begin
               (website:scan-directory db (string-append path "/" (car fs)) fullpath)
               (loop (cdr fs))))))
         ((eq? t 'regular)
           (let ((trimpath (string-append "/" (string-mapconcat (cdr (string-split-sane
                   (pregexp-replace* fullpath (pregexp-replace* "//" path "/") "") #\/)) "/"))))
             (website:log 2 "==> adding " path " [" trimpath "]..")
             (table-set! db trimpath (website:compress (file->u8vector path)))
           )
         )
         (else
           (website:log 0 "** unsupported file type: " path "\n")
         )
       )
    ))
  ))

;; ------------------
;; render the website

(define (website:lookup-mimetype ext)
  (case (string->symbol ext)
    ((sxml html htm) "text/html")
    ((txt) "text/plain")
    ((css) "text/css")
    ((css-dyn) "text/css")
    ((xml) "text/xml")
    ((png) "image/png")
    ((jpg) "image/jpeg")
    ((svg) "image/svg+xml")
    ((json) "application/json")
    ((pdf) "application/pdf")
    ((js) "text/javascript")
    (else "application/force-download")
  ))

(define (website:server db port)
  (let ((accept-port (open-tcp-server (list server-address: "*" port-number: port reuse-address: #t))))
    (let loop () (let ((connection (read accept-port)))
        (if (not (eof-object? connection))
            (begin (thread-start! (make-safe-thread (lambda ()
              (current-exception-handler log:exception-handler)
              (website:serve db connection) )))
     (loop)))))))

(define (website:trim-string s)
  (if (string? s)
    (let ((len (string-length s)))
      (cond
        ((= len 0) s)
        ((char=? (string-ref s 0) #\space) (website:trim-string (substring s 1 len)))
        ((or (char=? (string-ref s (- len 1)) #\space)
             (char=? (string-ref s (- len 1)) #\return))
           (website:trim-string (substring s 0 (- len 1))))
        (else s))) s))

(define (website:split-header line)
  (let ((len (string-length line)))
    (let loop ((i 0))
      (if (= i len) (list line "")
        (if (char=? (string-ref line i) #\:)
          (list (string-downcase (website:trim-string (substring line 0 i))) (website:trim-string (substring line (+ i 1) len)))
            (loop (+ i 1)))))))

(define (website:read-header port)
  (let loop ((headers '()))
    (let ((line (website:trim-string (read-line port))))
      (if (not (string? line)) #f
        (if (= (string-length line) 0) (begin (website:log 4 "website:read-header: " headers) headers)
          (loop (append headers (list (website:split-header line)))))))))

(define (website:read-content port headers)
  (let* ((entry (assoc "content-length" headers))
         (len (if entry (string->number (cadr entry)) #f))
         (str (if len (make-string len) #f)))
    (if str (read-substring str 0 len port)) str))

(define (website:build-cgi-environment request headers)
  (define (cadr-safe s) (if (> (length s) 1) (cadr s) ""))
  (define (cgi-extract headers http-id cgi-id . fun)
    (let* ((entry (assoc http-id headers)))
      (list (list cgi-id (if entry (if (null? fun) (cadr entry) (fun (cadr entry))) "")))))
  (let* ((r (string-split-sane request #\space))
         (m (car r))
         (p (string-split-sane (cadr-safe r) #\?))
         (pi (car p))
         (qs (cadr-safe p)))
  (append
    (cgi-extract headers "referer" "HTTP_REFERER")
    (cgi-extract headers "accept" "HTTP_ACCEPT")
    (cgi-extract headers "accept-language" "HTTP_ACCEPT_LANGUAGE")
    (cgi-extract headers "accept-encoding" "HTTP_ACCEPT_ENCODING")
    (cgi-extract headers "user-agent" "HTTP_USER_AGENT")
    (cgi-extract headers "host" "HTTP_HOST")
    (cgi-extract headers "cookie" "HTTP_COOKIE")
    (cgi-extract headers "connection" "HTTP_CONNECTION")
    (cgi-extract headers "content-length" "CONTENT_LENGTH")
    (cgi-extract headers "content-type" "CONTENT_TYPE")
    `(("REQUEST_METHOD" ,m))
    `(("PATH_INFO" ,pi))
    `(("QUERY_STRING" ,qs))
    `(("REQUEST_URI" ,(cadr r)))
  )))

(define (website:serve db port)
  (with-exception-catcher (lambda (e) (website:safecall close-port port) #f)
   (lambda ()
     (let* ((request (website:trim-string (website:safecall read-line port)))
            (headers  (website:read-header port))
            (content (website:read-content port headers))
            (cgi-env (website:build-cgi-environment request headers)))
       (if (string? request)
         (let* ((r (string-split-sane request #\space))
                (m (car r)))
            (website:log 3 "website:serve request: " request)
            (website:log 3 "website:serve header: " headers)
            (website:log 3 "website:serve cgi env: " cgi-env)
            (website:log 3 "website:serve content: " content)
            (if (or (string-ci=? m "GET") (string-ci=? m "POST"))
              (begin
                (if content (set! cgi-env (append cgi-env (list (list "CONTENT" content)))))
                (website:servefile db port
                  (if (string=? (cadr r) "/") "/index.html" (cadr r)) cgi-env)))
            (website:safecall force-output port)
            (website:safecall close-port port)
    ))))))

(define (website:servefile db port doc cgi-env)
  (website:log 3 "serving " doc "..")
  (let* ((decomposit (string-split-sane doc #\?))
         (document (car decomposit))
         (zdata (table-ref db document #f))
         (catchall (table-ref db 'catchall #f))
         (data (if (u8vector? zdata) (website:decompress zdata)
                 (if (procedure? zdata) (with-input-from-port port (lambda () (zdata cgi-env)))
                   (if (procedure? catchall) (with-input-from-port port (lambda () (catchall document cgi-env))) #f))))
         (ext (car (reverse (string-split-sane document #\.))))
         (headers (string-append "HTTP/1.0 200 OK\n"
                                "Content-Type: " (website:lookup-mimetype ext) "\n"
                                (if (procedure? zdata) "Cache-Control: no-cache, no-store\n" "")
                                "Access-Control-Allow-Origin: *\n"
                              ;;  "Access-Control-Allow-Methods: GET,POST,PUT\n"
                                "\n")))
    (if data
      (begin
        (website:safecall display headers port)
        (if (string=? ext "sxml")
          (sxml->xml (if (= (u8vector-ref data 0) 96) (eval (with-input-from-u8vector data read)) (with-input-from-u8vector data read)) port)
          (website:safecall write-subu8vector data 0 (u8vector-length data) port)
        ))
      (begin
        (website:log 0 "!! unknown source: " document)
        (website:safecall display "HTTP/1.0 400 OK\ncontent-type: text/plain\n\nInvalid Request\n" port))
   )))

;; ----------------

(define (website-dataurl db0 document)
  (let* ((db (if db0 db0 website:db))
         (zdata (table-ref db (if (char=? (string-ref document 0) #\/) document (string-append "/" document)) #f))
         (data (if (u8vector? zdata) (website:decompress zdata) #f))
         (ext (car (reverse (string-split-sane document #\.))))
         (mimetype (website:lookup-mimetype ext)))
    (string-append "data:" mimetype ";base64,"  (u8vector->base64-string data))))

;; ----------------

(define website:db (make-table))
(define (website-getdb) website:db)

(define (website-serve db port) (thread-start! (make-safe-thread (lambda ()
  (current-exception-handler log:exception-handler)
  (website:server (if db db website:db) port)))))

(define (website-addhook db document proc)
  (table-set! (if db db website:db) document proc))

(define (website-catchall db proc)
  (table-set! (if db db website:db) 'catchall proc))

(define (website->table path)
  (let ((t (make-table)))
    (website:scan-directory t path)
    t))

(define make-website make-table)

(define (website-merge! db t) (table-merge! (if db db website:db) t))

;; eof
