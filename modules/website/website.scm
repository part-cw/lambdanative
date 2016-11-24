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
  log-system file->u8vector pregexp-replace*
  string-mapconcat string-split sxml->xml 
  u8vector-compress u8vector-decompress
))

(define website:debuglevel 0)
(define (website:log level . x)
   (if (>= website:debuglevel level) (apply log-system (append (list "website: ") x))))

(define website:compress u8vector-compress)
(define website:decompress u8vector-decompress)

;; ------------------
;; recursively load a website

;; scan and load directory recursively
(define (website:scan-directory db path)
  (if (file-exists? path)
    (let* ((i (file-info path))
           (t (file-info-type i)))
     (cond
       ((eq? t 'directory)
         (website:log 2 "=> scanning " path "..")
         (let loop ((fs (directory-files path)))
           (if (fx= (length fs) 0) #f (begin
             (website:scan-directory db (string-append path "/" (car fs)))
             (loop (cdr fs))))))
       ((eq? t 'regular)
         (let ((trimpath (string-append "/" (string-mapconcat (cdr (string-split (pregexp-replace* "//" path "/") #\/)) "/"))))
           (website:log 2 "==> adding " path " [" trimpath "]..")
           (table-set! db trimpath (website:compress (file->u8vector path)))
         )
       )
       (else
         (website:log 0 "** unsupported file type: " path "\n")
       )
     )
  )))

;; ------------------
;; render the website

(define (website:lookup-mimetype ext)
  (case (string->symbol ext)
    ((sxml html htm) "text/html")
    ((txt) "text/plain")
    ((css) "text/css")
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
            (begin (thread-start! (make-thread (lambda () (website:serve db connection) )))
     (loop)))))))

(define (website:serve db port)
  (with-exception-catcher (lambda (e) (close-port port) #f)
    (lambda () (let ((request (read-line port)))
       (if (string? request)
         (let* ((r (string-split request #\space))
                (m (car r)))
            (website:log 3 "website:serve got " request)
            (if (string-ci=? m "GET")
              (website:servefile db port
                (if (string=? (cadr r) "/") "/index.html" (cadr r))))
            (force-output port)
            (close-port port)
    ))))))

(define (website:servefile db port doc)
  (website:log 3 "serving " doc "..")
  (let* ((decomposit (string-split doc #\?))
         (document (car decomposit))
         (zdata (table-ref db document #f))
         (data (if (u8vector? zdata) (website:decompress zdata)
                 (if (procedure? zdata) (zdata (if (> (length decomposit) 1) (cadr decomposit) #f)) #f)))
         (ext (car (reverse (string-split document #\.)))))
    (if data
      (begin
        (display (string-append "HTTP/1.0 200 OK\ncontent-type: " (website:lookup-mimetype ext) "\n\n") port)
        (if (string=? ext "sxml")
          (sxml->xml (if (= (u8vector-ref data 0) 96) (eval (with-input-from-u8vector data read)) (with-input-from-u8vector data read)) port)
          (write-subu8vector data 0 (u8vector-length data) port)
        ))
      (begin
        (website:log 0 "!! unknown source: " document)
        (display "HTTP/1.0 400 OK\ncontent-type: text/plain\n\nInvalid Request\n" port))
   )))

;; ----------------

(define (website-serve db port) (thread-start! (make-thread (lambda () (website:server db port)))))

(define (website-addhook db document proc)
  (table-set! db document proc))

(define (website->table path)
  (let ((t (make-table)))
    (website:scan-directory t path)
    t))

(define make-website make-table)

;; eof 
