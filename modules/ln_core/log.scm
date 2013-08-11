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
;; logger

;; we are logging from different threads
(define log:mutex (make-mutex))
(define (log:grab!) (mutex-lock! log:mutex))
(define (log:release!) (mutex-unlock! log:mutex))

(define log:verbose 0)

(define (log-verbose n) (set! log:verbose n))

(define log:path (string-append (system-directory) (system-pathseparator) "log"))

(set! log:file (string-append log:path (system-pathseparator) "log_"  
    (time->string (current-time) "%Y%m%d_%H%M%S") ".txt"))

;; 20101007: don't log if the directory doesn't exist
(define log:on (not (or (not (file-exists? (system-directory))) (not (file-exists? log:path)))))

(define log:hook #f)

;; general log submission
(define log:submit (lambda (t . s)
  (if log:on (begin
  (log:grab!)
  (let ((now (time->string (current-time) "%Y-%m-%d %T"))
        (str (with-output-to-string "" (lambda () (for-each display s)))))
    (with-output-to-file (list path: log:file append: #t)
       (lambda () (for-each display (list "[" t "] " now ": " str "\n"))))
    (if (procedure? log:hook) (apply log:hook (list t str)))
  )
  (log:release!)))))

(define (log-system s . x) (apply log:submit (append (list "SYSTEM" s) x)))

(define (log-status s . x) (apply log:submit (append (list "STATUS" s) x)))

(define (log-error  s . x) (apply log:submit (append (list "ERROR" s) x)))

(define (log-warning  s . x) (apply log:submit (append (list "WARNING" s) x)))

(define (log-debug  s v . x) 
  (if (fx>= v log:verbose) (apply log:submit (append (list "DEBUG" s) x))))

(define (log-folder-cleanup)
  (if log:on (begin
    (log:grab!)
    (let ((maxfiles 10))
       (let loop ((fs (sort (directory-files log:path) string>?))(n 0))
         (if (fx> (length fs) 0)
            (let* ((f (car fs))(match (string-contains f "log_")))
               (if (and match (fx>= n (fx- maxfiles 1))) (delete-file 
                 (string-append log:path (system-pathseparator) f)))
               (loop (cdr fs) (if match (fx+ n 1) n))))))
    (log:release!)
  )))

;; try to output location of continuation
;; this only works if debug information is available
(define (trace:identify cont)
  (let ((locat (##continuation-locat cont)))
    (if locat
        (let* ((container (##locat-container locat))
               (file (##container->path container)))
          (if file
              (let* ((filepos (##position->filepos (##locat-position locat)))
                     (line (##fixnum.+ (##filepos-line filepos) 1))
                     (col (##fixnum.+ (##filepos-col filepos) 1)))
                (log-error "trace: " file " line=" line " col=" col))
              #f))
        #f)))

(define (log-trace thread)
  (let* ((capture (##thread-continuation-capture thread)))
     (let cloop ((cont (##continuation-first-frame capture #f))(n 0))
        (if cont (let ((locat (##continuation-locat cont)))
        (if (and locat (> n 1)) (trace:identify cont))
      (cloop (##continuation-next-frame cont #f)(fx+ n 1)))))
 ))

(define (exception->string e)
  (let* ((str (with-output-to-string '() (lambda () (display-exception e (current-output-port)))))
         (tmp (string-split str #\newline)))
    (string-mapconcat (reverse tmp) ": ")))

(define (log:exception-handler e)
  (log-error (thread-name (current-thread)) ": "(exception->string e))
  (log-trace (current-thread))
  (log-error "HALT")
  (exit))

;; catch primordial thread exceptions
(current-exception-handler log:exception-handler)

;; catch exceptions in threads
(define (make-safe-thread p . name)
  (let ((p2 (lambda () (current-exception-handler log:exception-handler) (p))))
    (make-thread p2 (if (fx= (length name) 1) (car name) 'unnamed_thread))))

;; trim files in log directory
(log-folder-cleanup)

;; let's say hello to ourselves
(log-system "Application " (system-appname) " built " (system-builddatetime))
(log-system "Git hash " (system-buildhash))

;; eof
