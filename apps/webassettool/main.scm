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

;; tool to embed a conventional website into Scheme

(define (usage)
  (for-each display (list
    "Website to Scheme converter\n"
    "usage: webassettool [scm|web|repl] <website directory>\n"
    "  scm  : output scheme representation on stdout\n"
    "  web  : render website on port 8080\n"
    "  repl : launch repl after loading website\n"
  ))
  (exit))

(define args (let loop ((n 0)(res '()))
  (if (= n (system-cmdargc)) res (loop (+ n 1) (append res (list (system-cmdargv n)))))))

(if (< (length args) 3) (usage))

(define use-repl (member "repl" args))
(define use-web (member "web" args))
(define use-scm (member "scm" args))

(define db (website->table (string-append "." (system-pathseparator) (list-ref args 2))))

(if use-web (website-serve db 8080))

(if use-scm (begin
  (display ";; Automatically generated. Do not edit\n")
  (display "(website-merge! #f (list->table '")
  (write (table->list db))
  (newline)
  (display "))\n")
))

(if (or use-repl use-web)
  (let loop ()
    (with-exception-catcher (lambda (e)
      (for-each display (list (exception->string e) "\n")) #f)
        (lambda () (##repl-debug)))
    (loop))
)

;; eof
