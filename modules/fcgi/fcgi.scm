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

;; support FastCGI

;; Note that FCGI changes the behaviour of stdout
;; normal output operations go to the webserver log

(c-declare #<<c-declare-end

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <fcgi_stdio.h>

c-declare-end
)

(define FCGI-Accept (c-lambda () int "FCGI_Accept"))

(define (FCGI-display x)
  ((c-lambda (char-string) void "printf")
    (with-output-to-string "" (lambda () (display x)))))

(define FCGI-Finish (c-lambda () void "FCGI_Finish"))

(define (FCGI-getenv s) (getenv s ""))

(define (fcgi-server init-proc session-proc close-proc)
  (if init-proc (init-proc))
  (let loop ()
    (if (>= (FCGI-Accept) 0) (begin
      (cgi-env-init)
      (let ((outp (with-output-to-string "" (lambda ()
              (if session-proc (session-proc) (for-each display (list
                 "Content-Type: text/plain\n\n"
                 "No session hook?\n")))))))
        (cgi-env-clear)
        (FCGI-display outp)
        (FCGI-Finish))) (thread-sleep! 0.1)) 
      (loop))
  (if close-proc (close-proc)) ;; not reached
 )

;; override cgi-server for automatic CGI->FastCGI conversion
(set! cgi-server fcgi-server)

;; eof
