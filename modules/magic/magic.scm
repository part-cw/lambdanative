#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
Copyright (c) 2019 by Joerg F. Wittenberger
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

;; minimal bindings to libmagic

(c-declare #<<EOF
#include <magic.h>
EOF
)

(define-macro (define-c-constant name type value)
  `(define ,name ((c-lambda () ,type ,(string-append "___result = " value ";")))))

(define-c-constant MAGIC_NONE int "MAGIC_NONE")
(define-c-constant MAGIC_DEBUG int "MAGIC_DEBUG")
(define-c-constant MAGIC_SYMLINK int "MAGIC_SYMLINK")
(define-c-constant MAGIC_COMPRESS int "MAGIC_COMPRESS")
(define-c-constant MAGIC_DEVICES int "MAGIC_DEVICES")
(define-c-constant MAGIC_MIME int "MAGIC_MIME")
(define-c-constant MAGIC_CONTINUE int "MAGIC_CONTINUE")
(define-c-constant MAGIC_CHECK int "MAGIC_CHECK")
(define-c-constant MAGIC_PRESERVE_ATIME int "MAGIC_PRESERVE_ATIME")
(define-c-constant MAGIC_RAW int "MAGIC_RAW")
(define-c-constant MAGIC_ERROR int "MAGIC_ERROR")

(define magic-open
  (c-lambda (int) (pointer void) "magic_open"))

(define (magic-load file)
  (define magic-load*
    (c-lambda ((pointer void) char-string) int "magic_load"))
  (cond
   ((or (string? file) (not file))
    (magic-load* file))
   ;; Load the embedded magic.mgc
   (else
    (let ((embedded (string-append
		     (system-directory) (system-pathseparator)
		     "lib" (system-pathseparator) "magic.mgc")))
      (if (not (magic-load* embedded))
	  (log-error "magic: couldn't load embedded magic.mgc"))))))

(define magic-error
  (c-lambda ((pointer void)) char-string "magic_error"))

(define magic-file
  (c-lambda ((pointer void) char-string) char-string "magic_file"))

(define (magic-buffer ptr u8data)
  ((c-lambda
    ((pointer void) scheme-object int)
    char-string
    "___result = (char*)magic_buffer(___arg1, ___BODY_AS(___arg2,___tSUBTYPED), ___arg3);")
   ptr u8data (u8vector-length u8data)))

(define magic-close
  (c-lambda ((pointer void)) void "magic_close"))

(define magic-setflags
  (c-lambda ((pointer void) int) int "magic_setflags"))

(define magic-errno
  (c-lambda ((pointer void)) int "magic_errno"))

;; eof
