#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
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

;; minimal libuuid wrapper

(define uuid:debuglevel 0)
(define (uuid:log level . x) (if (fx>= uuid:debuglevel level) (apply log-system (append (list "uuid: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>

#ifndef WIN32
#include <ln_uuid/uuid.h>
#else
#include <rpc.h>
#endif

char *_make_uuid()
{
  static char str[37];
#ifndef WIN32
  uuid_t gen;
  uuid_generate(gen);
  uuid_unparse(gen, str);
#else
  int i=0,j=0;
  UUID uuid = {0};
  unsigned char* sz = 0;
  UuidCreate(&uuid);
  UuidToString(&uuid, &sz);
  while (1) {
    if (sz[i]>31) str[j++]=(char)sz[i];
      else if (sz[i]==0) { str[j]=0; break; }
    if (j>36) break;
    i++;
  }
  RpcStringFree(&sz);
#endif
  return str;
}

end-of-c-declare
)

(define (make-uuid)
  (uuid:log 1 "make-uuid")
  ((c-lambda () char-string "_make_uuid")))

(define (system-uuid)
  (let* ((uuidfile (string-append (system-directory) (system-pathseparator) "uuid"))
         (id (if (file-exists? uuidfile)
           (with-input-from-file uuidfile (lambda () (read-line)))
           (let ((newid (string-downcase (make-uuid))))
             (with-output-to-file uuidfile (lambda () (display newid) (newline))) newid))))
    id))

;; eof
