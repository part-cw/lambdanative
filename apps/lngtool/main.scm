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

;; lambdanative tool to embed source code

(define (usage)
  (for-each display (list
    "Usage: lngtool <language> <source file>\n"
    "outputs scheme code to standard out\n"))
  (exit))

(if (or (not (= (system-cmdargc) 3)) (not (file-exists?  (system-cmdargv 2)))) (usage))

(define datastring (apply string-append 
  (with-input-from-file (system-cmdargv 2) (lambda ()
    (let loop ((res '()))
      (let ((line (read-line)))
        (if (not (string? line)) res
          (loop (append res (list (string-append line "\n")))))))))))

(display ";; automatically generated. Do not edit.\n")
(write `(,(string->symbol (system-cmdargv 1)) (list->string (map integer->char (u8vector->list (u8vector-decompress
   ',(u8vector-compress (list->u8vector (map char->integer (string->list datastring))))
  ))))))

(newline)

;; eof
