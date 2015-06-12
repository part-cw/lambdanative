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

(##namespace ("minizip#"))
(##include "~~lib/gambit#.scm")
(##include "minizip#.scm")
(##namespace ("" log-system system-directory system-pathseparator u8vector->string unit-test))

(define minizip:debuglevel 0)
(define (minizip:log level . x)
   (if (>= minizip:debuglevel level) (apply log-system (append (list "minizip: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <minizip.h>

end-of-c-declare
)

(define (pointer->u8vector ptr len)
  (let ((u8v (make-u8vector len)))
    ((c-lambda (scheme-object (pointer void) int) void
       "memcpy(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,___arg3); free(___arg2);")
       u8v ptr len) u8v))

(define (minizip-unpack-to-disk rootdir u8data)
  (minizip:log 2 "minizip-unpack-to-disk " rootdir " " u8data)
  ((c-lambda (char-string scheme-object int) int
     "___result=minizip_unpack_to_disk(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);")
    rootdir u8data (u8vector-length u8data)))

(define (minizip-unpack-to-memory u8data)
  (minizip:log 2 "minizip-unpack-to-memory " u8data)
  (let ((unpack_len (u32vector)))
    (pointer->u8vector ((c-lambda (scheme-object int scheme-object) (pointer void)
        "___result=minizip_unpack_to_memory(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
           ___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)));")
       u8data (u8vector-length u8data) unpack_len) (u32vector-ref unpack_len 0))))

;; ----------
;; unit test

(define minizip:test '#u8(80 75 3 4 10 0 0 0 0 0 206 96 204 70 42 136 95 80 8 0 0 0 8 0 0 0 6 0 28 0 116 101 115 116 109 101
    85 84 9 0 3 179 45 123 85 179 45 123 85 117 120 11 0 1 4 245 1 0 0 4 20 0 0 0 84 69 83 84 32 77 69 10
    80 75 1 2 30 3 10 0 0 0 0 0 206 96 204 70 42 136 95 80 8 0 0 0 8 0 0 0 6 0 24 0 0 0 0 0 1 0 0 0 164 129
    0 0 0 0 116 101 115 116 109 101 85 84 5 0 3 179 45 123 85 117 120 11 0 1 4 245 1 0 0 4 20 0 0 0 80
    75 5 6 0 0 0 0 1 0 1 0 76 0 0 0 72 0 0 0 0 0) )

(unit-test "minizip" "unpacking of embedded zips"
  (lambda () 
    (let ((testfile (string-append (system-directory) (system-pathseparator) "testme")))
      (minizip-unpack-to-disk (system-directory) minizip:test)
      (if (file-exists? testfile) 
        (let ((str (with-input-from-file testfile (lambda () (read-line)))))
          (delete-file testfile)
          (if (string=? str "TEST ME")
            (string=? (u8vector->string (minizip-unpack-to-memory minizip:test)) "TEST ME\n")
        #f)) #f))))

;; eof
