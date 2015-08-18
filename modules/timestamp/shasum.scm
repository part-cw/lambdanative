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

;; shasum functions of various lenghts

(c-declare #<<end-of-c-declare
#include <stdlib.h>
#include <stdio.h>
#include <openssl/sha.h>

int calc_sha512sum (unsigned char* path, unsigned char* hash){
  FILE *file = fopen(path, "rb");
  if(!file) return 0;
  SHA512_CTX sha512;
  SHA512_Init(&sha512);
  const int bufSize = 4096;
  char* buffer = malloc(bufSize);
  int bytesRead = 0;
  if (!buffer)
    return 0;
  while((bytesRead = fread(buffer, 1, bufSize, file))) {
    SHA512_Update(&sha512, buffer, bytesRead);
  }
  SHA512_Final(hash, &sha512);
  fclose(file);
  free(buffer);
  return SHA512_DIGEST_LENGTH;
}

int calc_sha256sum (unsigned char* path, unsigned char* hash){
  FILE *file = fopen(path, "rb");
  if(!file) return 0;

  SHA256_CTX sha256;
  SHA256_Init(&sha256);
  const int bufSize = 4096;
  char* buffer = malloc(bufSize);
  int bytesRead = 0;
  if (!buffer)
    return 0;
  while((bytesRead = fread(buffer, 1, bufSize, file))) {
    SHA256_Update(&sha256, buffer, bytesRead);
  }
  SHA256_Final(hash, &sha256);
  fclose(file);
  free(buffer);
  return SHA256_DIGEST_LENGTH;
}

int calc_sha1sum (unsigned char* path, unsigned char* hash){
  FILE *file = fopen(path, "rb");
  if(!file) return 0;

  SHA_CTX sha1;
  SHA1_Init(&sha1);
  const int bufSize = 4096;
  char* buffer = malloc(bufSize);
  int bytesRead = 0;
  if (!buffer)
    return 0;
  while((bytesRead = fread(buffer, 1, bufSize, file))) {
    SHA1_Update(&sha1, buffer, bytesRead);
  }
  SHA1_Final(hash, &sha1);
  fclose(file);
  free(buffer);
  return SHA256_DIGEST_LENGTH;
}
end-of-c-declare
)

;; Scheme bindings
(define (sha256sum filename)
  (let* ((vl ((c-lambda () int "___result=SHA256_DIGEST_LENGTH;")))
         (v (make-u8vector vl))
         (f ((c-lambda (char-string scheme-object) bool "___result=
           calc_sha256sum(___arg1,___CAST(unsigned char*,___BODY_AS(___arg2,___tSUBTYPED)));")
           filename v)))
    (if f v #f)
  ))
(define (sha512sum filename)
  (let* ((vl ((c-lambda () int "___result=SHA512_DIGEST_LENGTH;")))
         (v (make-u8vector vl))
         (f ((c-lambda (char-string scheme-object) bool "___result=
            calc_sha512sum(___arg1,___CAST(unsigned char*,___BODY_AS(___arg2,___tSUBTYPED)));")
            filename v)))
    (if f v #f)
  ))
(define (sha1sum filename)
  (let* ((vl ((c-lambda () int "___result=SHA_DIGEST_LENGTH;")))
         (v (make-u8vector vl))
         (f ((c-lambda (char-string scheme-object) bool "___result=
            calc_sha1sum(___arg1,___CAST(unsigned char*,___BODY_AS(___arg2,___tSUBTYPED)));")
            filename v)))
    (if f v #f)
  ))
(define shasum sha1sum)

;; eof
