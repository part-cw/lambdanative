#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2026, University of British Columbia
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
#include <openssl/evp.h>
#include <openssl/sha.h>

static int calc_digest_sum(const char *path, unsigned char *hash,
                           const EVP_MD *md, unsigned int expected_len){
  if (path == NULL || hash == NULL || md == NULL)
    return 0;
  FILE *file = fopen(path, "rb");
  if (file == NULL)
    return 0;

  EVP_MD_CTX *ctx = EVP_MD_CTX_new();
  if (ctx == NULL) {
    fclose(file);
    return 0;
  }

  unsigned char buffer[4096];
  unsigned int digest_len = 0;
  int ok = EVP_DigestInit_ex(ctx, md, NULL) == 1;
  size_t bytes_read;
  while (ok && (bytes_read = fread(buffer, 1, sizeof(buffer), file)) > 0)
    ok = EVP_DigestUpdate(ctx, buffer, bytes_read) == 1;
  if (ferror(file))
    ok = 0;
  if (fclose(file) != 0)
    ok = 0;
  if (ok)
    ok = EVP_DigestFinal_ex(ctx, hash, &digest_len) == 1 &&
         digest_len == expected_len;
  EVP_MD_CTX_free(ctx);
  return ok ? (int)digest_len : 0;
}

int calc_sha512sum(const char *path, unsigned char *hash){
  return calc_digest_sum(path, hash, EVP_sha512(), SHA512_DIGEST_LENGTH);
}

int calc_sha256sum(const char *path, unsigned char *hash){
  return calc_digest_sum(path, hash, EVP_sha256(), SHA256_DIGEST_LENGTH);
}

int calc_sha1sum(const char *path, unsigned char *hash){
  return calc_digest_sum(path, hash, EVP_sha1(), SHA_DIGEST_LENGTH);
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
