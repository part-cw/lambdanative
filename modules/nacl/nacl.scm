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

;; minimal FFI interface to the NaCl networking and cryptography library
;; the exposed API subset supports both libsodium and libtweetnacl

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(WIN32) || defined(LINUX)
#define USE_TWEETNACL 1
#else
#define USE_SODIUM 1
#endif

#ifdef USE_TWEETNACL

#ifdef WIN32
#include <Windows.h>
#include <wincrypt.h>
#endif

#ifdef IOS
#include <Security/Security.h>
#endif

#include <assert.h>

void randombytes(unsigned char * ptr,unsigned int length)
{
  int err = 1;
#ifdef WIN32
  static HCRYPTPROV prov = 0;
  if (!prov) { if (!CryptAcquireContext(&prov, NULL, NULL, PROV_RSA_FULL, 0)) err = 1; }
  if (!err && !CryptGenRandom(prov, length, ptr)) err = 1;
#elif defined(IOS)
  err = (SecRandomCopyBytes(kSecRandomDefault,length,ptr)!=noErr);
#else  // generic unix approach - WARNING: THIS BLOCKS IF ENTRYOP POOL EMPTY
  FILE* fh = fopen("/dev/random", "rb");
  if (fh != NULL) {
    if (fread(ptr, length, 1, fh) == 0) err = 1;
    fclose(fh);
  } else { err = 1; }
#endif
  if (err) { assert("FATAL: randombytes() failed"); }
}

#include <tweetnacl.h>

#endif // USE_TWEETNACL

#ifdef USE_SODIUM
#include <sodium.h>
#endif // USE_SODIUM

end-of-c-declare
)

(define-macro (nacl-def arg)
  `(define ,arg ((c-lambda () int ,(string-append "___result=" (symbol->string arg) ";"))))
)

(nacl-def crypto_box_PUBLICKEYBYTES)
(nacl-def crypto_box_SECRETKEYBYTES)
(nacl-def crypto_box_NONCEBYTES)
(nacl-def crypto_box_ZEROBYTES)
(nacl-def crypto_box_BEFORENMBYTES)
(nacl-def crypto_box_BOXZEROBYTES)
(nacl-def crypto_scalarmult_SCALARBYTES)
(nacl-def crypto_scalarmult_BYTES)
(nacl-def crypto_sign_BYTES)
(nacl-def crypto_secretbox_KEYBYTES)
(nacl-def crypto_secretbox_NONCEBYTES)
(nacl-def crypto_secretbox_ZEROBYTES)
(nacl-def crypto_secretbox_BOXZEROBYTES)
(nacl-def crypto_stream_KEYBYTES)
(nacl-def crypto_stream_NONCEBYTES)
(nacl-def crypto_auth_BYTES)
(nacl-def crypto_auth_KEYBYTES)
(nacl-def crypto_onetimeauth_BYTES)
(nacl-def crypto_onetimeauth_KEYBYTES)
(nacl-def crypto_hash_BYTES)

(define-macro (nacl-fun sym str #!rest args)
  (define (nacl-ptr arg) (string=? (substring (symbol->string arg) 0 1) "*"))
  (define (nacl-cast arg) (if (string=? (substring (symbol->string arg) 1 2) "L") "unsigned long long*" "unsigned char*"))
  (let ((cl `((c-lambda ,(let loop ((as args)(res '()))
        (if (= (length as) 0) res
          (loop (cdr as) (append res (list (if (nacl-ptr (car as)) 'scheme-object 'unsigned-long-long))))))
         int ,(string-append "___result=" str "("
      (let loop ((n 1)(as args)(res ""))
        (if (= (length as) 0) res
          (let ((argn (string-append "___arg" (number->string n))))
             (loop (+ n 1) (cdr as) (string-append res
               (if (nacl-ptr (car as)) (string-append "___CAST(" (nacl-cast (car as)) ",___BODY_AS(") "")
               argn (if (nacl-ptr (car as)) ",___tSUBTYPED))" "") (if (< n (length args)) "," ""))))))
         ");")))))
    `(define ,(append (list sym) args) ,(append cl args))
  ))

(nacl-fun crypto-box "crypto_box" *c *m mlen *n *pk *sk)
(nacl-fun crypto-box-open "crypto_box_open" *m *c clen *n *pk *sk)
(nacl-fun crypto-box-afternm "crypto_box_afternm" *c *m mlen *n *k)
(nacl-fun crypto-box-beforenm "crypto_box_beforenm" *k *pk *sk)
(nacl-fun crypto-box-keypair "crypto_box_keypair" *pk *sk)
(nacl-fun crypto-core-salsa20 "crypto_core_salsa20" *out *in *k *c)
(nacl-fun crypto-core-hsalsa20 "crypto_core_hsalsa20" *out *in *k *c)
(nacl-fun crypto-verify-16 "crypto_verify_16" *x *y)
(nacl-fun crypto-verify-32 "crypto_verify_32" *x *y)
(nacl-fun crypto-stream-salsa20 "crypto_stream_salsa20" *c clen *n *k)
(nacl-fun crypto-stream-salsa20-xor "crypto_stream_salsa20_xor" *c *m mlen *n *k)
(nacl-fun crypto-stream-xor "crypto_stream_xor" *c *m mlen *n *k)
(nacl-fun crypto-stream "crypto_stream" *c clen *n *k)
(nacl-fun crypto-hash "crypto_hash" *out *in inlen)
(nacl-fun crypto-onetimeauth "crypto_onetimeauth" *out *in inlen *k)
(nacl-fun crypto-onetimeauth-verify "crypto_onetimeauth_verify" *h *in inlen *k)
(nacl-fun crypto-scalarmult-base "crypto_scalarmult_base" *q *n)
(nacl-fun crypto-scalarmult "crypto_scalarmult" *q *n *p)
(nacl-fun crypto-secretbox "crypto_secretbox" *c *m mlen *n *k)
(nacl-fun crypto-secretbox-open "crypto_secretbox_open" *m *c clen *n *k)
(nacl-fun crypto-sign-keypair "crypto_sign_keypair" *pk *sk)
(nacl-fun crypto-sign "crypto_sign" *sm *LLsmlen *m mlen *sk)
(nacl-fun crypto-sign-open "crypto_sign_open" *m *LLmlen *sm smlen *pk)

;; eof
