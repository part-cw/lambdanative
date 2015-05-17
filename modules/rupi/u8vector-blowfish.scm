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

;; Blowfish, a simple & reasonably fast symmetric cipher (albeit with high initialization overhead)
;; maximum key length is 56 bytes, minimum 4

(c-declare  #<<end-of-c-declare
#include <openssl/blowfish.h>

#ifndef u_int8_t
typedef unsigned char  u_int8_t;
#endif

// minimal glue for 16 distinct keys
static BF_KEY mykey[16];

void _blf_key(int idx, void *data, int len) {
  BF_set_key(&mykey[idx&15], len, (u_int8_t*)data);
}

void _blf_enc(int idx, void *data, int len) {
  u_int8_t *d=(u_int8_t*)data;
  int blocks=(len>>3);

  int i;
  for (i=0;i<blocks;i++){
    BF_ecb_encrypt(d,d,&mykey[idx&15],BF_ENCRYPT);
    d+=8;
  }
}
void _blf_dec(int idx, void *data, int len) {
  u_int8_t *d=(u_int8_t*)data;
  int blocks=(len>>3);

  int i;
  for (i=0;i<blocks;i++){
    BF_ecb_encrypt(d,d,&mykey[idx&15],BF_DECRYPT);
    d+=8;
  }
}

end-of-c-declare
)

;; maximum key length is 56 bytes, minimum 4
(define (u8vector-setkey-blowfish keyidx vect)
  ((c-lambda (int scheme-object int) void
     "_blf_key(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);")
         keyidx vect (u8vector-length vect)))

(define (u8vector-encrypt-blowfish keyidx vect)
  (if (u8vector? vect)
    (let* ((padlen1 (fx- 8 (modulo (fx+ 1 (u8vector-length vect)) 8)))
           (padlen (if (fx= padlen1 8) 0 padlen1))
           (pad (random-u8vector padlen))
           (tmp (u8vector-append (u8vector padlen) vect pad)))
      ((c-lambda (int scheme-object int) void
        "_blf_enc(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);") keyidx tmp (u8vector-length tmp))
      tmp
    )
    #f
  ))

(define (u8vector-decrypt-blowfish keyidx vect)
  (if (u8vector? vect)
    (let ((tmp (u8vector-copy vect)))
      ((c-lambda (int scheme-object int) void
        "_blf_dec(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)), ___arg3);") keyidx tmp (u8vector-length tmp))
      (let ((padlen (u8vector-ref tmp 0)))
        (if (fx> padlen (fx- (u8vector-length tmp) 1))
          #f
          (subu8vector tmp 1 (fx- (u8vector-length tmp) padlen))
        )
      )
    )
    #f
  ))

;; unit tests
;; -----------------

(unit-test "u8vector-blowfish" "1000 random vectors"
  (lambda () (let loop ((n 1000))
    (if (fx= n 0)
      #t
      (if (let* ((keyidx (random-integer 16))
                 (keylen (+ (random-integer 52) 4))
                 (key (random-u8vector keylen))
                 (datalen (random-integer 100000))
                 (data (random-u8vector datalen)))
            (u8vector-setkey-blowfish keyidx key)
            (not (equal? data (u8vector-decrypt-blowfish keyidx (u8vector-encrypt-blowfish keyidx data)))))
        #f
        (loop (fx- n 1)))
    ))
  ))
;; eof
