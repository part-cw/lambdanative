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

;; minimal code to verify NIST P256 ECDSA signatures (e.g. used in the U2F standard)

(##namespace ("p256ecdsa#"))
(##include "~~lib/gambit#.scm")
(##include "p256ecdsa#.scm")
(##namespace ("" unit-test))

(c-declare #<<end-of-c-declare

#include <openssl/ec.h>
#include <openssl/ecdsa.h>
#include <openssl/obj_mac.h>
#include <openssl/sha.h>

static int p256_ecdsa_verify_hash(const unsigned char *key_x, int key_x_len,
                                  const unsigned char *key_y, int key_y_len,
                                  const unsigned char *hash, int hash_len,
                                  const unsigned char *signature, int signature_len) {
  int result = -1;
  EC_KEY *key = NULL;
  EC_POINT *public_key = NULL;
  BIGNUM *x = NULL;
  BIGNUM *y = NULL;
  ECDSA_SIG *parsed_signature = NULL;
  const unsigned char *signature_cursor = signature;

  if (key_x_len != 32 || key_y_len != 32 || hash_len != SHA256_DIGEST_LENGTH ||
      signature_len <= 0)
    return -1;

  key = EC_KEY_new_by_curve_name(NID_X9_62_prime256v1);
  if (key == NULL)
    goto cleanup;

  public_key = EC_POINT_new(EC_KEY_get0_group(key));
  x = BN_bin2bn(key_x, key_x_len, NULL);
  y = BN_bin2bn(key_y, key_y_len, NULL);
  parsed_signature = d2i_ECDSA_SIG(NULL, &signature_cursor, signature_len);
  if (public_key == NULL || x == NULL || y == NULL || parsed_signature == NULL ||
      signature_cursor != signature + signature_len ||
      EC_POINT_set_affine_coordinates_GFp(EC_KEY_get0_group(key), public_key, x, y, NULL) != 1 ||
      EC_KEY_set_public_key(key, public_key) != 1 || EC_KEY_check_key(key) != 1)
    goto cleanup;

  result = ECDSA_do_verify(hash, hash_len, parsed_signature, key);

cleanup:
  ECDSA_SIG_free(parsed_signature);
  BN_free(x);
  BN_free(y);
  EC_POINT_free(public_key);
  EC_KEY_free(key);
  return result;
}

end-of-c-declare
)

(define (sha256-hash data)
  (let ((digest (make-u8vector 32)))
    ((c-lambda (scheme-object int scheme-object) void
       "SHA256(___CAST(const unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
     data (u8vector-length data) digest)
    digest))

(define p256-ecdsa-verify-hash-raw
  (c-lambda (scheme-object int scheme-object int scheme-object int scheme-object int) int
    "___result=p256_ecdsa_verify_hash(
      ___CAST(const unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
      ___CAST(const unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4,
      ___CAST(const unsigned char*,___BODY_AS(___arg5,___tSUBTYPED)),___arg6,
      ___CAST(const unsigned char*,___BODY_AS(___arg7,___tSUBTYPED)),___arg8);"))

(define (p256-ecdsa-verify-hash key-x key-y hash signature)
  (let ((result (p256-ecdsa-verify-hash-raw key-x (u8vector-length key-x) 
          key-y (u8vector-length key-y)
          hash (u8vector-length hash) 
          signature (u8vector-length signature))))
    (cond ((= result 1) #t)
          ((= result 0) #f)
          (else #f))))

(define (p256-ecdsa-verify-msg key-x key-y msg signature)
  (p256-ecdsa-verify-hash key-x key-y (sha256-hash msg) signature))

;; ---------
;; unit test

(define key-x '#u8(103 14 213 0 232 192 93 132 69 186 22 86 96 24 35 106 23 78 190 76 28 2 120 198 147 20 22 202 214 86 250 36))
(define key-y '#u8(181 132 141 217 24 77 184 233 121 141 145 175 72 27 16 32 143 105 255 138 159 140 185 146 76 22 130 125 13 23 154 222))

(define message '#u8(
  #xf4 #x5d #x55 #xf3 #x55 #x51 #xe9 #x75 #xd6 #xa8 #xdc #x7e #xa9 #xf4 #x88 #x59
  #x39 #x40 #xcc #x75 #x69 #x4a #x27 #x8f #x27 #xe5 #x78 #xa1 #x63 #xd8 #x39 #xb3
  #x40 #x40 #x84 #x18 #x08 #xcf #x9c #x58 #xc9 #xb8 #x72 #x8b #xf5 #xf9 #xce #x8e
  #xe8 #x11 #xea #x91 #x71 #x4f #x47 #xba #xb9 #x2d #x0f #x6d #x5a #x26 #xfc #xfe
  #xea #x6c #xd9 #x3b #x91 #x0c #x0a #x2c #x96 #x3e #x64 #xeb #x18 #x23 #xf1 #x02
  #x75 #x3d #x41 #xf0 #x33 #x59 #x10 #xad #x3a #x97 #x71 #x04 #xf1 #xaa #xf6 #xc3
  #x74 #x27 #x16 #xa9 #x75 #x5d #x11 #xb8 #xee #xd6 #x90 #x47 #x7f #x44 #x5c #x5d
  #x27 #x20 #x8b #x2e #x28 #x43 #x30 #xfa #x3d #x30 #x14 #x23 #xfa #x7f #x2d #x08
  #x6e #x0a #xd0 #xb8 #x92 #xb9 #xdb #x54 #x4e #x45 #x6d #x3f #x0d #xab #x85 #xd9
  #x53 #xc1 #x2d #x34 #x0a #xa8 #x73 #xed #xa7 #x27 #xc8 #xa6 #x49 #xdb #x7f #xa6
  #x37 #x40 #xe2 #x5e #x9a #xf1 #x53 #x3b #x30 #x7e #x61 #x32 #x99 #x93 #x11 #x0e
  #x95 #x19 #x4e #x03 #x93 #x99 #xc3 #x82 #x4d #x24 #xc5 #x1f #x22 #xb2 #x6b #xde
  #x10 #x24 #xcd #x39 #x59 #x58 #xa2 #xdf #xeb #x48 #x16 #xa6 #xe8 #xad #xed #xb5
  #x0b #x1f #x6b #x56 #xd0 #xb3 #x06 #x0f #xf0 #xf1 #xc4 #xcb #x0d #x0e #x00 #x1d
  #xd5 #x9d #x73 #xbe #x12
))

(define signature '#u8(
  #x30 #x44 #x02 #x20 #x43 #x18 #xfc #xeb #x3b #xa8 #x3a #xa8 #xa3 #xcf #x41 #xb7
  #x81 #x4a #xf9 #x01 #xe1 #x8b #x6e #x95 #xc1 #x3a #x83 #x25 #x9e #xa5 #x2e #x66
  #x7c #x98 #x25 #xd9 #x02 #x20 #x54 #xf3 #x7f #x5a #xe9 #x36 #x9c #xa2 #xf0 #x51
  #xe0 #x6e #x78 #x48 #x60 #xa3 #xf9 #x8a #xd5 #x2c #x37 #x5a #x0a #x29 #xc9 #xf7
  #xea #x57 #x7e #x88 #x46 #x12
))

(unit-test "p256ecdsa" "verify ECDSA signature"
  (lambda () (p256-ecdsa-verify-msg key-x key-y message signature)))

;; eof
