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

;; minimal code to verify NIST P256 ECDSA signatures (e.g. used in the U2F standard)

(##namespace ("p256ecdsa#"))
(##include "~~lib/gambit#.scm")
(##include "p256ecdsa#.scm")
(##namespace ("" unit-test))

(c-declare #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "p256ecdsa/dsa_sig.h"
#include "p256ecdsa/p256.h"
#include "p256ecdsa/p256_ecdsa.h"
#include "p256ecdsa/sha256.h"

end-of-c-declare
)

(c-define-type p256_int "p256_int")
(c-define-type p256_int* (pointer p256_int))

(define make-p256-int (c-lambda () p256_int* "___result=(p256_int*)malloc(sizeof(p256_int));"))
(define free-p256-int (c-lambda (p256_int*) void "free(___arg1);"))

(define (p256-from-bin u8v)
  (let ((x (make-p256-int)))
    ((c-lambda (scheme-object p256_int*) void
       "p256_from_bin(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);") u8v x)
      x))

(define (dsa-sig-unpack sig)
  (let* ((r (make-p256-int))
         (s (make-p256-int))
         (sig-len (u8vector-length sig))
         (res ((c-lambda (scheme-object int p256_int* p256_int*) int
             "___result=dsa_sig_unpack(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,___arg3,___arg4);")
               sig sig-len r s)))
   (if (not (= 0 res)) (list r s) (begin
     (free-p256-int r) (free-p256-int s) #f))))

(define (sha256-hash data)
  (let ((len (u8vector-length data))
        (digest (make-u8vector 32)))
    ((c-lambda (scheme-object int scheme-object) void
         "SHA256_hash(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2, ___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)));")
            data len digest)
    digest))

(define (p256-from-hash data) (p256-from-bin (sha256-hash data)))

(define (p256-to-bin p256)
  (let ((data (make-u8vector 32)))
    ((c-lambda (p256_int* scheme-object) void 
       "p256_to_bin(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)));") p256 data)
    data))

;; ---------

(define (p256-ecdsa-verify-hash key-x key-y hash signature)
  (let* ((x (p256-from-bin key-x))
         (y (p256-from-bin key-y))
         (h (p256-from-bin hash))
         (s (dsa-sig-unpack signature))
         (args (append (list x y h) (if s s '())))
         (res (if s (apply (c-lambda (p256_int* p256_int* p256_int* p256_int* p256_int*) int "p256_ecdsa_verify") args) #f)))
   (for-each free-p256-int args)
   (if res (> res 0) res)))

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
