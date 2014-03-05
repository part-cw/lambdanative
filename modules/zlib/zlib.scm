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

(c-declare #<<c-declare-end

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <zlib.h>

int _zlib_compress(Bytef *ibuf, int ilen, Bytef *obuf, int olen, int compressionlevel)
{
  int res = 0;
  uLongf dstLen = (uLongf)olen;
  uLongf srcLen = (uLongf)ilen;
  if ( compress2(obuf,&dstLen,ibuf,srcLen,compressionlevel) == Z_OK) res = (int) dstLen;
  return res;
}

int _zlib_decompress(Bytef *ibuf, int ilen, Bytef *obuf, int olen)
{
  int res=0;
  uLongf dstLen = (uLongf)olen;
  uLongf srcLen = (uLongf)ilen;
  if ( uncompress(obuf, &dstLen, ibuf, srcLen) == Z_OK) res = (int) dstLen;
  return res;
}

c-declare-end
)

(define (zlib-u8vector-compress inbuf . compressionlevel)
  (if (u8vector? inbuf)
  (let* ((inlen (u8vector-length inbuf))
         (outlen (fix (+ (* 1.1 inlen) 12 1)))
         (outbuf (make-u8vector outlen))
         (clevel (if (fx= (length compressionlevel) 1) (car compressionlevel) 5)))
    (if (fx< inlen 16) (u8vector-append (u8vector 0) inbuf)
      (let ((retval ((c-lambda  (scheme-object int scheme-object int int) int
         "___result=_zlib_compress(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
              ___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4,___arg5);") inbuf inlen 
                outbuf outlen clevel)))
        (if (fx= retval 0) #f (u8vector-append (u8vector 1) (subu8vector outbuf 0 retval)))))) #f))

(define (zlib-u8vector-decompress inbuf)
  (if (u8vector? inbuf)
  (let ((flag (u8vector-ref inbuf 0))
        (inlen (u8vector-length inbuf)))
   (if (fx= flag 0) (subu8vector inbuf 1 inlen)
     (let expand ((outlen (* 5 inlen))) 
       (let* ((outbuf (make-u8vector outlen 0))
              (retval ((c-lambda  (scheme-object int scheme-object int) int
                 "___result=_zlib_decompress(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
                  ___CAST(void*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4);")
                 (subu8vector inbuf 1 inlen) (- inlen 1) outbuf outlen)))
       (if (> outlen (* inlen 10000)) (begin (log-error "zlib: decompression failed") #f)
         (if (> retval 0) (subu8vector outbuf 0 retval) (expand (* outlen 10)))))))) #f))

(unit-test "zlib-compress" "1000 random vectors (min compression)"
  (lambda () (let loop ((n 1000))
      (if (fx= n 0) #t (if (let* ((datalen (random-integer 100000))
           (data (random-u8vector datalen)))
          (not (equal? data (zlib-u8vector-decompress (zlib-u8vector-compress data))))
        ) #f (loop (fx- n 1)))))))

(unit-test "zlib-compress" "1000 empty vectors (max compression)"
  (lambda () (let loop ((n 1000))
      (if (fx= n 0) #t (if (let* ((datalen (random-integer 100000))
           (data (make-u8vector datalen 7)))
          (not (equal? data (zlib-u8vector-decompress (zlib-u8vector-compress data))))
        ) #f (loop (fx- n 1)))))))

;; eof
