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

;; minimal bindings for the libquirc QR code decoder

;; (png->qrcodes "some.png") will output list of QR codes in image

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <quirc.h>

static struct quirc_data qd;
static struct quirc_code qc;

int _quirc_setup(struct quirc *q, int w0, int h0, unsigned char *src)
{
  int w,h;
  int res = quirc_resize(q,w0,h0);
  if (res==QUIRC_SUCCESS) {
    unsigned char *dst = quirc_begin(q,&w,&h);
    if (w==w0&&h==h0) { 
      // png representation is flipped
      int i;
      for (i=0;i<h;i++) { memcpy(&dst[(h-i-1)*w],&src[i*w],w); }
    }
    quirc_end(q);
  }
  return res;
}

end-of-c-declare
)

(define QUIRC_SUCCESS ((c-lambda () int "___result = QUIRC_SUCCESS;")))
(define QUIRC_ERROR_INVALID_GRID_SIZE ((c-lambda () int "___result = QUIRC_ERROR_INVALID_GRID_SIZE;")))
(define QUIRC_ERROR_INVALID_VERSION ((c-lambda () int "___result = QUIRC_ERROR_INVALID_VERSION;")))
(define QUIRC_ERROR_FORMAT_ECC ((c-lambda () int "___result = QUIRC_ERROR_FORMAT_ECC;")))
(define QUIRC_ERROR_UNKNOWN_DATA_TYPE ((c-lambda () int "___result = QUIRC_ERROR_UNKNOWN_DATA_TYPE;")))
(define QUIRC_ERROR_DATA_OVERFLOW ((c-lambda () int "___result = QUIRC_ERROR_DATA_OVERFLOW;")))
(define QUIRC_ERROR_DATA_UNDERFLOW ((c-lambda () int "___result = QUIRC_ERROR_DATA_UNDERFLOW;")))

(define quirc_new (c-lambda () (pointer void) "quirc_new"))
(define quirc_destroy (c-lambda ((pointer void)) void "quirc_destroy"))

;;(define quirc_resize (c-lambda ((pointer void) int int) int "quirc_resize"))
;;(define quirc_end (c-lambda ((pointer void)) void "quirc_end"))

(define quirc_setup (c-lambda ((pointer void) int int scheme-object) int
   "___result=_quirc_setup(___arg1,___arg2,___arg3,___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)));"))

(define quirc_count (c-lambda ((pointer void)) int "quirc_count"))

(define quirc_extract_and_decode (c-lambda ((pointer void) int) int 
  "quirc_extract(___arg1,___arg2,&qc); ___result=quirc_decode(&qc,&qd);"))

(define quirc_decoded_len (c-lambda () int "___result=qd.payload_len;"))

(define quirc_decoded_data 
   (c-lambda (scheme-object) void 
      "memcpy(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),qd.payload,qd.payload_len);"))

(define (quirc_decode q)
  (let ((n (quirc_count q)))
    (let loop ((i 0)(res '()))
      (if (= i n) res 
        (let* ((decoderes (quirc_extract_and_decode q i))
               (u8len (if (fx= decoderes QUIRC_SUCCESS) (quirc_decoded_len)
                          (begin
                            (log-debug "Quirc image nr " 1 n ": " ((c-lambda (int) char-string "quirc_strerror") decoderes))
                            #f)))
               (u8data (if u8len (make-u8vector u8len 0) #f)))
          (if u8data (quirc_decoded_data u8data))
          (loop (fx+ i 1) (append res (if u8data (list u8data) '()))))))))

(define (quirc:greyscale u8data w h)
  (let* ((u8len (u8vector-length u8data))
        (pixlen (* w h))
        (factor (fix (/ u8len pixlen))))
    (cond
      ((= factor 1) u8data)
      ((= factor 3) 
         (let ((gdata (make-u8vector pixlen)))
           (let loop ((i 0))
              (if (fx= i pixlen) gdata 
                (let ((r (u8vector-ref u8data (fx* i 3)))
                      (g (u8vector-ref u8data (fx+ (fx* i 3) 1)))
                      (b (u8vector-ref u8data (fx+ (fx* i 3) 2))))
                  (u8vector-set! gdata i (fix (/ (+ r g b) 3)))
                  (loop (fx+ i 1)))))))
      ((= factor 4) 
         (let ((gdata (make-u8vector pixlen)))
           (let loop ((i 0))
              (if (fx= i pixlen) gdata 
                (let ((r (u8vector-ref u8data (fx* i 4)))
                      (g (u8vector-ref u8data (fx+ (fx* i 4) 1)))
                      (b (u8vector-ref u8data (fx+ (fx* i 4) 2))))
                  (u8vector-set! gdata i (fix (/ (+ r g b) 3)))
                  (loop (fx+ i 1)))))))
      (else (log-error "quirc:greyscale: invalid factor [" factor "]") #f))))

(define (png->qrcodes pngfile)
  (if (file-exists? pngfile) 
    (let* ((w   (png-width pngfile))
           (h   (png-height pngfile))
           (data (quirc:greyscale (png->u8vector pngfile) w h))
           (q   (quirc_new))
           (res (begin (quirc_setup q w h data) (quirc_decode q))))
      (quirc_destroy q) (map u8vector->string res)) #f))

;; eof
