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

;; bindings for The Fastest Fourier Transform in the South (FFTS) library
(include "fft-tests.scm")

(c-declare  #<<end-of-c-declare

#include <string.h>
#include "ffts/ffts.h"

int fft_sign(float *data, float *data_out, int n, int sign){
  ffts_plan_t *fft = ffts_init_1d(n, sign);
  if(!fft) return 0;
  float *in = malloc(sizeof(float)*n*2);
  float *out = malloc(sizeof(float)*n*2);
  memcpy(in, data, sizeof(float)*n*2);
  memcpy(out, data_out, sizeof(float)*n*2);
  ffts_execute(fft, in, out);
  memcpy(data_out, out, sizeof(float)*n*2);
  ffts_free(fft);
  free(in);
  free(out);
  if (sign == 1){
    int i;
    for (i=0;i<n*2;i++){
      data_out[i]/=n;
    }
  }
  return 1;
}

int fft2_sign(float *data, float *data_out, size_t n, size_t n2, int sign){
  ffts_plan_t *fft = ffts_init_2d(n, n2, sign);
  if(!fft) return 0;
  float *in = malloc(sizeof(float)*n*n2*2);
  float *out = malloc(sizeof(float)*n*n2*2);
  memcpy(in, data, sizeof(float)*n*n2*2);
  memcpy(out, data_out, sizeof(float)*n*n2*2);
  ffts_execute(fft, in, out);
  memcpy(data_out, out, sizeof(float)*n*n2*2);
  ffts_free(fft);
  free(in);
  free(out);
  if (sign == 1){
    int i;
    for (i=0;i<n*2;i++){
      data_out[i]/=n*n2;
    }
  }
  return 1;
}

int fftn_sign(float *data, float *data_out, int rank, size_t *Ns, int sign){
  ffts_plan_t *fft = ffts_init_nd(rank, Ns, sign);
  if(!fft) return 0;
  ffts_execute(fft, data, data_out);
  ffts_free(fft);
  return 1;
}

end-of-c-declare
)

(define (fft_sign lst sign)
  (let* ((infl (list->f32vector (apply append (map (lambda (c) (list (flo (real-part c)) (flo (imag-part c)))) lst))))
         (outfl (make-f32vector (f32vector-length infl) 0.)))
    (if ((c-lambda (scheme-object scheme-object int int) bool
      "___result=fft_sign(___CAST(float*,___BODY_AS(___arg1,___tSUBTYPED)),
                ___CAST(float*,___BODY_AS(___arg2,___tSUBTYPED)),
                ___arg3,___arg4);")
           infl outfl (length lst) sign)
      (let loop ((lst (f32vector->list outfl)) (ret (list)))
        (if (null? lst)
          ret
          (loop (cddr lst) (append ret (list (+ (car lst) (* (cadr lst) +i)))))
        ))
      #f
    )
  ))
(define (fft lst) (fft_sign lst -1))
(define (ifft lst) (fft_sign lst 1))

(define (fft2_sign lstlst sign)
  (let* ((rows (length lstlst))
         (cols (length (car lstlst)))
         (infl (list->f32vector (apply append (map (lambda (c)
                 (list (flo (real-part c)) (flo (imag-part c)))) (listlist-flatten lstlst)))))
         (outfl (make-f32vector (f32vector-length infl) 0.)))
    (if ((c-lambda (scheme-object scheme-object int int int) bool
      "___result=fft2_sign(___CAST(float*,___BODY_AS(___arg1,___tSUBTYPED)),
                ___CAST(float*,___BODY_AS(___arg2,___tSUBTYPED)),
                ___arg3,___arg4,___arg5);")
           infl outfl rows cols sign)
      (let loop ((lst (f32vector->list outfl)) (ret (list)))
        (if (null? lst)
          (let loopr ((r 0) (retm '()))
            (if (fx= r rows)
              retm
              (loopr (fx+ r 1) (append retm (list
                (let loopc ((c 0) (reti '()))
                  (if (fx= c cols)
                    reti
                    (loopc (fx+ c 1) (append reti (list (list-ref ret (+ (* r cols) c)))))
                  )
                )
              )))
            ))
          (loop (cddr lst) (append ret (list (+ (car lst) (* (cadr lst) +i)))))
        ))
      #f
    )
  ))
(define (fft2 lstlst) (fft2_sign lstlst -1))
(define (ifft2 lstlst) (fft2_sign lstlst 1))


;; eof
