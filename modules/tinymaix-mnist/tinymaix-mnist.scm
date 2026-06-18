#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
Copyright (c) 2026, Benson Muite
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

;; minimal bindings for the TinyMaix inference library

;; (png->digit "some.png") will give probabilities for a digit
;; in an image using a model trained on the MNIST dataset.

(c-declare  #<<end-of-c-declare
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Copyright 2022 Sipeed Technology Co., Ltd. All Rights Reserved.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. */

#include <tinymaix.h>
#if TM_MDL_TYPE == TM_MDL_INT8
#include "mnist_valid_q.h"
#elif TM_MDL_TYPE == TM_MDL_FP32
#include "mnist_valid_f.h"
#elif TM_MDL_TYPE == TM_MDL_FP16
#include "mnist_valid_fp16.h"
#elif TM_MDL_TYPE == TM_MDL_FP8_143
#include "mnist_fp8_143.h"
#elif TM_MDL_TYPE == TM_MDL_FP8_152
#include "mnist_fp8_152.h"
#endif

static tm_err_t layer_cb(tm_mdl_t* mdl, tml_head_t* lh)
{   //dump middle result
    int h = lh->out_dims[1];
    int w = lh->out_dims[2];
    int ch= lh->out_dims[3];
    mtype_t* output = TML_GET_OUTPUT(mdl, lh);
    return TM_OK;
}

int _tinymaix_mnist_process(uint8_t pic[28*28], float probabilities[10])
{
  TM_DBGT_INIT();
  tm_mdl_t mdl;
  tm_mat_t in_uint8 = {3,28,28,1, {(mtype_t*)pic}};
  tm_mat_t in = {3,28,28,1, {NULL}};
  tm_mat_t outs[1];
  tm_err_t res;

  res = tm_load(&mdl, mdl_data, NULL, layer_cb, &in);
  if(res != TM_OK) {
    return -1;
  }
  TM_DBGT_START();
  res = tm_run(&mdl, &in, outs);
  tm_unload(&mdl);
  if(res==TM_OK) {
    tm_mat_t out = outs[0];
    float* data  = out.dataf;
    for(int i=0;i<10;i++) probabilities[i] = data[i];
  }else{
    return -1;
  }
  return 0;
}

/* End Apache-2.0 licensed code */

end-of-c-declare
)


(define tinymaix_mnist_process (c-lambda ((pointer void) (pointer void)) int
  "___result=_tinymaix_mnist_process(___arg1,___arg2);"))


(define (tinymaix_mnist:greyscale u8data w h)
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
      (else (log-error "tinymaix_mnist:greyscale: invalid factor [" factor "]") #f))))


(define (png->number pngfile)
  (if (file-exists? pngfile)
    (let* ((w   (png-width pngfile))
           (h   (png-height pngfile))
           (data (tinymaix_mnist:greyscale (png->u8vector pngfile) w h))
           (q   (make-u8vector 10 0.0))
           (ret (tinymaix_mnist_process data q))
          )
      q
      )
    (list "Image file is missing")
    ))

;; eof
