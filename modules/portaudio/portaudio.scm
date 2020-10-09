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

;; utility functions to support portaudio configuration
;; can be used to configure audio and rtaudio modules

(c-declare  #<<end-of-c-declare

#include <portaudio.h>

int portaudio_needsinit=1;
int portaudio_idev=-1;
int portaudio_odev=-1;

end-of-c-declare
)

(define pa-idev-set! (c-lambda (int) void "portaudio_idev = ___arg1;"))
(define pa-odev-set! (c-lambda (int) void "portaudio_odev = ___arg1;"))

(define pa-devcount (c-lambda () int "
  if (portaudio_needsinit) { Pa_Initialize(); portaudio_needsinit=0; }
  ___result=Pa_GetDeviceCount();
  "))

(define pa-devinfo-name (c-lambda (int) char-string "
  if (portaudio_needsinit) { Pa_Initialize(); portaudio_needsinit=0; }
   PaDeviceInfo *deviceInfo = Pa_GetDeviceInfo( ___arg1);
  ___result=deviceInfo->name;
  "))

(define pa-devinfo-inputs (c-lambda (int) int "
  if (portaudio_needsinit) { Pa_Initialize(); portaudio_needsinit=0; }
   PaDeviceInfo *deviceInfo = Pa_GetDeviceInfo( ___arg1);
  ___result=deviceInfo->maxInputChannels;
  "))

(define pa-devinfo-outputs (c-lambda (int) int "
  if (portaudio_needsinit) { Pa_Initialize(); portaudio_needsinit=0; }
   PaDeviceInfo *deviceInfo = Pa_GetDeviceInfo( ___arg1);
  ___result=deviceInfo->maxOutputChannels;
  "))

(define (pa-devlist)
  (let ((n (pa-devcount)))
    (let loop ((i 0)(res '()))
      (if (= i n) res
        (loop (+ i 1) (append res (list
          (list i (pa-devinfo-name i) (pa-devinfo-inputs i) (pa-devinfo-outputs i)))))))))

(define (pa-devlist-outputs)
  (let loop ((ds (pa-devlist))(res '()))
    (if (fx= (length ds) 0) res
       (loop (cdr ds) (append res
         (if (> (cadddr (car ds)) 0) (list (car ds)) '()))))))

(define (pa-devlist-inputs)
  (let loop ((ds (pa-devlist))(res '()))
    (if (fx= (length ds) 0) res
       (loop (cdr ds) (append res
         (if (> (caddr (car ds)) 0) (list (car ds)) '()))))))

(define (pa-dev-exists? devname)
  (member devname (map cadr (pa-devlist))))

(define (pa-odev-exists? devname)
  (member devname (map cadr (pa-devlist-outputs))))

(define (pa-idev-exists? devname)
  (member devname (map cadr (pa-devlist-inputs))))

;; eof
