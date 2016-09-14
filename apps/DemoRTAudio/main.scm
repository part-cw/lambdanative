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
;; real time audio example (sine wave and echo)

(c-declare  #<<end-of-c-declare

#include <math.h>

void rtaudio_register(void (*)(int), void (*)(float), void (*)(float*,float*));

int mode=0;
double srate=0;

float buffer;

void my_realtime_init(int samplerate) { srate=(double)samplerate; buffer=0; }  

#define RINGSZE 10000
static float ring[RINGSZE];
int ring_in=0, ring_out=0;

void my_realtime_input(float v) 
{ 
  if (mode!=0) {
    buffer=v; 
    ring[ring_in++]=v;
    if (ring_in==RINGSZE) ring_in=0;
  }
}  

void my_realtime_output(float *v1,float *v2) 
{ 
  static double t=0;
  if (mode==0) {
    buffer = 0.95*sin(2*M_PI*440.*t);
    *v1=*v2=(float)buffer;
  } else {
    *v1=*v2=ring[ring_out++];
    if (ring_out==RINGSZE) ring_out=0;
  }
  t+=1/srate;
} 

end-of-c-declare
)

(c-initialize "rtaudio_register(my_realtime_init,my_realtime_input,my_realtime_output);")

(define rtdemo-mode (c-lambda (int) void "mode=___arg1;"))
(define rtdemo-buffer (c-lambda () double "___result=buffer;"))

(define gui #f)
(define mybox #f)

(main
;; initialization
  (lambda (w0 h0)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let ((w (glgui-width-get))
          (h (glgui-height-get)))
      (set! mybox (glgui-box gui 0 44 w (- h 44 44) Black))
      (glgui-menubar gui 0 (- h 44) w 44)
      (glgui-menubar gui 0 0 w 44)
      (let ((wgt (glgui-image gui 0 (- h 44) w 44 title.img White)))
        (glgui-widget-set! gui wgt 'align GUI_ALIGNCENTER))
      (glgui-button-string gui (/ w 4) (/ h 2) (/ w 2) 32 '("Sine" "Echo") ascii_18.fnt
        (lambda (g w t x y)
          (let ((mode (glgui-widget-get g w 'value)))
            (rtdemo-mode mode))))
    ) 
    (rtaudio-start 8000 0.5)
    (let ((logdir (string-append (system-directory) "/log")))
     (if (not (file-exists? logdir)) (create-directory logdir)))
  )
;; events
  (lambda (t x y) 
    (if (= t EVENT_KEYPRESS) (begin 
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-widget-set! gui mybox 'color (color-shade Red (abs (rtdemo-buffer))))
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend) (terminate))
;; resume
  (lambda () (glgui-resume))
)

;; eof
