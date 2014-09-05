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
;; Primitive sensor demonstrator for android and ios devices

(define gui #f)

(define xyzlabel #f)
(define gyrolabel #f)
(define gpslabel #f)

(main
;; initialization
  (lambda (w0 h0)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let* ((w (glgui-width-get))
           (h (glgui-height-get))
           (h2 (/ h 2.))
           (w2 (/ w 2.))
           (bw 150) (bh 50)
           (bx (/ (- w bw) 2.))
           (by (/ (- (/ h2 2.) bh) 2.)))
      (glgui-image gui 0 (+ h2 (/ h2 2)) w (/ h2 2) artwork.img White)
      (let ((wgt (glgui-label gui 0  (+ (/ h 2.) 50 30) w 16 "Accelerometer [X:Y:Z]" ascii_12.fnt White)))
        (glgui-widget-set! gui wgt 'align GUI_ALIGNCENTER))
      (set! xyzlabel (glgui-label gui 0 (+ (/ h 2.) 50) w 30 "::" ascii_30.fnt White))
      (glgui-widget-set! gui xyzlabel 'align GUI_ALIGNCENTER)
      (let ((wgt (glgui-label gui 0  (+ (/ h 2.) 0 30) w 16 "Gyroscope [Y:P:R]" ascii_12.fnt White)))
        (glgui-widget-set! gui wgt 'align GUI_ALIGNCENTER))
      (set! gyrolabel (glgui-label gui 0 (- (/ h 2.) 0.) w 30 ":" ascii_30.fnt White))
      (glgui-widget-set! gui gyrolabel 'align GUI_ALIGNCENTER)
      (let ((wgt (glgui-label gui 0  (+ (/ h 2.) -50 30) w 16 "GPS [LAT:LNG]" ascii_12.fnt White)))
        (glgui-widget-set! gui wgt 'align GUI_ALIGNCENTER))
      (set! gpslabel (glgui-label gui 0 (- (/ h 2.) 50 ) w 30 ":" ascii_30.fnt White))
      (glgui-widget-set! gui gpslabel 'align GUI_ALIGNCENTER)
      (glgui-button gui bx by bw bh exit.img (lambda (x . y) (force-terminate)))
    )
  )
;; events
  (lambda (t x y)
    (let ((curx (accel-x)) (cury (accel-y))(curz (accel-z))
          (curyaw (gyro-yaw)) (curpitch (gyro-pitch)) (curroll (gyro-roll))
          (curlng (gps-longitude)) (curlat (gps-latitude)))
      (glgui-widget-set! gui xyzlabel 'label 
        (string-append (float->choppedstring curx 6) ":"
          (float->choppedstring cury 6) ":" (float->choppedstring curz 6)))
      (glgui-widget-set! gui gyrolabel 'label 
        (string-append (float->choppedstring curyaw 6) ":"
          (float->choppedstring curpitch 6) ":" (float->choppedstring curroll 6)))
      (glgui-widget-set! gui gpslabel 'label 
        (string-append (float->choppedstring curlat 8) ":"
          (float->choppedstring curlng 8)))
      (if (= t EVENT_KEYPRESS) (begin 
        (if (= x EVENT_KEYESCAPE) (terminate))))
      (glgui-event gui t x y)))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
