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
;; simple example of multitouch handling (ios and android only)

(define gui #f)
(define mybox #f)

(main
;; initialization
  (lambda (w0 h0)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (set! mybox (glgui-box gui 0 0 0 0 Orange))
  )
;; events
  (lambda (t x y) 
    (if (and (= t EVENT_KEYPRESS) (= x EVENT_KEYESCAPE)) (terminate))
    (multitouch t x y)
    (if (multitouch?) 
      (let* ((x1 (multitouch-x 0))
             (y1 (multitouch-y 0))
             (x2 (multitouch-x 1))
             (y2 (multitouch-y 1))
             (x0 (min x1 x2))
             (y0 (min y1 y2))
             (w0 (abs (- x1 x2)))
             (h0 (abs (- y1 y2))))
        (glgui-widget-set! gui mybox 'x x0)
        (glgui-widget-set! gui mybox 'y y0)
        (glgui-widget-set! gui mybox 'w w0)
        (glgui-widget-set! gui mybox 'h h0)
      )
      (let* ((w (glgui-width-get))
             (h (glgui-height-get)))
        (glgui-widget-set! gui mybox 'x (/ w 4))
        (glgui-widget-set! gui mybox 'y (/ h 4))
        (glgui-widget-set! gui mybox 'w (/ w 2))
        (glgui-widget-set! gui mybox 'h (/ h 2)))
    )
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

