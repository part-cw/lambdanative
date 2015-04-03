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

;; rotating cube example

(define alpha 0.)
(define alphaincrement 0.5)
(define beta 30.)
(define betaincrement 0.)

(define cube_vertices `(
  ((-1 -1 1) (1 -1 1) (-1 1 1) (1 1 1)) ;; front
  ((1 -1 1) (1 -1 -1) (1 1 1) (1 1 -1)) ;; right
  ((1 -1 -1) (-1 -1 -1) (1 1 -1) (-1 1 -1)) ;; back
  ((-1 -1 -1) (-1 -1 1) (-1 1 -1) (-1 1 1)) ;; left
  ((-1 -1 -1) (1 -1 -1) (-1 -1 1) (1 -1 1)) ;; bottom
  ((-1 1 1) (1 1 1) (-1 1 -1) (1 1 -1)) ;; top
))

(define cube_colors `(,Red ,Green ,Blue ,Orange ,Cyan ,Yellow))

;; custom OpenGL rendering hook
(define (render-custom)
  (glClearColor 0. 0. 0. 0.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glFrustum -1.0 1.0 -1.0 1.0 1.5 20.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glEnable GL_CULL_FACE)
  (glCullFace GL_BACK)
  (glEnable GL_DEPTH_TEST)
  (glDepthMask GL_TRUE)
  (glTranslatef 0 0 -5)
  (glRotatef alpha 0 1 0)
  (glRotatef beta 1 0 0)
  (let loop ((vs cube_vertices)(cs cube_colors))
    (if (> (length vs) 0) (begin
      (glCoreColor (car cs))
      (glCoreBegin GL_TRIANGLE_STRIP)
      (map (lambda (l) (apply glCoreVertex3f l)) (car vs))
      (glCoreEnd)
      (loop (cdr vs) (cdr cs)))))
  (set! alpha (+ alpha alphaincrement))
  (set! beta (- beta betaincrement)))

(define gui #f)

(define (box-callback g wgt t x y)
  (let ((ox (glgui-widget-get g wgt 'offsetx))
        (oy (glgui-widget-get g wgt 'offsety))
        (w (glgui-widget-get g wgt 'w))
        (h (glgui-widget-get g wgt 'h)))
    (set! alphaincrement (/ ox w))
    (set! betaincrement (/ oy h))
  ))

(define (button-callback g wgt t x y)
  (set! alphaincrement (- alphaincrement))
  (set! betaincrement (- betaincrement)))

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let* ((w (glgui-width-get))
           (h (glgui-height-get)))
      (set! mybox (glgui-box-dragable gui 80 110 160 230 Red box-callback))
      (glgui-widget-set! gui mybox 'draw-handle #f) ;; hides the box
      (glgui-button-string gui 10 10 (- w 20) 32 "Reverse" ascii_16.fnt button-callback)
    )
    (glCore-registerhook render-custom)
  )
;; events
  (lambda (t x y) 
    (if (= t EVENT_KEYPRESS) (begin 
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-event gui t x y)
  )
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
