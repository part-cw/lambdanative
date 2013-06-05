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

(include "glgui_battery.scm")
;; If the image gets redone these x,y,w,h need replacing!
(define glgui:battery-inner-box '(8 3 28 12))

(define (glgui:battery-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (v (glgui-widget-get g wgt 'value))
   (lw (glgui-widget-get g wgt 'warning-level))
   (lr (glgui-widget-get g wgt 'red-level))
   (ly (glgui-widget-get g wgt 'yellow-level))
   (i glgui:battery-inner-box)
   (full (if (> v 100) 100 (if (< v 0) 0 v)))
   (iw (ceiling (* (caddr i) (/ full 100))))
   (ix (+ x (car i) (- (caddr i) iw) .5)) ;; The 0.5 makes it not jump back and forth
   (iy (+ y (cadr i)))
   (ih (cadddr i)))
  (glgui:draw-pixmap-left x y w h glgui_battery.img (if (< v 0) DarkGray (if (> v lw) White Red)))
  (if (>= v 5) (glgui:draw-box ix iy iw ih (if (> v ly) LightGray (if (> v lr) (color-shade Yellow 0.8) Red))))
))

(define (glgui:battery-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (if (and (procedure? cb) inside (= type EVENT_BUTTON1UP)) (cb g wgt type mx my))
   inside
))

(define (glgui-battery g x y w h v)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'callback #f
     'value v
     'hidden #f
     'warning-level 10
     'red-level 25
     'yellow-level 50
     'draw-handle  glgui:battery-draw
     'input-handle glgui:battery-input
  ))
;;eof