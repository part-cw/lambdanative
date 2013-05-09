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
;; image widget

(define (glgui:image-draw g wgt)
  (let ((x (glgui-widget-get-dyn g wgt 'x))
        (y (glgui-widget-get-dyn g wgt 'y))
        (w (glgui-widget-get-dyn g wgt 'w))
        (h (glgui-widget-get-dyn g wgt 'h))
        (color (glgui-widget-get-dyn g wgt 'color))
        (bgcolor (glgui-widget-get-dyn g wgt 'bgcolor))
        (img (glgui-widget-get g wgt 'image))
        (align (glgui-widget-get g wgt 'align)))
    (if bgcolor (glgui:draw-box x y w h bgcolor))
    (if img
     ((cond ((= align GUI_ALIGNLEFT) glgui:draw-pixmap-left)
            ((= align GUI_ALIGNRIGHT) glgui:draw-pixmap-right)
            (else glgui:draw-pixmap-center)) x y w h img color))
  ))

(define (glgui:image-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
   (if (and inside cb (= type EVENT_BUTTON1UP)) (cb g wgt))
  inside
))

(define (glgui-image g x y w h image color . bgcolor)
  (glgui-widget-add g
     'x x
     'y y
     'w w 
     'h h
     'image image
     'callback #f
     'color color
     'hidden #f
     'bgcolor (if (fx= (length bgcolor) 1) (car bgcolor) #f)
     'align GUI_ALIGNCENTER
     'draw-handle  glgui:image-draw
     'input-handle glgui:image-input
  ))

;; eof
