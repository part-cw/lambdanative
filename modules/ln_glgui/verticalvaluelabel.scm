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

;; vertical value label (right aligned value, with right aligned label below)
;; ----------------

(define (glgui:verticalvaluelabel-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (c (glgui-widget-get-dyn g wgt 'color))
         (tc (glgui-widget-get-dyn g wgt 'titlecolor))
         (label (glgui-widget-get g wgt 'label))
         (fnt (glgui-widget-get g wgt 'font))
         (img (glgui-widget-get g wgt 'image))
         (ih (cadr img))
         (fh 0))
;;         (fh (if label (cadr (glgui:stringheight label fnt)) (glgui:fontheight fnt)))
;;         (fh (cadr (cadr (car fnt))))
    (if label (glgui:draw-text-right (- x 200) y 200 fh label fnt c))
    (glgui:draw-pixmap-right  (- x 200) (- y ih) 200 ih img (if label tc (color-fade tc 0.5)))
  ))

;M @deffn {procedure} glgui-verticalvaluelabel g x y img fnt color
(define (glgui-verticalvaluelabel g x y img fnt color)
  (glgui-widget-add g
     'x x
     'y y
     'font fnt
     'label #f
     'image img
     'callback #f
     'color color
     'titlecolor White
     'hidden #f
     'draw-handle  glgui:verticalvaluelabel-draw
     'input-handle #f
  ))

;; eof