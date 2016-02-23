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
;; pixmap widget

(define (glgui:pixmap-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (i (glgui-widget-get g wgt 'image))
         (sw (glgui-widget-get-dyn g wgt 'sw))
         (sh (glgui-widget-get-dyn g wgt 'sh))
         (w (if sw sw (car i)))
         (h (if sh sh (cadr i)))
         (cb (glgui-widget-get g wgt 'callback))
         (armed (glgui-widget-get g wgt 'armed))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (cond
       ((and (procedure? cb) inside (= type EVENT_BUTTON1DOWN))
          (glgui-widget-set! g wgt 'armed #t))
       ((and armed (procedure? cb) inside (= type EVENT_BUTTON1UP)) 
          (glgui-widget-set! g wgt 'armed #f)
          (cb g wgt)))
  inside
))

(define (glgui:pixmap-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (i (glgui-widget-get g wgt 'image))
         (sw (glgui-widget-get-dyn g wgt 'sw))
         (sh (glgui-widget-get-dyn g wgt 'sh))
         (w (if sw sw (car i)))
         (h (if sh sh (cadr i)))
         (a (glgui-widget-get-dyn g wgt 'angle))
         (c (glgui-widget-get g wgt 'color)))
  (glCoreColor c)
  (apply glCoreTextureDraw (append (list x y 
    (if (= w 0) (car i) w) (if (= h 0) (cadr i) h)) 
    (cddr i) (list a)))
))

;; img is a texture list (w h t x1 y1 x2 y2)
;; stretch list (sw sh) [2 items] is used to stretch the texture to a desired size (use 0 to reinit on the fly)
;; stretch list (s) [1 item] is used to scale the image by strech factor s
(define (glgui-pixmap g x y img . stretch)
  (let ((sw #f)
        (sh #f))
     (if (= (length stretch ) 2) 
       (begin 
         (set! sw (car stretch))
         (set! sh (cadr stretch))
       )
       (if (= (length stretch ) 1)
         (let ((pw (car img))
               (ph (cadr img)))
           (set! sw (* pw (car stretch)))
      	   (set! sh (* ph (car stretch))))
       )
     )
  (glgui-widget-add g
     'x x
     'y y
     'sw sw
     'sh sh
     'angle 0.
     'image img
     'callback #f
     'color White
     'hidden #f
     'armed #f
     'align GUI_ALIGNLEFT
     'draw-handle  glgui:pixmap-draw
     'input-handle glgui:pixmap-input
  )
 )
)

;;eof
