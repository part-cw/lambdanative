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

(define (glgui:sprite-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (i (glgui-widget-get g wgt 'image))
         (w (glgui-widget-get g wgt 'w))
         (h (glgui-widget-get g wgt 'h))
         (presscb (glgui-widget-get g wgt 'presscallback))
         (releasecb (glgui-widget-get g wgt 'releasecallback))
         (inside (and i (> mx x) (< mx (+ x (if w w (car i)))) (> my y) (< my (+ y (if h h (cadr i)))))))
   (cond
     ((and inside presscb (= type EVENT_BUTTON1DOWN)) (presscb g wgt))
     ((and inside releasecb (= type EVENT_BUTTON1UP)) (releasecb g wgt))
   )
  inside
 ))

(define (glgui:sprite-draw g wgt)
  (let ((x (glgui-widget-get-dyn g wgt 'x))
        (y (glgui-widget-get-dyn g wgt 'y))
        (i (glgui-widget-get g wgt 'image))
        (w (glgui-widget-get g wgt 'w))
        (h (glgui-widget-get g wgt 'h))
        (a (glgui-widget-get g wgt 'angle))
        (c (glgui-widget-get g wgt 'color))
        (rendercallback (glgui-widget-get g wgt 'rendercallback))) 
  (if i (begin
    (glCoreColor c)
    (apply glCoreTextureDraw (append (list x y 
      (if w w (car i)) (if h h (cadr i))) 
      (cddr i) (list a)))
    (if rendercallback (rendercallback g wgt))
  ))
))

(define (glgui-sprite g . args)
  (let ((wgt (glgui-widget-add g
     'image #f
     'x 0 'y 0 'angle 0.
     'presscallback #f
     'releasecallback #f
     'rendercallback #f
     'color White
     'hidden #f
     'draw-handle  glgui:sprite-draw
     'input-handle glgui:sprite-input
    )))
   (let loop ((as args))
     (if (fx= (length as) 0) wgt (begin
       (glgui-widget-set! g wgt (car as) (cadr as))
       (loop (cddr as)))))
 ))

;;eof
