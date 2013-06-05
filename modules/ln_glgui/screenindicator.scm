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

(include "glgui_screenindicator_selected.scm")
(include "glgui_screenindicator_empty.scm")

(define (glgui:screenindicator-draw g wgt)
  (let ((y (glgui-widget-get-dyn g wgt 'y))
        (w (glgui-widget-get-dyn g wgt 'w))
        (h (glgui-widget-get-dyn g wgt 'h))
  (c (glgui-widget-get-dyn g wgt 'color)) ;; We color screen indicators in this color
  (ifull glgui_screenindicator_selected.img)  ;; Image for selected screen indicator
  (iempty glgui_screenindicator_empty.img)  ;; Image for other screen indicators
        (si (glgui-widget-get-dyn g wgt 'idx))    ;; Index of selected screen
  (sn (glgui-widget-get-dyn g wgt 'screens))) ;; Number of screens
    (if (and (fx>= si 0) (fx> sn si))
      (let* ((dy (/ (- h (cadr ifull)) 2))    ;; element height offset
       (spacer (if (< (* sn (+ (car ifull) 5)) (* w 0.70)) 5 (if (< (* sn (+ (car ifull) 2)) (* w 0.75)) 2 0)));; [5,2 or 0 is spacing between elements]
       (dx (+ (car ifull) spacer))    ;; element width offset 
       (startx (/ (- w (* sn dx)) 2)))    ;; element start position (centered)
  (let loop ((i 0))
    (if (fx= i sn) #f
      (begin 
        (glCoreColor c)
        (apply glCoreTextureDraw (append (list (+ startx (* i dx)) (+ y dy)) (if (fx= i si) ifull iempty) (list 0)))
        (loop (fx+ i 1))
      )
    )
  ))
  )))

(define (glgui:screenindicator-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (if (and (procedure? cb) inside (= type EVENT_BUTTON1UP)) (cb g wgt type mx my))
  inside
))

(define (glgui-screenindicator g x y w h c)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'callback #f
     'color c
     'hidden #f
     'idx 0
     'screens 0
     'draw-handle  glgui:screenindicator-draw
     'input-handle glgui:screenindicator-input
  ))
;;eof