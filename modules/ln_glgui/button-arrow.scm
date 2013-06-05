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

(define (glgui:button-arrow-draw g wgt)
  (let ((_x (glgui-widget-get-dyn g wgt 'x))
        (_y (glgui-widget-get-dyn g wgt 'y))
        (_w (glgui-widget-get-dyn g wgt 'w))
        (_h (glgui-widget-get-dyn g wgt 'h))
        (left (glgui-widget-get-dyn g wgt 'left))
        (a (glgui-widget-get-dyn g wgt 'armed))
        (i (glgui-widget-get g wgt 'image))
        (c (glgui-widget-get g wgt 'color))
        (bsc (glgui-widget-get g wgt 'button-selected-color))
        (bnc (glgui-widget-get g wgt 'button-normal-color))
        (f (glgui-widget-get g wgt 'font)))
    (glCoreColor (if a bsc bnc))

    ;; Compute coordinates, for texture coordinates, assume glgui_button_arrow.img has texture from 0. 0. to 1. 1.
    (let* ((x (flo _x))
           (y (flo _y))
           (w (flo _w))
           (h (flo _h))
           (w2 (fl/ w 2.))
           (h2 (fl/ h 2.))
           (cx (fl+ x w2))
           (cy (fl+ y h2))
           (x2 (fl+ x w))
           (y2 (fl+ y h)))

      ;; Is arrow pointing left
      (if left
        ;; Send coordinates for 5 points of arrow pointing left
        (glCoreTexturePolygonDraw cx cy (list 
            (list (fl+ x 10.) y2 0. 1.)   ;; Top left
            (list x2 y2 1. 1.)            ;; Top right
            (list x cy 0. 0.5)            ;; Point on the left
            (list x2 y 1. 0.)             ;; Bottom right
            (list (fl+ x 10.) y 0. 0.))   ;; Bottom left
          (caddr glgui_button_arrow.img) 0.)
        ;; OR send coordinates for 5 points of arrow pointing right
        (glCoreTexturePolygonDraw cx cy (list
            (list (fl- x2 10.) y2 1. 1.)   ;; Top right
            (list x y2 0. 1.)              ;; Top left
            (list x2 cy 1. 0.5)            ;; Point on the right
            (list x y 0. 0.)               ;; Bottom left
            (list (fl- x2 10.) y 1. 0.))   ;; Bottom right
          (caddr glgui_button_arrow.img) 0.))
      (if f ;;String based regular buttons
        (glgui:draw-text-center (if left (+ x 10) x) y (- w 10) h (car i) f c)
        (let* ((sw (car i)) (sh (cadr i))
               (sx (fix (+ x (/ (- w sw) 2.) (if left 5 -5))))
               (sy (fix (+ y (/ (- h sh) 2.)))))
          (glCoreColor c)
          (apply glCoreTextureDraw (append (list sx sy sw sh) (cddr i) (list 0.))))))
))

;; process input for the button
(define (glgui:button-arrow-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x)) 
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (armed (glgui-widget-get g wgt 'armed))
         (i (glgui-widget-get g wgt 'image))
         (cb (glgui-widget-get g wgt 'callback))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (cond
       ((and (= type EVENT_BUTTON1DOWN) inside)
          (glgui-widget-set! g wgt 'armed #t))
       ((= type EVENT_BUTTON1UP) 
          (if (and armed inside) (begin 
          (if (list? (car i)) ;; it's a toggle
            (glgui-widget-set! g wgt 'value (fix (floor (/ (* (length i) (- mx x)) w)))))
          (if (procedure? cb) (cb g wgt type mx my))))
          (glgui-widget-set! g wgt 'armed #f))
     )
  inside
))

(define (glgui-button-arrow g x y w h l img callback)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'left l
     'image img
     'callback callback
     'color White
     ;; Placed the selected and unselected background colors here so apps could change them if needed.
     'button-selected-color DarkGrey
     'button-normal-color LightGrey
     'armed #f
     'hidden #f
     'font #f
     'draw-handle  glgui:button-arrow-draw
     'input-handle glgui:button-arrow-input
  ))

(define (glgui-button-arrow-string g x y w h l str fnt callback)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'left l
     'image (if (pair? str)(map list str)(list str))
     'callback callback
     'color White
     ;; Placed the selected and unselected background colors here so apps could change them if needed.
     'button-selected-color DarkGrey
     'button-normal-color LightGrey
     'armed #f
     'hidden #f
     'font fnt
     'draw-handle  glgui:button-arrow-draw
     'input-handle glgui:button-arrow-input
  ))

;;eof