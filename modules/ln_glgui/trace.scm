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
;; generic trace widget

(define (glgui:trace-slider-draw g wgt)
  (glgui:trace-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (sx (glgui-widget-get g wgt 'sx))
         (t (glgui-widget-get g wgt 'trace))
         (fnt (glgui-widget-get g wgt 'popup-font)))
    (if sx (begin
      (glgui:draw-box (fx+ x sx -1) (fx- y 5) 2 (fx+ h 5) White)
      (if fnt
        (let* ((values (table-ref t 'values))
               (val (f32vector-ref values (inexact->exact (floor (* (f32vector-length values) (/ sx w)))))))
          (if (not (nan? val))
            (let* ((vmin (table-ref t 'vmin))
                   (vmax (table-ref t 'vmax))
                   (dy (min h (max 0 (inexact->exact (round (* h (min 1. (max 0. (/ (- val vmin) (- vmax vmin))))))))))
                   (str (float->string val 1))
                   (fh (car (glgui:stringheight str fnt)))
                   (fw (glgui:stringwidth str fnt))
                   (newx (if (fx> sx (fx- w 50)) (fx+ x (fx- sx fw 2)) (fx+ x sx 2))))
              (glgui:draw-box newx (fx+ y dy -10) fw fh Black)
              (glgui:renderstring newx (fx+ y dy -10) str fnt White)
            )
          )
        )
      )
    ))
  )
)

(define (glgui:trace-slider-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x)) 
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (cond
       ((and (= type EVENT_BUTTON1DOWN) inside)
          (if (procedure? cb) (cb g wgt type (fx- mx x) my))
          (glgui-widget-set! g wgt 'sx (fx- mx x)))
       ((and (= type EVENT_MOTION) inside (glgui-widget-get g wgt 'sx))
          (glgui-widget-set! g wgt 'sx (fx- mx x)))
       ((= type EVENT_BUTTON1UP)
          (if (and inside (procedure? cb)) (cb g wgt type (fx- mx x) my)) 
          (glgui-widget-set! g wgt 'sx #f))
     )
  inside
))

(define (glgui:trace-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (a (glgui-widget-get-dyn g wgt 'angle))
         (t (glgui-widget-get g wgt 'trace))
;;	 (i (glgui-widget-get g wgt 'image))
	 (i (table-ref t 'image))
         (s (glgui-widget-get g wgt 'sperm))
         (limcol (glgui-widget-get g wgt 'limcol))
         (limfnt (glgui-widget-get g wgt 'limfnt))
         (limscale (glgui-widget-get g wgt 'limscale))
         (c (glgui-widget-get g wgt 'color)))
  ;; draw the limits
  (if limfnt (let* (
    (vmin (table-ref t 'vmin))
    (vmax (table-ref t 'vmax))
    (vlo (table-ref t 'vlo))
    (vhi (table-ref t 'vhi))
    (ylo (+ y (* h (/ (- vlo vmin) (- vmax vmin)))))
    (yhi (+ y (* h (/ (- vhi vmin) (- vmax vmin))))))
    (glgui:draw-box x ylo w (- yhi ylo) (color-fade White 0.05))
   ;; (glgui:draw-text-center x (- ylo 15) w 15 (number->string (fix vlo)) limfnt limcol)
   ;; (glgui:draw-text-center x (+ yhi 0) w 16 (number->string (fix vhi)) limfnt limcol)
    (glgui:draw-text-center x (- ylo 15) w 15 (float->string (flo (if (procedure? limscale) (limscale vlo) vlo)) 3) limfnt limcol)
    (glgui:draw-text-center x (+ yhi 0) w 16 (float->string (flo (if (procedure? limscale) (limscale vhi) vhi)) 3) limfnt limcol)
  ))
  (glCoreColor c)
  (apply glCoreTextureDraw (append (list x y
    (if (= w 0) (car i) w) (if (= h 0) (cadr i) h))
    (cddr i) (list a)))
  ;; draw the sperm
  (if s (let ((sx (table-ref t 'x))  
              (sy (table-ref t 'y))
              (tw (table-ref t 'w))
              (th (table-ref t 'h)))
    (if sy (glCoreTextureDraw 
      (+ x (* 1. w (/ sx tw)) -8) (+ y (* 1. h (/ sy th)) -8) 16 16 
         glgui_sperm.raw 0. 0. 1. 1. 0.))
  ))
))

;; stretch is used to stretch the texture to a desired size (use 0 to reinit on the fly)
(define (glgui-trace g x y w h trace color . lim)
;;  (let* ((img (table-ref trace 'image)))
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'angle 0.
     'trace trace
 ;;    'image img
     'callback #f
     'color color
     'sperm #f
     'hidden #f
     'draw-handle  glgui:trace-draw
     'input-handle #f
     'limfnt (if (>= (length lim) 1) (car lim) #f)
     'limcol (if (= (length lim) 2) (cadr lim) White)
     'limscale #f ;; use to scale the numbers 
  )
;; )
)

(define (glgui-trace-slider g x y w h trace color popup-fnt . lim)
  (let ((wgt (if (> (length lim) 0)
          (if (= (length lim) 1) 
            (glgui-trace g x y w h trace color (car lim))
            (glgui-trace g x y w h trace color (car lim) (cadr lim)))
          (glgui-trace g x y w h trace color)
        )))
  (glgui-widget-set! g wgt 'draw-handle glgui:trace-slider-draw)
  (glgui-widget-set! g wgt 'input-handle glgui:trace-slider-input)
  (glgui-widget-set! g wgt 'sx #f)
  (glgui-widget-set! g wgt 'popup-font popup-fnt)
  wgt)
)

;;eof
