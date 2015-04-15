#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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
;; recurrence plot widget

(define (glgui:recurrence-make-texture data gradient)
  (let* ((dataxl (if (list? (car data)) (car data) data))
         (datayl (if (list? (car data)) (cadr data) data))
         (datax (list->vector dataxl))
         (datay (list->vector datayl))
         (maxx (list-max (map abs dataxl)))
         (maxy (list-max (map abs datayl)))
         (dmax (fx+ (exact-ceiling maxx) (exact-ceiling maxy)))
         (w (vector-length datax))
         (h (vector-length datay))
         (w0 (fix (expt 2. (ceiling (/ (log w) (log 2.))))))
         (h0 (fix (expt 2. (ceiling (/ (log h) (log 2.))))))
         (data (make-u8vector (* w0 h0 3))))
    (let loopy ((y 0))
      (if (fx= y h)
        (list w h (glCoreTextureCreate w0 h0 data)
              0. 0. (/ w w0) (/ h h0))
        (begin
          (let loopx ((x 0))
            (if (fx= x w)
              #f
              (let* ((val (/ (abs (- (vector-ref datax x) (vector-ref datay y))) dmax))
                     (cl (color-gradient gradient val)))
                (u8vector-set! data (fx+ (* x 3) (fx* y w0 3)) (color-red cl))
                (u8vector-set! data (fx+ (* x 3) (fx* y w0 3) 1) (color-green cl))
                (u8vector-set! data (fx+ (* x 3) (fx* y w0 3) 2) (color-blue cl))
                (loopx (fx+ x 1))
              )
            )
          )
          (loopy (fx+ y 1))
        )
      )
    )
  )
)

(define (glgui:recurrence-update g wgt id val)
  (if (or (eq? id 'data) (eq? id 'gradient))
    (gtable-set! wgt 'img (glgui:recurrence-make-texture val (glgui-widget-get g wgt 'gradient)))
  )
)

(define (glgui:recurrence-draw g wgt)
  (let ((x (glgui-widget-get-dyn g wgt 'x))
        (y (glgui-widget-get-dyn g wgt 'y))
        (w (glgui-widget-get-dyn g wgt 'w))
        (h (glgui-widget-get-dyn g wgt 'h))
        (c (glgui-widget-get-dyn g wgt 'color))
        (bg (glgui-widget-get-dyn g wgt 'bgcolor))
        (img (glgui-widget-get-dyn g wgt 'img)))
    (if bg (glgui:draw-box x y w h bg))
    (glCoreColor c)
    (apply glCoreTextureDraw (append (list x y w h) (cddr img) (list 0.)))
  ))

(define (glgui-recurrence g x y w h data color . bgcolor)
  (glgui-widget-add g
    'x x
    'y y
    'w w
    'h h
    'data data
    'color color
    'bgcolor (if (fx= (length bgcolor) 1) (car bgcolor) #f)
    'img (glgui:recurrence-make-texture data (if (= color White) GRADIENT_THERMAL GRADIENT_GRAY))
    'gradient (if (= color White) GRADIENT_THERMAL GRADIENT_GRAY)
    'callback #f
    'hidden #f
    'draw-handle glgui:recurrence-draw
    'input-handle glgui:image-input
    'update-handle glgui:recurrence-update
))
;; eof
