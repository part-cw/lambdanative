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
;; barchart widget 

(define (glgui:barchart-draw g wgt)
  (let* ((x0 (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (sp (glgui-widget-get-dyn g wgt 'spacer))
         (v0 (glgui-widget-get-dyn g wgt 'vals))
         (v (if v0 v0 '()))
         (vmax0 (glgui-widget-get-dyn g wgt 'vmax))
         (vlen (length v))
         (stacked? (and (fx> vlen 0) (list? (car v))))
         (vmax (if vmax0 vmax0 (if v0 (if stacked? (list-max (map sum v)) (list-max v)) 0)))
         (dw (floor (/ (- w (* (fx+ vlen 1) sp)) vlen)))
         (color0 (glgui-widget-get g wgt 'color))
         (color (if (list? color0) color0 (make-list vlen color0))))
    (if stacked?
      ;; Stacked barchart
      (let loop ((vals v) (x (+ x0 sp)))
        (if (fx= (length vals) 0) 
          #f
          (let loop2 ((vals-st (car vals)) (dy y) (c color))
            (if (fx= (length vals-st) 0) 
              (loop (cdr vals) (+ x sp dw))
              (let ((dh (* h (/ (car vals-st) vmax))))
                (if (> dh 0) (glgui:draw-box x dy dw dh (car c)))
                (loop2 (cdr vals-st) (+ dy dh) (cdr c))
              )
            )
          )
        ) 
      )
      ;; Regular barchart
      (let loop ((vals v) (c color) (x (+ x0 sp)))
        (if (fx= (length vals) 0) 
          #f
          (let ((dh (* h (/ (car vals) vmax))))
            (if (> dh 0) (glgui:draw-box x y dw dh (car c)))
            (loop (cdr vals) (cdr c) (+ x sp dw))
          )
        )
      )
    )
  )
)

(define (glgui:barchart-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (armed (glgui-widget-get g wgt 'armed))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (cond
       ((and (procedure? cb) inside (= type EVENT_BUTTON1DOWN))
          (glgui-widget-set! g wgt 'armed #t))
       ((and armed (procedure? cb) inside (= type EVENT_BUTTON1UP))
          (glgui-widget-set! g wgt 'armed #f)
          (cb g wgt type mx my)))
  inside
))

(define (glgui-barchart g x y w h v c)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'callback #f
     'color c
     'vals v
     'spacer (if (fx> (length v) 5) 2 5)
     'hidden #f
     'armed #f
     'draw-handle  glgui:barchart-draw
     'input-handle glgui:barchart-input
  ))

;;eof
