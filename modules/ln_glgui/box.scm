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
;; box widget 

(define (glgui:box-draw g wgt)
  (let ((x (glgui-widget-get-dyn g wgt 'x))
        (y (glgui-widget-get-dyn g wgt 'y))
        (w (glgui-widget-get-dyn g wgt 'w))
        (h (glgui-widget-get-dyn g wgt 'h))
        (r (glgui-widget-get g wgt 'rounded))
        (c (glgui-widget-get g wgt 'color)))
  (if r
    (glgui:draw-rounded-box x y w h c)
    (glgui:draw-box x y w h c))
))

(define (glgui:box-input g wgt type mx my)
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

(define (glgui:box-input-drag g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (oldx (glgui-widget-get g wgt 'oldx))
         (oldy (glgui-widget-get g wgt 'oldy))
         (drag (glgui-widget-get g wgt 'drag))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
    (cond
      ((and (fx= type EVENT_MOTION) oldx oldy) ;; drag
        (if (or (> (abs (- mx oldx)) 5) (> (abs (- my oldy)) 5))
          (glgui-widget-set! g wgt 'drag #t)
        )
      )
      ((and inside (fx= type EVENT_BUTTON1DOWN)) ;; touch down
        (glgui-widget-set! g wgt 'oldx (fix mx))
        (glgui-widget-set! g wgt 'oldy (fix my))
        (glgui-widget-set! g wgt 'drag #f)
      )
      ((and oldx oldy (fx= type EVENT_BUTTON1UP)) ;; touch release
       (if (and inside drag) (begin
          (glgui-widget-set! g wgt 'offsetx (- mx oldx))
          (glgui-widget-set! g wgt 'offsety (- my oldy))
          (glgui-widget-set! g wgt 'drag #f)
          (glgui-widget-set! g wgt 'oldx #f))
          (glgui-widget-set! g wgt 'oldy #f)
        )
        (if (and inside cb)
          (if (procedure? cb) (cb g wgt type mx my))
        )
      )
      (else (if (not inside) (begin
        (glgui-widget-set! g wgt 'oldx #f)
        (glgui-widget-set! g wgt 'oldy #f)
        ))
      )
    )
  inside
))



(define (glgui-box g x y w h c)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'rounded #f
     'callback #f
     'color c
     'hidden #f
     'armed #f
     'draw-handle  glgui:box-draw
     'input-handle glgui:box-input
  ))

(define (glgui-box-dragable g x y w h color cb)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'rounded #f
     'callback cb
     'color color
     'offsetx 0
     'offsety 0
     'hidden #f
     'draw-handle  glgui:box-draw
     'input-handle glgui:box-input-drag
  ))
;;eof
