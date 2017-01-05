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
;; container widget
(define (glgui:container-input-drag g wgt type mx my)
  (let* ((xofs (glgui-widget-get-dyn g wgt 'xofs))
         (yofs (glgui-widget-get-dyn g wgt 'yofs))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (drag_oldx (glgui-widget-get g wgt 'drag_oldx))
         (drag_oldy (glgui-widget-get g wgt 'drag_oldy))
         (dragged? (glgui-widget-get g wgt 'dragged?))
         (draggable_x? (glgui-widget-get g wgt 'draggable_x?))
         (draggable_y? (glgui-widget-get g wgt 'draggable_y?))
         (drag_keep (glgui-widget-get g wgt 'drag_keep))
         (inside (and (> mx xofs) (< mx (+ xofs w)) (> my yofs) (< my (+ yofs h)))))
    (cond
      ((and (fx= type EVENT_MOTION) drag_oldx drag_oldy) ;; drag
        (if (or (> (abs (- mx drag_oldx)) 5) (> (abs (- my drag_oldy)) 5)) (begin
          (glgui-widget-set! g wgt 'dragged? #t)
          (if (and draggable_x?
                   (< (+ xofs (- mx drag_oldx)) (- (glgui-width-get) drag_keep))
                   (> (+ xofs (- mx drag_oldx) w) drag_keep))
            (glgui-widget-set! g wgt 'xofs (+ xofs (- mx drag_oldx)))
          )
          (if (and draggable_y?
                   (< (+ yofs (- my drag_oldy)) (- (glgui-height-get) drag_keep))
                   (> (+ yofs (- my drag_oldy) h) drag_keep))
            (glgui-widget-set! g wgt 'yofs (+ yofs (- my drag_oldy)))
          )
          (glgui-widget-set! g wgt 'drag_oldx (fix mx))
          (glgui-widget-set! g wgt 'drag_oldy (fix my))
        ))
      )
      ((and inside (fx= type EVENT_BUTTON1DOWN)) ;; touch down
        (glgui-widget-set! g wgt 'drag_oldx (fix mx))
        (glgui-widget-set! g wgt 'drag_oldy (fix my))
        (glgui-widget-set! g wgt 'dragged? #f)
      )
      ((and drag_oldx drag_oldy (fx= type EVENT_BUTTON1UP)) ;; touch release
       (if (and inside dragged?) (begin
          (glgui-widget-set! g wgt 'dragged? #f)
          (glgui-widget-set! g wgt 'drag_oldx #f))
          (glgui-widget-set! g wgt 'drag_oldy #f)
        )
      )
      (else (if (not inside) (begin
        (glgui-widget-set! g wgt 'drag_oldx #f)
        (glgui-widget-set! g wgt 'drag_oldy #f)
        ))
      )
    )
    ;; Make sure the widgets within get events passed too
    (if (not dragged?)
      (glgui:inputloop type mx my wgt)
      inside
    )
))

(define (glgui-container-update g wgt id val)
  (if (or (eq? id 'draggable_x?) (eq? id 'draggable_y?))
    (let ((draggable_x? (glgui-widget-get g wgt 'draggable_x?))
          (draggable_y? (glgui-widget-get g wgt 'draggable_y?)))
      (if (or val draggable_x? draggable_y?)
        (glgui-widget-set! g wgt 'input-handle glgui:container-input-drag)
        (glgui-widget-set! g wgt 'input-handle #f)
      )
    )
  )
)

(define (glgui-container g x y w h)
  (glgui-widget-add g
    'x 0
    'y 0
    'w w
    'h h
    'xofs x
    'yofs y
    'widget-list '()
    'widget-count 0
    'container #t
    'hidden #f
    'draw-handle #f
    'input-handle #f
    'draggable_x? #f
    'draggable_y? #f
    'drag_keep 10
    'update-handle glgui-container-update
  ))

;; eof
