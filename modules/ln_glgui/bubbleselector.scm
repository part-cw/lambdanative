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
;; widget for choosing between images on a scale where the images
;; bubble up larger as the mouse/finger is dragged over them

(define (glgui:bubbleselector-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (sc (glgui-widget-get g wgt 'scale))
         (imgs (glgui-widget-get g wgt 'imglist))
         (imglen (length imgs))
         (val (glgui-widget-get g wgt 'value))
         (bp (glgui-widget-get g wgt 'bubblepercent))
         ;; Get the scale of each of the images
         (scales (let wloop ((i 0) (sclist '()))
                    (if (fx< i imglen)
                      (let ((newsc (cond
                                    ((not val) sc) 
                                    ((fx= i val) (+ 1. (* (- sc 1.) bp)))
                                    ((fx= i (+ val 1)) (+ 1. (* (- sc 1.) (- 1 bp))))
                                    (else sc))))
                         (wloop (+ i 1) (append sclist (list newsc))))
                  sclist)))
         ;; Get total width of images in their current state
         (totalw (apply + (map (lambda (img imgsc) (* (car img) imgsc)) imgs scales)))
         (totalspacew (- w totalw))
         (spacew (/ totalspacew imglen)) 
         (c (glgui-widget-get g wgt 'color)))
    (glCoreColor c)
    (let loop ((i 0) (ix (+ x (/ spacew 2))))
      (if (fx< i imglen)
        (let* ((img (list-ref imgs i))
               (csc (list-ref scales i))
               (scw (* (car img) csc))
               (sch (* (cadr img) csc))
               (iy (+ y (/ (- h sch) 2))))
          (apply glCoreTextureDraw (append (list ix iy scw sch) (cddr img) (list 0.)))
          (loop (+ i 1) (+ ix scw spacew))))))
)

(define (glgui:bubbleselector-input g wgt type mx my)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (h (fix (glgui-widget-get-dyn g wgt 'h)))
         (sc (glgui-widget-get g wgt 'scale))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h))))
         (armed (glgui-widget-get g wgt 'armed)))
    (if (= type EVENT_BUTTON1UP) (glgui-widget-set! g wgt 'armed #f))
    (if (= type EVENT_BUTTON1DOWN) (glgui-widget-set! g wgt 'armed #t))
    (cond
      ;; Releasing, choose just one item
      ((and inside (= type EVENT_BUTTON1UP))
        (let* ((imgs (glgui-widget-get g wgt 'imglist))
               (imglen (length imgs))
               (totalw (apply + (map car imgs)))
               (fsc (/ w totalw)))
          ;; Determine which item(s) are selected
          (let loop ((i 0) (ix x))
            (let ((newx (+ ix (* (car (list-ref imgs i)) fsc))))
              (if (and (< i (- imglen 1)) (> mx newx))
                (loop (+ i 1) newx)
                (let ((cb (glgui-widget-get g wgt 'callback)))
                  ;; Set current value
                  (glgui-widget-set! g wgt 'value i)
                  ;; Just one image at the largest size
                  (glgui-widget-set! g wgt 'bubblepercent 0.)
                  (if cb (cb g wgt type mx my)))))))
        #t)
      
      ;; Potentially bubbling up two different items partially
      ((and inside (or (= type EVENT_BUTTON1DOWN) (and armed (= type EVENT_MOTION))))
        (let* ((imgs (glgui-widget-get g wgt 'imglist))
               (imglen (length imgs))
               (totalw (apply + (map car imgs)))
               (fsc (/ w totalw))
               (firstimgw (* (caar imgs) fsc)))
          ;; Determine which item(s) are selected
          (let loop ((i 0) (ix (+ x (* firstimgw 0.7))) (bubblestw (* firstimgw 0.3)))
            (if (or (fx= i (- imglen 1)) (< mx ix))
              ;; If before the bubble or on the last item, just one item selected
              (begin
                 ;; Set current value
                 (glgui-widget-set! g wgt 'value i)
                 ;; Just one image at the largest size
                 (glgui-widget-set! g wgt 'bubblepercent 0.))
              ;; Otherwise check if in the area between items
              (let* ((nextimgw (if (fx< i (- imglen 1)) (* (car (list-ref imgs (+ i 1))) fsc) 0))
                     (bubblesidew (* nextimgw 0.3))
                     (bubbleendx (+ ix bubblestw bubblesidew))
                     (nextx (+ bubbleendx (* nextimgw 0.4))))
                (if (> mx bubbleendx)
                  ;; If to the right of this item, go to the next one
                  (loop (+ i 1) nextx bubblesidew)
                  ;; Otherwise compute percent across space
                  (begin
                    (glgui-widget-set! g wgt 'value i)
                    (glgui-widget-set! g wgt 'bubblepercent (/ (- mx ix) (- bubbleendx ix)))))))))
       #t)
      (else #f)))
)

(define (glgui-bubbleselector g x y w h images scale)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'callback #f
     'value #f
     'bubblepercent 0.
     'draw-handle  glgui:bubbleselector-draw
     'input-handle glgui:bubbleselector-input
     'callback #f
     'color White
     'hidden #f
     'armed #f
     'imglist images
     'scale scale)
)

;; eof
