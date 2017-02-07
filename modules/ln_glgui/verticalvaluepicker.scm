#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
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

;; When holding down on the button, wait time until values start changing
;; and then time between each value change 
(define glgui:verticalnumberwheel_waittime 0.6)
(define glgui:verticalnumberwheel_incrementtime 0.15)

;; Arrow texture
(define glgui:arrow (glCoreTextureCreate 4 4 (make-u8vector 16 #xff)))

(define (glgui:verticalvaluepicker-draw g wgt)
  (define (val->str v) (float->string v 1))
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (colorbg (glgui-widget-get-dyn g wgt 'colorbg))
         (colorvalue (glgui-widget-get-dyn g wgt 'colorvalue))
         (colorhighlight (glgui-widget-get-dyn g wgt 'colorhighlight))
         (colorarrows (glgui-widget-get-dyn g wgt 'colorarrows))
         (fnt (glgui-widget-get-dyn g wgt 'fnt))
         (cycle (glgui-widget-get-dyn g wgt 'cycle))
         (valmin (flo (glgui-widget-get g wgt 'valmin)))
         (valmax (flo (glgui-widget-get g wgt 'valmax)))
         (vallist (glgui-widget-get g wgt 'vallist))
         (value (flo (glgui-widget-get g wgt 'value)))
         ;; Top down flips everything so that value increase downward (unit becomes negative)
         (topdown (glgui-widget-get g wgt 'topdown))
         (scalearrows (glgui-widget-get g wgt 'scalearrows))
         (armtop (glgui-widget-get g wgt 'armtop))
         (armbottom (glgui-widget-get g wgt 'armbottom))
         (activitytime (glgui-widget-get g wgt 'activitytime))
         (initialwait (glgui-widget-get g wgt 'initialwait))
         (unit (if topdown (- (glgui-widget-get g wgt 'unit)) (glgui-widget-get g wgt 'unit)))
         (valprv (min valmax (max valmin (- value unit))))
         (valnxt (max valmin (min valmax (+ value unit))))
         ;; Below strings may actually be textures if textures in vallist
         (valstr (if vallist (list-ref vallist (fix value)) (val->str value)))
         (cbmid (glgui-widget-get g wgt 'callbackmid)))

     ;; Draw the background
     (if colorbg
       (glgui:draw-box x y w h colorbg))

     (let* ((aw (flo (/ w (if scalearrows 2 3))))
            (aw2 (fl/ aw 2.))
            (ah (if scalearrows (flo (/ h 5)) 20.))
            (ah2 (fl/ ah 2.))
            (ax1 (fl+ (flo x) (if scalearrows aw2 aw)))
            (cx (fl+ ax1 aw2))
            (ax2 (fl+ ax1 aw))
            (a1y1 (fl+ (flo y) (if scalearrows (/ ah 3) 15.)))
            (c1y (fl+ a1y1 ah2))
            (a1y2 (fl+ a1y1 ah))
            (a2y1 (- (flo (+ y h)) (if scalearrows (/ ah 3) 15.) ah))
            (c2y (fl+ a2y1 ah2))
            (a2y2 (fl+ a2y1 ah)))
     
       ;; Draw down arrow if any values less or cycle turned on
       (if (or (not (= value valprv)) cycle) 
         (begin
            (if armbottom
                (glgui:draw-box x y w (if scalearrows (/ h 3) (+ ah 25.)) colorhighlight))
            (glCoreColor colorarrows)
            (glCoreTexturePolygonDraw cx c1y (list 
              (list ax1 a1y2 0. 1.)
              (list ax2 a1y2 1. 1.)
              (list cx a1y1 0.5 0.))
              glgui:arrow 0.)))

       ;; Draw up arrow if any values more or cycle turned on
       (if (or cycle (not (= value valnxt))) 
          (begin
            (if armtop
              (if scalearrows
                  (glgui:draw-box x (+ y (* (/ h 3) 2)) w (/ h 3) colorhighlight)
                  (glgui:draw-box x (- a2y1 10.) w (+ ah 25.) colorhighlight)))
            (glCoreColor colorarrows)
            (glCoreTexturePolygonDraw cx c2y (list
               (list ax1 a2y1 0. 0.)
               (list cx a2y2 0.5 1.)
               (list ax2 a2y1 1. 0.))
            glgui:arrow 0.))))
        
    ;; Draw the selected value
    (if (string? valstr)
      (glgui:draw-text-center x y w h valstr fnt colorvalue)
      (glgui:draw-pixmap-center x y w h valstr colorvalue))
    
    ;; Handle press and hold
    (if activitytime
      (cond 
        ;; After initial wait time, change value by one, no longer in initial wait
        ((and initialwait (fl> (fl- ##now activitytime) glgui:verticalnumberwheel_waittime))
           (glgui-widget-set! g wgt 'initialwait #f)
           (glgui-widget-set! g wgt 'activitytime ##now)
           (if armtop
             ;; Set value one larger
             (let* ((valup (min valmax (max valmin (+ value unit))))
                    ;; If at extreme value already and cycle is on move to other extreme value
                    (newval (if (and cycle (= valup value)) (if (= value valmin) valmax valmin) valup)))
               (glgui-widget-set! g wgt 'value newval))
             ;; Set value one smaller
             (let* ((valdown (max valmin (min valmax (- value unit))))
                    ;; If at extreme value already and cycle is on move to other extreme value
                    (newval (if (and cycle (= valdown value)) (if (= value valmin) valmax valmin) valdown)))
               (glgui-widget-set! g wgt 'value newval))))
        ;; If still holding down after the increment time 
        ((and (not initialwait) (fl> (fl- ##now activitytime) glgui:verticalnumberwheel_incrementtime))
           (glgui-widget-set! g wgt 'activitytime ##now)
           (if armtop
             ;; Set value one larger
             (let* ((valup (min valmax (max valmin (+ value unit))))
                    ;; If at extreme value already and cycle is on move to other extreme value
                    (newval (if (and cycle (= valup value)) (if (= value valmin) valmax valmin) valup)))
               (glgui-widget-set! g wgt 'value newval))
             ;; Set value one smaller
             (let* ((valdown (max valmin (min valmax (- value unit))))
                    ;; If at extreme value already and cycle is on move to other extreme value
                    (newval (if (and cycle (= valdown value)) (if (= value valmin) valmax valmin) valdown)))
               (glgui-widget-set! g wgt 'value newval))))))
   
))

(define (glgui:verticalvaluepicker-input g wgt type mx my)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (h (fix (glgui-widget-get-dyn g wgt 'h)))
         (cb (glgui-widget-get g wgt 'callback))
         (cbmid (glgui-widget-get g wgt 'callbackmid))
         (cycle (glgui-widget-get g wgt 'cycle))
         (value (flo (glgui-widget-get g wgt 'value)))
         (valmin (flo (glgui-widget-get g wgt 'valmin)))
         (valmax (flo (glgui-widget-get g wgt 'valmax)))
         ;; Top down flips everything so that value increase downward (unit becomes negative)
         (topdown (glgui-widget-get g wgt 'topdown))
         (scalearrows (glgui-widget-get g wgt 'scalearrows))
         (unit (if topdown (* -1 (glgui-widget-get g wgt 'unit)) (glgui-widget-get g wgt 'unit)))
         (armtop (glgui-widget-get g wgt 'armtop))
         (armbottom (glgui-widget-get g wgt 'armbottom))
         (activitytime (glgui-widget-get g wgt 'activitytime))
         (initialwait (glgui-widget-get g wgt 'initialwait))
         (inside (and (fx> (fix mx) x) (fx< (fix mx) (fx+ x w)) (fx> (fix my) y) (fx< (fix my) (fx+ y h))))
         (loc_top (and inside (> my (- (+ y h) (if scalearrows (/ h 3) 60)))))
         (loc_bottom (and inside (< my (+ y (if scalearrows (/ h 3) 60))))))
     ;; If touch down
     (cond
       ((and inside (fx= type EVENT_BUTTON1DOWN))
         ;; arm up or down buttons
         (glgui-widget-set! g wgt 'armtop loc_top)
         (glgui-widget-set! g wgt 'armbottom loc_bottom)
         ;; Remember time that button was pressed for initial wait before changing values
         (glgui-widget-set! g wgt 'activitytime ##now)
         (glgui-widget-set! g wgt 'initialwait #t))
       ;; Allow dragging while holding down, as long as they are still inside, otherwise release button
       ((or (not inside) (not (fx= type EVENT_MOTION)))
         (cond
            ;; touched on top button, now releasing
            ((and inside armtop (fx= type EVENT_BUTTON1UP) initialwait)
              (let* ((valup (min valmax (max valmin (+ value unit))))
                      ;; If at extreme value already and cycle is on move to other extreme value
                      (newval (if (and cycle (= valup value)) (if (= value valmin) valmax valmin) valup)))
                  (glgui-widget-set! g wgt 'value newval)
                  (if cb (cb g wgt 0 0 0))))
       
            ;; touched on bottom button, now releasing
            ((and inside armbottom (fx= type EVENT_BUTTON1UP) initialwait)
              (let* ((valdown (max valmin (min valmax (- value unit))))
                     ;; If at extreme value already and cycle is on move to other extreme value
                     (newval (if (and cycle (= valdown value)) (if (= value valmin) valmax valmin) valdown)))
                  (glgui-widget-set! g wgt 'value newval)
                  (if cb (cb g wgt 0 0 0))))
       
            ;; touched value (between top and bottom buttons), now releasing
            ((and inside (fx= type EVENT_BUTTON1UP) initialwait)
              (if cbmid (cbmid g wgt 0 0 0)))

            ;; If a change was made to the value by holding down the button, call the callback
            ((and (or armbottom armtop) activitytime (not initialwait))
               (if cb (cb g wgt 0 0 0))))
    
         (glgui-widget-set! g wgt 'armtop #f)
         (glgui-widget-set! g wgt 'armbottom #f)
         (glgui-widget-set! g wgt 'activitytime #f)
         (glgui-widget-set! g wgt 'initialwait #f)))
  inside 
))

(define (glgui-verticalvaluepicker g x y w h vmin vmax colorarrows colorhighlight colorvalue colorbg font . steps)
  (let ((unit 
          (if (= (length steps) 1) 
            ;; If units given, use 1 for step size if a list, otherwise use given step size
            (if (list? (car steps)) 1 (car steps))
            ;; If no units, given compute the step size
            (let ((a (- vmax vmin)))
              (cond ((> a 10.) 1.)
                ((> a 1.) 0.5)
                (else 0.1)))))
        ;; If units given and they are a list, use list as values
        (vallist (if (and (= (length steps) 1) (list? (car steps))) (car steps) #f)))
    (glgui-widget-add g
       'x x
       'y y
       'w w
       'h h
       'callback #f
       'callbackmid #f
       'draw-handle  glgui:verticalvaluepicker-draw
       'input-handle glgui:verticalvaluepicker-input
       'hidden #f
       ;; If a list of values, min and max specify range of indices
       'value (if vallist 0 vmin)
       'valmin (if vallist 0 vmin)
       'valmax (if vallist (- (length vallist) 1) vmax)
       'vallist vallist
       'cycle #f       ;; If set to true, then the wheel will allow cycling - going past highest value to start back at lowest
       'colorarrows colorarrows
       'colorhighlight colorhighlight
       'colorvalue colorvalue
       'colorbg colorbg
       'fnt font
       'unit unit
       'armtop #f
       'armbottom #f
       'activitytime #f
       'initialwait #f
       ;; Topdown can be set to true to make the max value be displayed at the bottom instead of the top
       'topdown #f
       'scalearrows #f
     )))

;; eof
