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
(define glgui:horizontalnumberwheel_waittime 0.6)
(define glgui:horizontalnumberwheel_incrementtime 0.15)

;; Arrow texture
(define glgui:arrow (glCoreTextureCreate 4 4 (make-u8vector 16 #xff)))

(define (glgui:horizontalvaluepicker-draw g wgt)
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
         ;; Right left flips everything so that value increase leftward (unit becomes negative)
         (rightleft (glgui-widget-get g wgt 'rightleft))
         (armright (glgui-widget-get g wgt 'armright))
         (armleft (glgui-widget-get g wgt 'armleft))
         (activitytime (glgui-widget-get g wgt 'activitytime))
         (initialwait (glgui-widget-get g wgt 'initialwait))
         (unit (if rightleft (- (glgui-widget-get g wgt 'unit)) (glgui-widget-get g wgt 'unit)))
         (valprv (min valmax (max valmin (- value unit))))
         (valnxt (max valmin (min valmax (+ value unit))))
         ;; Below strings may actually be textures if textures in vallist
         (valstr (if vallist (list-ref vallist (fix value)) (val->str value))))

     ;; Draw the background
     (if colorbg
       (glgui:draw-box x y w h colorbg))

     (let* ((ah (flo (/ h 3)))
            (ah2 (fl/ ah 2.))
            (aw 20.)
            (aw2 (fl/ aw 2.))
            (ay1 (fl+ (flo y) ah))
            (cy (fl+ ay1 ah2))
            (ay2 (fl+ ay1 ah))
            (a1x1 (fl+ (flo x) 15.))
            (c1x (fl+ a1x1 aw2))
            (a1x2 (fl+ a1x1 aw))
            (a2x1 (- (flo (+ x w)) 15. aw))
            (c2x (fl+ a2x1 aw2))
            (a2x2 (fl+ a2x1 aw)))
     
       ;; Draw left arrow if any values less or cycle turned on
       (if (or (not (= value valprv)) cycle) 
         (begin
            (if armleft
              (glgui:draw-box x y (+ aw 25.) h colorhighlight))
            (glCoreColor colorarrows)
            (glCoreTexturePolygonDraw cy c1x (list 
              (list a1x2 ay1 0. 1.)
              (list a1x2 ay2 1. 1.)
              (list a1x1 cy 0.5 0.))
              glgui:arrow 0.)))

       ;; Draw right arrow if any values more or cycle turned on
       (if (or cycle (not (= value valnxt))) 
          (begin
            (if armright
              (glgui:draw-box (- a2x1 10.) y (+ aw 25.) h colorhighlight))
            (glCoreColor colorarrows)
            (glCoreTexturePolygonDraw cy c2x (list
               (list a2x1 ay1 0. 0.)
               (list a2x2 cy 0.5 1.)
               (list a2x1 ay2 1. 0.))
            glgui:arrow 0.))))
        
    ;; Draw the selected value
    (if (string? valstr) 
      (glgui:draw-text-center x y w h valstr fnt colorvalue)
      (glgui:draw-pixmap-center x y w h valstr colorvalue))
    
    ;; Handle press and hold
    (if activitytime
      (cond 
        ;; After initial wait time, change value by one, no longer in initial wait
        ((and initialwait (fl> (fl- ##now activitytime) glgui:horizontalnumberwheel_waittime))
           (glgui-widget-set! g wgt 'initialwait #f)
           (glgui-widget-set! g wgt 'activitytime ##now)
           (if armright
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
        ;; If still holding left after the increment time 
        ((and (not initialwait) (fl> (fl- ##now activitytime) glgui:horizontalnumberwheel_incrementtime))
           (glgui-widget-set! g wgt 'activitytime ##now)
           (if armright
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

(define (glgui:horizontalvaluepicker-input g wgt type mx my)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (h (fix (glgui-widget-get-dyn g wgt 'h)))    
         (cb (glgui-widget-get g wgt 'callback))
         (cycle (glgui-widget-get g wgt 'cycle))
         (value (flo (glgui-widget-get g wgt 'value)))
         (valmin (flo (glgui-widget-get g wgt 'valmin)))
         (valmax (flo (glgui-widget-get g wgt 'valmax)))
         ;; Right left flips everything so that value increase downward (unit becomes negative)
         (rightleft (glgui-widget-get g wgt 'rightleft))
         (unit (if rightleft (* -1 (glgui-widget-get g wgt 'unit)) (glgui-widget-get g wgt 'unit)))
         (armright (glgui-widget-get g wgt 'armright))
         (armleft (glgui-widget-get g wgt 'armleft))
         (activitytime (glgui-widget-get g wgt 'activitytime))
         (initialwait (glgui-widget-get g wgt 'initialwait))
	 (inside (and (fx> (fix my) y) (fx< (fix my) (fx+ y h)) (fx> (fix mx) x) (fx< (fix mx) (fx+ x w))))
         (loc_right (and inside (> mx (- (+ x w) 60))))
         (loc_left (and inside (< mx (+ x 60)))))
     ;; If touch down
     (cond
       ((and inside (fx= type EVENT_BUTTON1DOWN))
         ;; arm right or left buttons
         (glgui-widget-set! g wgt 'armright loc_right)
         (glgui-widget-set! g wgt 'armleft loc_left)
         ;; Remember time that button was pressed for initial wait before changing values
         (glgui-widget-set! g wgt 'activitytime ##now)
         (glgui-widget-set! g wgt 'initialwait #t))
       ;; Allow dragging while holding down, as long as they are still inside, otherwise release button
       ((or (not inside) (not (fx= type EVENT_MOTION)))
         (cond
            ;; touched on right button, now releasing
            ((and inside armright (fx= type EVENT_BUTTON1UP) initialwait)
              (let* ((valup (min valmax (max valmin (+ value unit))))
                      ;; If at extreme value already and cycle is on move to other extreme value
                      (newval (if (and cycle (= valup value)) (if (= value valmin) valmax valmin) valup)))
                  (glgui-widget-set! g wgt 'value newval)
                  (if cb (cb g wgt 0 0 0))))
       
            ;; touched on left button, now releasing
            ((and inside armleft (fx= type EVENT_BUTTON1UP) initialwait)
              (let* ((valdown (max valmin (min valmax (- value unit))))
                     ;; If at extreme value already and cycle is on move to other extreme value
                     (newval (if (and cycle (= valdown value)) (if (= value valmin) valmax valmin) valdown)))
                  (glgui-widget-set! g wgt 'value newval)
                  (if cb (cb g wgt 0 0 0))))
       
            ;; If a change was made to the value by holding down the button, call the callback
            ((and (or armleft armright) activitytime (not initialwait))
               (if cb (cb g wgt 0 0 0))))
    
         (glgui-widget-set! g wgt 'armright #f)
         (glgui-widget-set! g wgt 'armleft #f)
         (glgui-widget-set! g wgt 'activitytime #f)
         (glgui-widget-set! g wgt 'initialwait #f)))
  inside 
))

(define (glgui-horizontalvaluepicker g x y w h vmin vmax colorarrows colorhighlight colorvalue colorbg font . steps)
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
       'draw-handle  glgui:horizontalvaluepicker-draw
       'input-handle glgui:horizontalvaluepicker-input
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
       'armright #f
       'armleft #f
       'activitytime #f
       'initialwait #f
       ;; rightleft can be set to true to make the max value be displayed at the left instead of the right
       'rightleft #f
     )))

;; eof
