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

;; vertical (number) wheel widget
;; Height of the wheel - any larger h specified adds space for icons
(define glgui:verticalnumberwheel_height 140)
(define glgui:verticalnumberwheel_ttop (- 1. (/ 140. 255.)))

(define (glgui:verticalnumberwheel-draw g wgt)
  (define (val->str v) (float->autostring v 1))
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (colorshade (glgui-widget-get-dyn g wgt 'colorshade))
         (colorvalue (glgui-widget-get-dyn g wgt 'colorvalue))
         (coloricon (glgui-widget-get-dyn g wgt 'coloricon))
         (sprite (glgui-widget-get-dyn g wgt 'sprite))
         (sprite2 (glgui-widget-get-dyn g wgt 'sprite2))
         (bigfnt (glgui-widget-get-dyn g wgt 'bigfnt))
         (smlfnt (glgui-widget-get-dyn g wgt 'smlfnt))
         (cycle (glgui-widget-get-dyn g wgt 'cycle))
         (valmin (flo (glgui-widget-get g wgt 'valmin)))
         (valmax (flo (glgui-widget-get g wgt 'valmax)))
         (vallist (glgui-widget-get g wgt 'vallist))
         (down (glgui-widget-get g wgt 'down))
         (val0 (flo (glgui-widget-get g wgt 'value)))
         ;; Top down flips everything so that value increase downward (unit becomes negative)
         (topdown (glgui-widget-get g wgt 'topdown))
         (unit (if topdown (- (glgui-widget-get g wgt 'unit)) (glgui-widget-get g wgt 'unit)))
         (val (min valmax (max valmin (* (round (/ val0 unit)) unit))))
         (valoffset (- val val0))
         (valprv (min valmax (max valmin (- val unit))))
         (valnxt (max valmin (min valmax (+ val unit))))
         ;; Below strings may actually be textures if textures in vallist
         (valstr (if vallist (list-ref vallist (fix val)) (val->str val)))
         (valprvstr (if vallist (list-ref vallist (fix valprv)) (val->str valprv)))
         (valnxtstr (if vallist (list-ref vallist (fix valnxt)) (val->str valnxt)))
         (wheelh glgui:verticalnumberwheel_height)
         (wheelhthird (/ wheelh 3)))
   (if colorshade (glgui:draw-box x y w h colorshade))
   (if sprite
     (let ((sw (car sprite)))
       (glCoreColor coloricon)
       (apply glCoreTextureDraw (append (list (+ x (/ (- w sw) 2)) (+ y 10)) sprite (list 0.)))
   ))
   (if sprite2
     (let ((sw (car sprite2))(sh (cadr sprite2)))
       (glCoreColor White)
       (apply glCoreTextureDraw (append (list (+ x (/ (- w sw) 2)) (+ y h -10 (- sh))) sprite2 (list 0.)))
   ))
   (let* ((wheelw (- w 4))
          (wheelx (+ x 2))
          (wheely (+ y (/ (- h wheelh) 2)))
          (offsety (* (/ valoffset unit) wheelhthird))
          (wheeltleft (glgui:verticalnumberwheel-calc-texture-width wheelw)))
     (glCoreColor (if down White LightGray)) ;; tactile display?
     (apply glCoreTextureDraw (list wheelx wheely wheelw wheelh glgui_verticalnumberwheel.raw 0. 1. wheeltleft glgui:verticalnumberwheel_ttop 0))
     (glCoreClipPush (flo wheelx) (flo wheely) (fl+ (flo wheelx) (flo wheelw)) (fl+ (flo wheely) (flo wheelh)))
     (if (string? valstr) 
        (glgui:draw-text-center wheelx (+ wheely offsety) wheelw wheelh valstr bigfnt colorvalue)
        (glgui:draw-pixmap-center wheelx (+ wheely offsety) wheelw wheelh valstr colorvalue))
     ;; If wheel spun all the way down but cycle is turned on, put top value below it
     (if (and cycle (= val valprv))
       (let ((extreme (if topdown valmin valmax)))
         (set! valprv extreme)
         (set! valprvstr (if vallist (list-ref vallist (fix extreme)) (val->str extreme)))))
     
     ;; Draw value below the current one if any
     (if (not (= val valprv)) 
        (begin
          (if (string? valprvstr) 
            (glgui:draw-text-center wheelx (+ wheely offsety) wheelw wheelhthird valprvstr smlfnt DimGrey)
            (glgui:draw-pixmap-center wheelx (+ wheely offsety) wheelw wheelhthird valprvstr DimGrey))
          ;; If showing part of value below this, then draw it (mostly cropped)
          (if (> offsety 0)
             (let ((valprvprv (min valmax (max valmin (- valprv unit)))))
               ;; Check for cycling
               (if (and cycle (= valprv valprvprv))
                 (set! valprvprv (if topdown valmin valmax)))
               ;; If a previous previous value to show, draw it
               (if (not (= valprv valprvprv))
                 (let ((valprvprvstr (if vallist (list-ref vallist (fix valprvprv)) (val->str valprvprv))))
                   (if (string? valprvprvstr) 
                     (glgui:draw-text-center wheelx (+ (- wheely wheelhthird) offsety) wheelw (/ wheelh 3) valprvprvstr smlfnt DimGrey)
                     (glgui:draw-pixmap-center wheelx (+ (- wheely wheelhthird) offsety) wheelw (/ wheelh 3) valprvprvstr DimGrey))))))))
     
     ;; If wheel spun all the way up but cycle is turned on, put bottom value above it
     (if (and cycle (= val valnxt))
       (let ((extreme (if topdown valmax valmin)))
         (set! valnxt extreme)
         (set! valnxtstr (if vallist (list-ref vallist (fix extreme)) (val->str extreme)))))
     ;; Draw value above the current one if any
     (if (not (= val valnxt)) 
        (begin
          (if (string? valnxtstr) 
             (glgui:draw-text-center wheelx (+ wheely (* 2 wheelhthird) offsety) wheelw wheelhthird valnxtstr smlfnt DimGrey)
             (glgui:draw-pixmap-center wheelx (+ wheely (* 2 wheelhthird) offsety) wheelw wheelhthird valnxtstr DimGrey))
          ;; If showing part of value above this, then draw it (mostly cropped)
          (if (< offsety 0)
             (let ((valnxtnxt (max valmin (min valmax (+ valnxt unit)))))
               ;; Check for cycling
               (if (and cycle (= valnxt valnxtnxt))
                 (set! valnxtnxt (if topdown valmax valmin)))
               (if (not (= valnxt valnxtnxt))
                 (let ((valnxtnxtstr (if vallist (list-ref vallist (fix valnxtnxt)) (val->str valnxtnxt))))
                   (if (string? valnxtnxtstr) 
                     (glgui:draw-text-center wheelx (+ wheely wheelh offsety) wheelw (/ wheelh 3) valnxtnxtstr smlfnt DimGrey)
                     (glgui:draw-pixmap-center wheelx (+ wheely wheelh offsety) wheelw (/ wheelh 3) valnxtnxtstr DimGrey))))))))
   )
   (glCoreClipPop)

   ;; handle drag
   (let ((dragv (glgui-widget-get g wgt 'dragv))
         (drag (glgui-widget-get g wgt 'drag)))
     (if (and (> (abs dragv) 1.) drag)
       ;; Get difference in time since last draw, and determine fake y drag value
       (let* ((timediff (- ##now drag))
              (deltay (* dragv timediff))
              (hunit (abs (/ unit 2.)))    ;; if topdown, then unit is negative - but want to treat half unit as positive here
              (newval0 (fl+ val0 (fl* (fl/ deltay (fl/ (flo h) 3.)) (flo (- unit)))))
              (newval (if cycle
                    ;; Handle cycling, if more than half a unit past min value when dragging then go to max values  
                    (if (< newval0 (- valmin hunit)) 
                      (+ (- valmax (- valmin newval0)) (if topdown (- unit) unit)) 
                      ;; If more than half a unit past max value when dragging then go to min values
                      (if (> newval0 (+ valmax hunit)) 
                        (- (+ valmin (- newval0 valmax 1)) (if topdown (- unit) unit))
                        newval0))
                    newval0))
              ;; Slow down at a rate of roughly 100 pixels/s^2
              (newv (if (> dragv 0.) (max (- dragv (* timediff 100.)) 0.) (min (+ dragv (* timediff 100.)) 0.))))
         (if (< (abs newv) 1.)
           ;; If new velocity is less than 1, stop movement and snap in to value
           (begin
             (glgui-widget-set! g wgt 'drag #f)
             (glgui-widget-set! g wgt 'value (* (round (/ (max valmin (min valmax newval)) unit)) unit))
             (glgui-widget-set! g wgt 'dragv 0)
             (if cb (cb g wgt 0 0 0)))
           ;; Otherwise update value and set new velocity
           (begin
             (glgui-widget-set! g wgt 'drag ##now)
             (if cycle
               ;; Keep within min - half a unit and max + half a unit if cycling
               (begin
                 (glgui-widget-set! g wgt 'value (max (- valmin hunit) (min (+ valmax hunit) newval)))
                 (glgui-widget-set! g wgt 'dragv newv))
               ;; If not cycling, keep within min and max
               (let ((value (max valmin (min valmax newval))))
                 (glgui-widget-set! g wgt 'value value)
                 (if (or (= valmin value) (= valmax value))  
                   ;; If at one of the extreme values, stop auto-scroll
                   (begin
                     (glgui-widget-set! g wgt 'drag #f)
                     (glgui-widget-set! g wgt 'dragv 0)
                     (if cb (cb g wgt 0 0 0)))
                   ;; Otherwise update velocity
                   (glgui-widget-set! g wgt 'dragv newv)))))))))
))

(define (glgui:verticalnumberwheel-calc-texture-width width)
  (let loop ((p 1))
    (if (fx>= p width)
        ;; Return the pixel width divided by the power of 2 just greater than it
        (flo (/ width p))
        (loop (* p 2))))
)

(define (glgui:verticalnumberwheel-input g wgt type mx my)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (h (fix (glgui-widget-get-dyn g wgt 'h)))
         (m (+ y (/ h 2)))  ;; middle         
         (cb (glgui-widget-get g wgt 'callback))
         (cycle (glgui-widget-get g wgt 'cycle))
         (value (flo (glgui-widget-get g wgt 'value)))
         (valmin (flo (glgui-widget-get g wgt 'valmin)))
         (valmax (flo (glgui-widget-get g wgt 'valmax)))
         ;; Top down flips everything so that value increase downward (unit becomes negative)
         (topdown (glgui-widget-get g wgt 'topdown))
         (unit (if topdown (* -1 (glgui-widget-get g wgt 'unit)) (glgui-widget-get g wgt 'unit)))
         (down (glgui-widget-get g wgt 'down))
         (drag (glgui-widget-get g wgt 'drag))
         (oldys (glgui-widget-get g wgt 'oldys))
         (fsty (fix (glgui-widget-get g wgt 'fsty)))
         (armcenter (glgui-widget-get g wgt 'armcenter))
         (armtop (glgui-widget-get g wgt 'armtop))
         (armbottom (glgui-widget-get g wgt 'armbottom))
         (inside (and (fx> (fix mx) x) (fx< (fix mx) (fx+ x w)) (fx> (fix my) y) (fx< (fix my) (fx+ y h))))
         (loc_center (and inside (< (abs (- my m)) 27)))
         (loc_top   (and inside (not loc_center) (< my m) (> my (- m 70))))
         (loc_bottom  (and inside (not loc_center) (> my m) (< my (+ m 70)))))
     (cond
       ;; touch down
       ((and inside (fx= type EVENT_BUTTON1DOWN))
         ;; prepare for drag
         (glgui-widget-set! g wgt 'down value)
         (glgui-widget-set! g wgt 'oldys (list my 0 0))
         (glgui-widget-set! g wgt 'fsty my)
         (glgui-widget-set! g wgt 'drag #f)
         ;; Stop any previous automatic scrolling until release
         (if (glgui-widget-get g wgt 'dragv)
           (begin
             (glgui-widget-set! g wgt 'dragv 0)
             (glgui-widget-set! g wgt 'value (max valmin (min valmax (* (round (/ value unit)) unit))))))
         ;; arm buttons
         (glgui-widget-set! g wgt 'armcenter loc_center)
         (glgui-widget-set! g wgt 'armtop loc_top)
         (glgui-widget-set! g wgt 'armbottom loc_bottom)
       )
       ;; drag
       ((and down (fx= type EVENT_MOTION))
         (if (> (fxabs (fx- (fix my) fsty)) 5)
           ;; Determine distance in y since last motion or down
           (let* ((deltay (flo (fx- (fix my) (fix (car oldys)))))
                  ;; if topdown, then unit is negative - but want to treat half unit as positive here
                  (hunit (abs (/ unit 2.)))
                  ;; One unit change mapped to vertical movement of the height of one of the three rows of the wheel
                  (newval0 (fl+ value (fl* (fl/ deltay (fl/ (flo h) 3.)) (flo (- unit)))))
                  (newval (if cycle
                    ;; Handle cycling, if more than half a unit past min value when dragging then go to max values  
                    (if (< newval0 (- valmin hunit)) 
                      (+ (- valmax (- valmin newval0)) (if topdown (- unit) unit)) 
                      ;; If more than half a unit past max value when dragging then go to min values
                      (if (> newval0 (+ valmax hunit)) 
                        (- (+ valmin (- newval0 valmax 1)) (if topdown (- unit) unit))
                        newval0))
                    newval0)))
               (if cycle
                 ;; Keep within min - half a unit and max + half a unit if cycling
                 (glgui-widget-set! g wgt 'value (max (- valmin hunit) (min (+ valmax hunit) newval)))
                 (glgui-widget-set! g wgt 'value (max valmin (min valmax newval))))
               ;; Save current y with past two y values
               (glgui-widget-set! g wgt 'oldys (list my (car oldys) (cadr oldys)))
               (if (not drag)
                 (glgui-widget-set! g wgt 'drag ##now)))))

       ;; touch release
       ((and down (fx= type EVENT_BUTTON1UP)) 
         (if drag
           ;; If dragging has occurred
           (let* ((oldy1 (caddr oldys))
                  (oldy2 (cadr oldys))
                  (oldy3 (car oldys))
                  (recentdeltay (+ (- oldy3 oldy2) (- oldy2 oldy1)))   
                  (totaldeltay (- my fsty))
                  (dragv (/ (flo totaldeltay) (- ##now drag))))
              (if (and (> (abs dragv) 100.) (> (abs recentdeltay) 8.))
                (begin
                  ;; Setup continuous decaying scroll when user releases, don't go faster than 400 pixels/s
                  (glgui-widget-set! g wgt 'dragv (min (max dragv -300.0) 300.0))
                  (glgui-widget-set! g wgt 'drag ##now))
                (begin
                  ;; Otherwise snap-in value, no more movement
                  (glgui-widget-set! g wgt 'drag #f)
                  (glgui-widget-set! g wgt 'value (max valmin (min valmax (* (round (/ value unit)) unit)))))))
           ;; Otherwise snap-in value, no more movement
           (begin
             (glgui-widget-set! g wgt 'drag #f)
             (glgui-widget-set! g wgt 'value (max valmin (min valmax (* (round (/ value unit)) unit))))))
         (cond 
           ;; If down and up on the top or on the bottom (clicking on top or bottom) descrease or increase by one wheel spot
           ((and loc_top armtop (= down value))
              (let* ((valup (min valmax (max valmin (- value unit))))
                    ;; If at extreme value already and cycle is on move to other extreme value
                    (newval (if (and cycle (= valup value)) (if (= value valmin) valmax valmin) valup)))
                (glgui-widget-set! g wgt 'value newval))
           )
           ((and loc_bottom armbottom (= down value)) 
              (let* ((valdown (max valmin (min valmax (+ value unit))))
                    ;; If at extreme value already and cycle is on move to other extreme value
                    (newval (if (and cycle (= valdown value)) (if (= value valmin) valmax valmin) valdown)))
                (glgui-widget-set! g wgt 'value newval))
           )
         )
         (glgui-widget-set! g wgt 'down #f)
         (glgui-widget-set! g wgt 'armcenter #f)
         (glgui-widget-set! g wgt 'armtop #f)
         (glgui-widget-set! g wgt 'armbottom #f)
         ;; Trigger callback when button up if not flicking - if no drag velocity
         (if (and cb (not (glgui-widget-get g wgt 'drag))) (cb g wgt 0 0 0))
       )
       (else
          (glgui-widget-set! g wgt 'down #f)
          (glgui-widget-set! g wgt 'armcenter #f)
          (glgui-widget-set! g wgt 'armtop #f)
          (glgui-widget-set! g wgt 'armbottom #f)
       )
    )
  ;; Make a drag started here only affect this component, not things under it
  (or inside drag) 
))

(define (glgui-verticalnumberwheel g x y w h vmin vmax icon coloricon colorvalue colorshade font1 font2 icon2 . steps)
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
       'armcb #f
       'draw-handle  glgui:verticalnumberwheel-draw
       'input-handle glgui:verticalnumberwheel-input
       'hidden #f
       ;; If a list of values, min and max specify range of indices
       'value (if vallist 0 vmin)
       'valmin (if vallist 0 vmin)
       'valmax (if vallist (- (length vallist) 1) vmax)
       'vallist vallist
       'down  #f
       'drag  #f
       'dragy 0
       'dragv 0
       'oldys (list 0 0 0)
       'fsty  0
       'cycle #f       ;; If set to true, then the wheel will allow cycling - going past highest value to start back at lowest
       'sprite icon
       'sprite2 icon2
       'coloricon coloricon
       'colorvalue colorvalue
       'colorshade colorshade
       'bigfnt font1
       'smlfnt font2
       'unit unit
       'armcenter #f
       'armtop #f
       'armbottom #f
       ;; Topdown can be set to true to make the max value be displayed at the bottom instead of the top
       'topdown #f
     )))

;; eof
