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

;; number wheel widget

(define (glgui:numberwheel-draw g wgt)
  (define (val->str v) (float->autostring v 1))
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (colorshade (glgui-widget-get-dyn g wgt 'colorshade))
         (colorvalue (glgui-widget-get-dyn g wgt 'colorvalue))
         (coloricon (glgui-widget-get-dyn g wgt 'coloricon))
         (sprite (glgui-widget-get-dyn g wgt 'sprite))
         (sprite2 (glgui-widget-get-dyn g wgt 'sprite2))
         (bigfnt (glgui-widget-get-dyn g wgt 'bigfnt))
         (smlfnt (glgui-widget-get-dyn g wgt 'smlfnt))
         (valmin (flo (glgui-widget-get g wgt 'valmin)))
         (valmax (flo (glgui-widget-get g wgt 'valmax)))
         (vallist (glgui-widget-get g wgt 'vallist))
         (cb (glgui-widget-get g wgt 'callback))
         (armcb (glgui-widget-get g wgt 'armcb))
         (down (glgui-widget-get g wgt 'down))
         (val0 (flo (glgui-widget-get g wgt 'value)))
         (unit (glgui-widget-get g wgt 'unit))
         (val (* (round (/ val0 unit)) unit))
         (valoffset (- val val0))
         (valprv (max valmin (- val unit)))
         (valnxt (min valmax (+ val unit)))
         ;; Below strings may actually be textures if textures in vallist
         (valstr (if vallist (list-ref vallist (fix val)) (val->str val)))
         (valprvstr (if vallist (list-ref vallist (fix valprv)) (val->str valprv)))
         (valnxtstr (if vallist (list-ref vallist (fix valnxt)) (val->str valnxt))))
   (if colorshade (glgui:draw-box x y w h colorshade))
   (if sprite
     (let ((sh (cadr sprite)))
     (glCoreColor coloricon)
     (apply glCoreTextureDraw (append (list (+ x 10) (+ y (/ (- h sh) 2))) sprite (list 0.)))
   ))
   (if sprite2
     (let ((sw (car sprite2))(sh (cadr sprite2)))
     (glCoreColor White)
     (apply glCoreTextureDraw (append (list (+ x w -10 (- sw)) (+ y (/ (- h sh) 2))) sprite2 (list 0.)))
   ))
   (let* ((wheelw (car glgui_numberwheel.img))
          (wheelwthird (/ wheelw 3))
          (wheelh (cadr glgui_numberwheel.img))
          (wheelx (+ x (/ (- w wheelw) 2)))
          (wheely (+ y (/ (- h wheelh) 2)))
          (offsetx (* (/ valoffset unit) wheelwthird)))
     (glCoreColor (if down White LightGray)) ;; tactile display?
     (apply glCoreTextureDraw (append (list wheelx wheely) glgui_numberwheel.img (list 0)))
     ;; Apply cropping to numbers/images/strings on wheel (note that left and right edges of pixmap are black so don't include them)
     (glCoreClipPush (fl+ (flo wheelx) 3.0) (flo wheely) (fl+ (fl- (flo wheelx) 5.0) (flo wheelw)) (fl+ (flo wheely) (flo wheelh)))
     (if (string? valstr) 
        (glgui:draw-text-center (+ wheelx offsetx) wheely wheelw wheelh valstr bigfnt colorvalue)
        (glgui:draw-pixmap-center (+ wheelx offsetx) wheely wheelw wheelh valstr colorvalue))
     ;; Draw value left of the current one if any
     (if (not (= val valprv))
        (begin
          (if (string? valprvstr)
            (glgui:draw-text-center (+ wheelx offsetx) wheely wheelwthird wheelh valprvstr smlfnt DimGrey)
            (glgui:draw-pixmap-center (+ wheelx offsetx) wheely wheelwthird wheelh valprvstr DimGrey))
          ;; Draw the value further left of that, if part of it is showing (mostly cropped)
          (if (> offsetx 0)
             (let ((valprvprv (max valmin (- valprv unit))))
               ;; If a previous previous value to show, draw it
               (if (not (= valprv valprvprv))
                 (let ((valprvprvstr (if vallist (list-ref vallist (fix valprvprv)) (val->str valprvprv))))
                   (if (string? valprvprvstr) 
                     (glgui:draw-text-center (+ (- wheelx wheelwthird) offsetx) wheely wheelwthird wheelh valprvprvstr smlfnt DimGrey)
                     (glgui:draw-pixmap-center (+ (- wheelx wheelwthird) offsetx) wheely wheelwthird wheelh valprvprvstr DimGrey))))))))
     ;; Draw value right of the current one if any
     (if (not (= val valnxt)) 
        (begin
          (if (string? valnxtstr) 
            (glgui:draw-text-center (+ wheelx (* 2 wheelwthird) offsetx) wheely wheelwthird wheelh valnxtstr smlfnt DimGrey)
            (glgui:draw-pixmap-center (+ wheelx (* 2 wheelwthird) offsetx) wheely wheelwthird wheelh valnxtstr DimGrey))
         ;; Draw the value further right of that, if part of it is showing (mostly cropped)
          (if (< offsetx 0)
             (let ((valnxtnxt (min valmax (+ valnxt unit))))
               ;; If a next next value to show, draw it
               (if (not (= valnxt valnxtnxt))
                 (let ((valnxtnxtstr (if vallist (list-ref vallist (fix valnxtnxt)) (val->str valnxtnxt))))
                   (if (string? valnxtnxtstr) 
                     (glgui:draw-text-center (+ wheelx wheelw offsetx) wheely wheelwthird wheelh valnxtnxtstr smlfnt DimGrey)
                     (glgui:draw-pixmap-center (+ wheelx wheelw offsetx) wheely wheelwthird wheelh valnxtnxtstr DimGrey))))))))
   )
   (glCoreClipPop)
   ;; handle drag
   (let ((curval (glgui-widget-get g wgt 'value))
         (drag   (glgui-widget-get g wgt 'drag))
         (dragv   (glgui-widget-get g wgt 'dragv)))
     (if (and drag (> (abs dragv) 1.)) 
        ;;(let* ((delta (* -0.0075 dragv unit)))
        ;;(let* ((delta (* (if (> dragv 0) -1. 1.) 0.00001 (* dragv dragv) unit)))
        (let ((delta (* -0.01 dragv unit)))
        ;;  (for-each display (list "> delta=" delta "\n"))
          (glgui-widget-set! g wgt 'value (max valmin (min valmax (+ curval delta))))
          (glgui-widget-set! g wgt 'dragv 0.)
       )))

))

(define (glgui:numberwheel-input g wgt type mx my)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (m (+ x (/ w 2)))  ;; middle
         (h (fix (glgui-widget-get-dyn g wgt 'h)))         
         (cb (glgui-widget-get g wgt 'callback))
         (value (glgui-widget-get g wgt 'value))
         (valmin (flo (glgui-widget-get g wgt 'valmin)))
         (valmax (flo (glgui-widget-get g wgt 'valmax)))
         (unit (glgui-widget-get g wgt 'unit))
         (down (glgui-widget-get g wgt 'down))
         (drag (glgui-widget-get g wgt 'drag))
         (armcenter (glgui-widget-get g wgt 'armcenter))
         (armleft (glgui-widget-get g wgt 'armleft))
         (armright (glgui-widget-get g wgt 'armright))
         (inside (and (fx> (fix mx) x) (fx< (fix mx) (fx+ x w)) (fx> (fix my) y) (fx< (fix my) (fx+ y h))))
         (loc_center (and inside (< (abs (- mx m)) 35)))
         (loc_left   (and inside (not loc_center) (< mx m) (> mx (- m 89))))
         (loc_right  (and inside (not loc_center) (> mx m) (< mx (+ m 89)))))
         
     (cond
       ;; touch down
       ((and inside (fx= type EVENT_BUTTON1DOWN))
         ;; prepare for drag
         (glgui-widget-set! g wgt 'down value)
         (glgui-widget-set! g wgt 'drag #f)
         ;; arm buttons
         (glgui-widget-set! g wgt 'armcenter loc_center)
         (glgui-widget-set! g wgt 'armleft loc_left)
         (glgui-widget-set! g wgt 'armright loc_right)
       )
       ;; drag
       ((and down (fx= type EVENT_MOTION))
          (glgui-widget-set! g wgt 'dragv (if drag 
            (let* ((dragx (glgui-widget-get g wgt 'dragx))
                   (dragv (/ (- mx dragx) (- ##now drag)))) 
               dragv) 0))
           (glgui-widget-set! g wgt 'dragx mx)
           (glgui-widget-set! g wgt 'drag ##now)
           (glgui-widget-set! g wgt 'armcb #f)
       )
       ;; touch release
       ((and down (fx= type EVENT_BUTTON1UP)) 
         (if down (glgui-widget-set! g wgt 'value (* (round (/ value unit)) unit)))
         (cond 
          ;; ((and loc_center down (= down value) cb) (glgui-widget-set! g wgt 'armcb ##now))
          ;; ((and loc_center armcenter down (= down value) cb) 
          ;;    (glgui-widget-set! g wgt 'value (* (round (/ value unit)) unit))
          ;;   (cb g wgt 0 0 0))
           
           ;; If down and up on the left or on the right (clicking on left or right) descrease or increase by one wheel spot
           ((and loc_left armleft down (= down value))
                (glgui-widget-set! g wgt 'value (max valmin (- value unit)))
           )
           ((and loc_right armright down (= down value)) 
                (glgui-widget-set! g wgt 'value (min valmax (+ value unit)))
           )
         )
         (glgui-widget-set! g wgt 'down #f)
         (glgui-widget-set! g wgt 'drag #f)
         (glgui-widget-set! g wgt 'armcenter #f)
         (glgui-widget-set! g wgt 'armleft #f)
         (glgui-widget-set! g wgt 'armright #f)
        
         ;; Trigger callback when button up
         (if cb (cb g wgt 0 0 0))
       )
       (else 
          (glgui-widget-set! g wgt 'down #f)
          (glgui-widget-set! g wgt 'drag #f)
          (glgui-widget-set! g wgt 'armcenter #f)
          (glgui-widget-set! g wgt 'armleft #f)
          (glgui-widget-set! g wgt 'armright #f)
       )
    )
  (or inside drag)
))

(define (glgui-numberwheel g x y w h vmin vmax icon coloricon colorvalue colorshade font1 font2 icon2 . steps)
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
       'draw-handle  glgui:numberwheel-draw
       'input-handle glgui:numberwheel-input
       'hidden #f
       ;; If a list of values, min and max specify range of indices
       'value (if vallist 0 vmin)
       'valmin (if vallist 0 vmin)
       'valmax (if vallist (- (length vallist) 1) vmax)
       'vallist vallist
       'down  #f
       'drag  #f
       'dragx 0
       'dragv 0
       'sprite icon
       'sprite2 icon2
       'coloricon coloricon
       'colorvalue colorvalue
       'colorshade colorshade
       'bigfnt font1
       'smlfnt font2
       'unit unit
       'armcenter #f
       'armleft #f
       'armright #f
     )))

;; eof
