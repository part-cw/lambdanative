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
;; slider widget

;; Width of the value label on the right side
(define glgui:slider:vw 54)

(define (glgui:slider-draw g wgt)
  (define (val->str v) (float->string v 1))
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (barh (glgui-widget-get g wgt 'barh))
         (barrounded (glgui-widget-get g wgt 'barrounded))
         (handlew (glgui-widget-get g wgt 'handlew))
         (halfhandlew (/ handlew 2))
         (handleh (glgui-widget-get g wgt 'handleh))
         (colorvalue (glgui-widget-get g wgt 'colorvalue))
         (colorbarleft (glgui-widget-get g wgt 'colorbarleft))
         (colorbarright (glgui-widget-get g wgt 'colorbarright))
         (colorhandle (glgui-widget-get g wgt 'colorhandle))
         (handleimg (glgui-widget-get g wgt 'handleimg))
         (bgcolor (glgui-widget-get g wgt 'bgcolor))
         (showvalue (glgui-widget-get g wgt 'showvalue))
         (showlabels (glgui-widget-get g wgt 'showlabels))
         (coloricon (glgui-widget-get g wgt 'coloricon))
         (coloricon2 (glgui-widget-get g wgt 'coloricon2))
         (sprite (glgui-widget-get g wgt 'sprite))
         (sprite2 (glgui-widget-get g wgt 'sprite2))
         (valmin (flo (glgui-widget-get g wgt 'valmin)))
         (valmax (flo (glgui-widget-get g wgt 'valmax)))
         (down (glgui-widget-get g wgt 'down))
         (val0 (flo (glgui-widget-get g wgt 'value)))
         (unit (glgui-widget-get g wgt 'unit))
         (val (* (round (/ val0 unit)) unit))
         (valprv (max valmin (- val unit)))
         (valnxt (min valmax (+ val unit)))
         (valstr (val->str val)))
   (if bgcolor (glgui:draw-box x y w h bgcolor))
    
   ;; Draw string for current value if it should be shown
   (if showvalue
     (let ((valfnt (glgui-widget-get g wgt 'valfnt)))
       (glgui:draw-text-center (- (+ x w) glgui:slider:vw) y glgui:slider:vw h valstr valfnt colorvalue)
       (set! w (- w 2 glgui:slider:vw))))
    
   ;; Draw icons
   (if sprite2
     (let ((sw (car sprite2)) (sh (cadr sprite2)))
       (glCoreColor (if coloricon2 coloricon2 White))
       (apply glCoreTextureDraw (append (list (- (+ x w) 5 sw) (+ y (/ (- h sh) 2))) sprite2 (list 0.)))
       (set! w (- w sw 12))
   ))
   (set! x (+ x 3 halfhandlew))
   (set! w (- w 3 halfhandlew))
   (if sprite
     (let ((sw (car sprite)) (sh (cadr sprite)))
       (glCoreColor (if coloricon coloricon White))
       (apply glCoreTextureDraw (append (list x (+ y (/ (- h sh) 2))) sprite (list 0.)))
       (set! x (+ x sw 7))
       (set! w (- w sw 12))
   ))
   (set! w (- w 3 halfhandlew))
   ;; Set slider xmin and range
   (glgui-widget-set! g wgt 'xmin x)
   (glgui-widget-set! g wgt 'xrange w)
    
   ;; Set slider scale - number to multiply by to go from x difference to value difference
   (glgui-widget-set! g wgt 'scale (inexact->exact (/ (- valmax valmin) w)))
   
   ;; Draw slider bar and handle
   (let* ((range (- valmax valmin))
          (perc (/ (- val valmin) range))
          (bw1 (* w perc))
          (bw2 (- w bw1))
          (hmx (+ x bw1))
          (hx (- hmx halfhandlew))
          (labelfnt (glgui-widget-get g wgt 'labelfnt))
          (lh (if (and showlabels labelfnt) (apply + (glgui:stringheight "1" labelfnt)) 0))
          (hneeded (if showlabels (+ handleh 7 lh) handleh))
          ;; If showing labels, centering includes labels so include these in y-coordinate calculations for handle and bar
          (hy (+ (/ (- h hneeded) 2) (if showlabels (+ y lh 7) y)))
          (by (+ hy (/ (- handleh barh) 2))))
      
      ;; Remember bar x, y and width for input-handle
      (glgui-widget-set! g wgt 'barx x)
      (glgui-widget-set! g wgt 'bary by)
      (glgui-widget-set! g wgt 'barw w)
     
      ;; Draw labels, if turned on
      (if showlabels
        (let* ((showticks (glgui-widget-get g wgt 'showticks))
               (ly (+ y (/ (- h hneeded) 2)))
               (stepnum (fix (/ range unit)))
               ;; difference in x between tick marks
               (dx (/ w stepnum))
               (dxh (/ dx 2))
               (endx (+ x w)))
          (let loop ((cur valmin) (lx x))
            (if (<= lx endx) (begin
              (glgui:draw-text-center (- lx dxh) ly dx lh (val->str cur) labelfnt colorvalue)
              ;; tick marks just span distance between bottom of handle and bottom of bar
              (if showticks (glgui:draw-box lx by 2 (- hy by) colorvalue))
              (loop (+ cur unit) (+ lx dx)))))))
     
      ;; Draw bar on both sides of handle
      (if barrounded
         ;; If the bar is supposed to be rounded, don't want size of rounding to change, so keep almost full length rounded boxes
         (begin
           (glgui:draw-rounded-box x by (- w 6) barh colorbarleft)
           (glgui:draw-rounded-box (+ x 6) by (- w 6) barh colorbarright)
           (glgui:draw-box (+ x 5) (- by 1) (- bw1 5) barh colorbarleft)
           (glgui:draw-box hmx (- by 1) (- bw2 5) barh colorbarright))
          (begin
           (glgui:draw-box x by bw1 barh colorbarleft)
           (glgui:draw-box hmx by bw2 barh colorbarright)))
     
      ;; Draw handle (may be pixmap) and store location
      (if handleimg
        (glgui:draw-pixmap-center hx hy handlew handleh handleimg colorhandle)
        (glgui:draw-box hx hy handlew handleh colorhandle))
      (glgui-widget-set! g wgt 'handlex hx)
      (glgui-widget-set! g wgt 'handley hy))
))

(define (glgui:slider-input g wgt type mx my)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (m (+ x (/ w 2)))  ;; middle
         (h (fix (glgui-widget-get-dyn g wgt 'h)))  
         (hx (fix (glgui-widget-get g wgt 'handlex)))
         (hy (fix (glgui-widget-get g wgt 'handley)))
         (handlew (glgui-widget-get g wgt 'handlew))
         (handleh (glgui-widget-get g wgt 'handleh))
         (bx (fix (glgui-widget-get g wgt 'barx)))
         (by (min (fix (glgui-widget-get g wgt 'bary)) hy))
         (bw (fix (glgui-widget-get g wgt 'barw)))
         (bh (max (fix (glgui-widget-get g wgt 'barh)) handleh))
         (minx (fix (glgui-widget-get g wgt 'xmin)))
         (rangex (fix (glgui-widget-get g wgt 'xrange)))
         (cb (glgui-widget-get g wgt 'callback))
         (now (time->seconds (current-time)))
         (value (glgui-widget-get g wgt 'value))
         (valmin (glgui-widget-get g wgt 'valmin))
         (valmax (glgui-widget-get g wgt 'valmax))
         (scale (glgui-widget-get g wgt 'scale))
         (unit (glgui-widget-get g wgt 'unit))
         (downx (glgui-widget-get g wgt 'downx))
         (downval (glgui-widget-get g wgt 'downval))
         (drag (glgui-widget-get g wgt 'drag))
         ;; Inside the handle - pressing from current value
         (fromhandle (and (fx> (fix mx) hx) (fx< (fix mx) (fx+ hx handlew))
             (fx> (fix my) y) (fx< (fix my) (fx+ y h))))
         ;; Just near the bar - want to allow pressing anywhere near the slider
         (nearbar (and (fx> (fix mx) x) (fx< (fix mx) (fx+ x w))
             (fx> (fix my) y) (fx< (fix my) (fx+ y h)))))
    
     ;; If pressing passed the ends of the bar, fake within the ends
     (if nearbar
       (if (fx< mx bx) 
         (set! mx bx)
         (if (fx> mx (+ bx bw)) 
           (set! mx (+ bx bw)))))
    
     (cond
       ;; touch down
       ((and nearbar (fx= type EVENT_BUTTON1DOWN))
        
         ;; Check if press is not at current value
         (if (not fromhandle)
           ;; Move slider to where they pressed
           (let* ((newx (- mx bx))
                  (newval (* newx scale))
                  (rawval (min valmax (max valmin newval)))
                  (val (flo (* (* (round (/ rawval unit)) unit) 1.0))))
               (glgui-widget-set! g wgt 'value val)
               (glgui-widget-set! g wgt 'downval val))
           ;; Otherwise just remember starting value
           (glgui-widget-set! g wgt 'downval value))
        
         ;; Remember starting x
         (glgui-widget-set! g wgt 'downx mx)
       )
       ;; drag
       ((and downval (fx= type EVENT_MOTION))
          ;; Determine distance from start point, change value
          (let* ((diffx (- mx downx))
                 (diffval (* diffx scale))
                 (uval (+ downval diffval))
                 (rawval (min valmax (max valmin uval)))
                 (val (* (* (round (/ rawval unit)) unit) 1.0)))
            (glgui-widget-set! g wgt 'value (flo val))
          )
       )
       ;; touch release
       ((and (or downval nearbar) (fx= type EVENT_BUTTON1UP))
             
         (glgui-widget-set! g wgt 'downval #f)
         (glgui-widget-set! g wgt 'downx #f)
            
         ;; Trigger callback when button up
         (if cb (cb g wgt type mx my))
       )
       (else 
          (glgui-widget-set! g wgt 'downval #f)
          (glgui-widget-set! g wgt 'downx #f)
       )
    )
  nearbar
))

(define (glgui-slider g x y w h vmin vmax icon coloricon colorvalue colorhandle bgcolor font1 font2 icon2 coloricon2 . steps)
  (let ((unit
          (if (= (length steps) 1) 
            ;; If stepsize given, use it
            (car steps)
            ;; If not, compute the step size
            (let ((a (- vmax vmin)))
              (cond ((> a 10.) 1.)
                ((> a 1.) 0.5)
                (else 0.1))))))
    (glgui-widget-add g
       'x x
       'y y
       'w w
       'h h
       'callback #f
       'draw-handle  glgui:slider-draw
       'input-handle glgui:slider-input
       'hidden #f
       'value vmin
       'valmin vmin
       'valmax vmax
       'down  #f
       'drag  #f
       'dragx 0
       'dragv 0
       'sprite icon
       'sprite2 icon2
       'coloricon coloricon
       'coloricon2 coloricon2
       'colorvalue colorvalue
       'colorhandle colorhandle
       'handleimg #f               ;; An image can be used as the slider handle instead of a rectangle 
       'colorbarleft colorvalue    ;; Colour of bar to the left of the current handle location
       'colorbarright colorvalue   ;; Colour of the bar to the right of the current handle location
       'showvalue #f               ;; Whether to show a string of the value to the right of the scale
       'showlabels #t              ;; Whether to show labels along the length of the slider
       'showticks #t               ;; If showlabels is turned on, should tick marks be put above numbers
       'bgcolor bgcolor
       'valfnt font1
       'labelfnt font2
       'unit unit
       'barh 6                     ;; Height of the bar that the handle slides along
       'barrounded #f              ;; Whether the bar, which is a rectangle, should be rounded
       'handlew 8                  ;; Width of the handle that slides
       'handleh 20                 ;; Height of the handle that slides
       'handlex 0     ;; Below 5 parameters are updated in draw and used in input handle 
       'handley 0
       'barx 0
       'bary 0
       'barw 0
       'scale 1.
       'xmin 0
       'xrange 0
     )))

;; eof
