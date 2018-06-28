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
;; button widget

;; XXX if button changes, these needs updating
(define glgui_button_l.img (list 198 40 glgui_button.raw 0. 1. .37343750000000000000 .37500000000000000000))
(define glgui_button_r.img (list 198 40 glgui_button.raw 0.3 1. .77343750000000000000 .37500000000000000000))
(define glgui_button_m.img (list 198 40 glgui_button.raw 0.2 1. .57343750000000000000 .37500000000000000000))

;; Draw text for the button with a given alignment
(define (glgui:button-aligned-label-draw x y w h label fnt color align)
  (cond
    ((= align GUI_ALIGNCENTER) (glgui:draw-text-center x y w h label fnt color))
    ((= align GUI_ALIGNLEFT) (glgui:draw-text-left x y w h label fnt color))
    (else (glgui:draw-text-right x y w h label fnt color))))

(define (glgui:button-draw g wgt)
  (let ((x (glgui-widget-get-dyn g wgt 'x))
        (y (glgui-widget-get-dyn g wgt 'y))
        (w (glgui-widget-get-dyn g wgt 'w))
        (h (glgui-widget-get-dyn g wgt 'h))
        (a (glgui-widget-get-dyn g wgt 'armed))
        (i (glgui-widget-get g wgt 'image))
        (icon (glgui-widget-get g wgt 'icon))
        (r (glgui-widget-get g wgt 'rounded))
        (c (glgui-widget-get g wgt 'color))
        (sc (glgui-widget-get g wgt 'solid-color))
        (tsc (glgui-widget-get g wgt 'toggle-selected-color))
        (tnc (glgui-widget-get g wgt 'toggle-normal-color))
        (bsc (glgui-widget-get g wgt 'button-selected-color))
        (bnc (glgui-widget-get g wgt 'button-normal-color))
        (f (glgui-widget-get g wgt 'font))
        (align (glgui-widget-get g wgt 'align)))
  ;; toggle button
  (if (list? (car i))
    (let* ((n (length i))
           (dw (fix (/ w n)))
           (val0 (glgui-widget-get g wgt 'value))
           (val (if (number? val0) (list val0) val0)))
      (let loop ((j 0) (is i) (cs c) (icons icon))
        (if (< j n) (begin
          (if sc
            ;; If a solid colour, just draw a box for each button
            (glgui:draw-box (+ x (* j dw)) y (- dw 1) h (if (member j val) tsc tnc))
            (begin 
              (glCoreColor (if (member j val) tsc tnc))
              (apply glCoreTextureDraw
                (append (list (+ x (* j dw)) y (- dw 1) h) (cddr 
                  (if (= j 0) glgui_button_l.img
                    (if (= j (- n 1)) glgui_button_r.img
                      glgui_button_m.img))) (list 0.)))))
         (if f ;; This allows string-based toggle buttons
          (let* ((str (caar is))
                 (multi (and str (fx> (string-length str) 0) (glgui-widget-get g wgt 'multiline)))
                 (d (glgui-widget-get g wgt 'direction)))
           (if icons  ;; This allows the string-based toggle buttons to also have images on them
             (let* ((cx (fix (+ x (* j dw) )))
                    (sp (glgui-widget-get g wgt 'icon-space))
                    ;; If place icon to the left of text
                    (al (glgui-widget-get g wgt 'icon-align))
                    (cicon (if (list? icons) (car icons) icons))
                    (iw (car cicon))
                    (ih (cadr cicon)))
               (glCoreColor (if (list? cs) (car cs) cs))
               ;; If icon to the left or right of the text
               (if (or (= al GUI_ALIGNLEFT) (= al GUI_ALIGNRIGHT))
                  ;; Determine where to draw icon and string
                  (let* ((labelsplit (if multi ((if (= d GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) str (flo (- dw iw sp)) f) #f))
                         (sw (if multi (apply max (map (lambda (s) (glgui:stringwidth s f)) labelsplit)) (glgui:stringwidth str f)))
                         (ax (fix (+ cx (/ (- dw iw sw sp) 2.))))
                         (iy (fix (+ y (/ (- h ih) 2.))))
                         (left (= al GUI_ALIGNLEFT)))
                    (apply glCoreTextureDraw (append (list (if left ax (+ ax sw sp)) iy iw ih) (cddr cicon) (list 0.)))
                    (if multi
                      (glgui:button-draw-multiline-label (if left (+ ax iw sp) ax) y sw h labelsplit f c align)
                      (glgui:button-aligned-label-draw (if left (+ ax iw sp) ax) y sw h str f c align)))
                  ;; Determine where to draw icon and string
                  (let* ((sh (glgui:fontheight f))
                         (labelsplit (if multi ((if (= d GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) str (flo dw) f) #f))
                         (ix (fix (+ cx (/ (- dw iw) 2.))))
                         (ay (fix (+ y (/ (- h ih sh sp) 2.))))
                         (bottom (= al GUI_ALIGNBOTTOM)))
                    (apply glCoreTextureDraw (append (list ix (if bottom ay (+ ay sh sp)) iw ih) (cddr cicon) (list 0.)))
                    (if multi
                      (glgui:button-draw-multiline-label cx (if bottom (+ ay ih sp) ay) dw sh labelsplit f c align)
                      (glgui:button-aligned-label-draw cx (if bottom (+ ay ih sp) ay) dw sh str f c align)))))
             ;; No icon, so just draw text for the current button
             (if multi
               (let ((labelsplit ((if (= d GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) str (flo dw) f)))
                 ;; Multiline text
                 (glgui:button-draw-multiline-label (fix (+ x (* j dw) )) y dw h labelsplit f (if (list? cs) (car cs) cs) align))
               ;; Single line text
               (glgui:button-aligned-label-draw (fix (+ x (* j dw) )) y dw h str f (if (list? cs) (car cs) cs) align))))
           ;; Not string-based, so draw texture
           (let* ((sw (car (car is))) (sh (cadr (car is)))
              ;; (sx (fix (+ x (* j (+ dw 1)) (/ (- dw sw) 2.))))
             (sx (fix (+ x (* j dw) (/ (- dw sw) 2.))))
             (sy (fix (+ y (/ (- h sh) 2.)))))
             (glCoreColor (if (list? cs) (car cs) cs))
             (apply glCoreTextureDraw (append (list sx sy sw sh) (cddr (car is)) (list 0.)))))
         (loop (+ j 1) (cdr is) (if (list? cs) (cdr cs) cs) (if (list? icons) (cdr icons) icons))
       ))))
  ;; normal button
  (begin 
    (if sc
      ;; If a solid colour, just draw a rounded box for the button instead of the texture
      ((if r glgui:draw-rounded-box glgui:draw-box) x y w h (if a bsc bnc))
      (begin
        (glCoreColor (if a bsc bnc))
        (apply glCoreTextureDraw (append (list x y w h) (cddr glgui_button.img) (list 0.)))))
    (if f 
      (let* ((str (car i))
             (multi (and str (fx> (string-length str) 0) (glgui-widget-get g wgt 'multiline)))
             (d (glgui-widget-get g wgt 'direction)))
       (if icon
        ;; String based button with an icon as well
        (let ((sp (glgui-widget-get g wgt 'icon-space))
              ;; If place icon to the left of text
              (al (glgui-widget-get g wgt 'icon-align))
              (iw (car icon))
              (ih (cadr icon)))
          (glCoreColor c)
          ;; If icon to the left or right of the text
          (if (or (= al GUI_ALIGNLEFT) (= al GUI_ALIGNRIGHT))
             ;; Determine where to draw icon and string, get width of the string, max of all lines if multiple lines
             (let* ((labelsplit (if multi ((if (= d GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) str (flo (- w iw sp)) f) #f))
                    (sw (if multi (apply max (map (lambda (s) (glgui:stringwidth s f)) labelsplit)) (glgui:stringwidth str f)))
                    (ax (fix (+ x (/ (- w iw sw sp) 2.))))
                    (iy (fix (+ y (/ (- h ih) 2.))))
                    (left (= al GUI_ALIGNLEFT)))
               (apply glCoreTextureDraw (append (list (if left ax (+ ax sw sp)) iy iw ih) (cddr icon) (list 0.)))
               (if multi
                 (glgui:button-draw-multiline-label (if left (+ ax iw sp) ax) y sw h labelsplit f c align)
                 (glgui:button-aligned-label-draw (if left (+ ax iw sp) ax) y sw h str f c align)))
             ;; Determine where to draw icon and string, determine height of text, one or more lines
             (let* ((labelsplit (if multi ((if (= d GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) str (flo (if r (- w 10) w)) f) #f))
                    (sh (* (glgui:fontheight f) (if multi (length labelsplit) 1)))
                    ;; Position icon based on alignment
                    (ix (fix (+ x (/ (- w iw) 2.))))
                    (ay (fix (+ y (/ (- h ih sh sp) 2.))))
                    (bottom (= al GUI_ALIGNBOTTOM)))
               (apply glCoreTextureDraw (append (list ix (if bottom ay (+ ay sh sp)) iw ih) (cddr icon) (list 0.)))
               (if multi
                 (glgui:button-draw-multiline-label (if r (+ x 5) x) (if bottom (+ ay ih sp) ay) (if r (- w 10) w) sh labelsplit f c align)
                 (glgui:button-aligned-label-draw x (if bottom (+ ay ih sp) ay) w sh str f c align)))))
        ;; String based regular buttons
        (if multi
          (let ((labelsplit ((if (= d GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) str (flo (if r (- w 10) w)) f)))
            ;; Multiline text, leave more padding if button background is rounded
            (glgui:button-draw-multiline-label (if r (+ x 5) x) y (if r (- w 10) w) h labelsplit f c align))
          ;; Single line text
          (glgui:button-aligned-label-draw x y w h str f c align))))
      (let* ((sw (car i)) (sh (cadr i))
             (sx (fix (+ x (/ (- w sw) 2.))))
             (sy (fix (+ y (/ (- h sh) 2.)))))
        (glCoreColor c)
        (apply glCoreTextureDraw (append (list sx sy sw sh) (cddr i) (list 0.)))))
  ))
))

;; Draw a multiline label where strlist is list of the text lines to use as the label.
;; Draw it with alignment a in color c and given font f at x, y, within w, h.
(define (glgui:button-draw-multiline-label x y w h strlist f c a)
  (let* ((flx (flo x))
         (fly (flo y))
         (flw (flo w))
         (flh (flo h))
         (hline (flo (glgui:fontheight f)))
         (maxlines (fix (/ flh hline)))
         (labellines (if (fx> (length strlist) maxlines)
                       (list-head strlist maxlines)
                       strlist))
         (linecount (length labellines))
         (labelh (fl* (flo linecount) hline))
         (starty (fl- (fl+ fly flh) hline (fl/ (fl- flh labelh) 2.))))
    (let loop ((i 0) (yline starty))
      (if (fx< i (length labellines))
        (let ((linelabel (list-ref labellines i)))
           (glgui:button-aligned-label-draw flx yline flw hline linelabel f c a)
           (loop (fx+ i 1) (fl- yline hline))))))
)

;; process input for the button
(define (glgui:button-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x)) 
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (armed (glgui-widget-get g wgt 'armed))
         (i (glgui-widget-get g wgt 'image))
         (cb (glgui-widget-get g wgt 'callback))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (cond
       ((and (= type EVENT_BUTTON1DOWN) inside)
          (glgui-widget-set! g wgt 'armed #t))
       ((= type EVENT_BUTTON1UP) 
          (if (and armed inside) (begin 
          (if (list? (car i)) ;; it's a toggle
            (let ((oldval (glgui-widget-get g wgt 'value))
                  (newval (fix (floor (/ (* (length i) (- mx x)) w)))))
              (glgui-widget-set! g wgt 'value 
                (if (list? oldval) (if (member newval oldval) 
                  (let loop ((vs oldval)(res '()))
                    (if (= (length vs) 0) res 
                      (loop (cdr vs) (append res (if (= newval (car vs)) '() (list (car vs)))))))
                  (append oldval (list newval))) newval))
         ;;   (glgui-widget-set! g wgt 'value (fix (floor (/ (* (length i) (- mx x)) w))))
          ))
          (if (procedure? cb) (cb g wgt type mx my))))
          (glgui-widget-set! g wgt 'armed #f))
     )
  inside
))

;; De-arm button on hide so that unhiding it won't show it in its selected state
(define (glgui:button-update g wgt id val)
  (cond ((eq? id 'hidden)
          (if val (glgui-widget-set! g wgt 'armed #f)))))

(define (glgui-button g x y w h img callback)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'image img
     'callback callback
     'color White
     ;; Placed the selected and unselected background colors here so apps could change them if needed.
     'toggle-selected-color Grey
     'toggle-normal-color DimGrey
     'button-selected-color Grey
     'button-normal-color DimGrey
     'solid-color #f    ;; If solid colour, don't use button texture
     'value 0        ;; for toggles
     'armed #f
     'hidden #f
     'font #f
     'draw-handle  glgui:button-draw
     'input-handle glgui:button-input
  ))

(define (glgui-button-string g x y w h str fnt callback . icon)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'image (if (pair? str)(map list str)(list str))
     'callback callback
     'color White
     'value 0        ;; for toggles. Use list for multi-select
     ;; Placed the selected and unselected background colors here so apps could change them if needed.
     'toggle-selected-color Grey
     'toggle-normal-color DimGrey
     'button-selected-color Grey
     'button-normal-color DimGrey
     'solid-color #f    ;; If solid colour, don't use button texture
     'armed #f
     'hidden #f
     'rounded #t
     'font fnt
     'multiline #f
     'align GUI_ALIGNCENTER
     'direction GUI_LEFTTORIGHT
     'icon (if (fx= (length icon) 1) (car icon) #f)
     'icon-space 5      ;; If an icon used beside the string, how much space to place between them
     'icon-align GUI_ALIGNLEFT    ;: If an icon is used, the side of the string on which it is placed
     'draw-handle  glgui:button-draw
     'input-handle glgui:button-input
     'update-handle glgui:button-update
  ))

;;eof
