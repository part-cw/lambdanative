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
(define (glgui:label-aligned-draw x y w h label fnt color align . showstart?)
  (cond 
    ((= align GUI_ALIGNLEFT) (glgui:draw-text-left x y w h label fnt color))
	((= align GUI_ALIGNRIGHT) (glgui:draw-text-right x y w h label fnt color))
	(else (glgui:draw-text-center x y w h label fnt color (if (fx= (length showstart?) 1) (car showstart?) #f)))))

(define (glgui:label-draw g wgt)
  (let* ((fnt (glgui-widget-get g wgt 'font))
         (x0 (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w0 (glgui-widget-get-dyn g wgt 'w))
         (h0 (glgui-widget-get-dyn g wgt 'h))
         (h (if (= h0 0) (glgui:fontheight fnt) h0))
         (r (glgui-widget-get g wgt 'rounded))
         (clearoninput (glgui-widget-get g wgt 'clearoninput))
         (color (glgui-widget-get-dyn g wgt 'color))
         (bgcolor (glgui-widget-get-dyn g wgt 'bgcolor))
         (password (glgui-widget-get g wgt 'password))
         (focus (glgui-widget-get g wgt 'focus))
         (blinkfactor (modulo (fix ##now) 2))
         (label0 (glgui-widget-get g wgt 'label))
         (label1 (if label0 (if password (make-string (string-length label0) #\*) label0) #f))
         (label (if (and label1 focus (fx= blinkfactor 0)) (string-append label1 "|") label1))
         (showstart (glgui-widget-get g wgt 'showstart))
         (align0 (glgui-widget-get g wgt 'align))
         ;; If being edited, make sure right end of string if visible if show start is false
         (align (if (and focus (= align0 GUI_ALIGNLEFT) (not showstart) label (> (glgui:stringwidth (string-append label1 "|") fnt) w0)) GUI_ALIGNRIGHT align0))
         ;; Shift x if being edited and cursor is not currently visible, and the text would otherwise move back and forth as cursor blinks
         (x (if (and label focus (fx= blinkfactor 1) (<= (glgui:stringwidth (string-append label1 "|") fnt) w0))
              (cond
                ((= align GUI_ALIGNLEFT) x0)
                ((= align GUI_ALIGNCENTER)
                    (- x0 (/ (- (glgui:stringwidth (string-append label1 "|") fnt) (glgui:stringwidth label1 fnt)) 2.)))
                (else
                    (- x0 (- (glgui:stringwidth (string-append label1 "|") fnt) (glgui:stringwidth label1 fnt)))))
              x0))
         ;; Shrink w if being edited and not all of the label can fit, so visible characters would otherwise change when cursor flashing
         (w (if (and label1 focus (fx= blinkfactor 1) (> (glgui:stringwidth (string-append label1 "|") fnt) w0))
              (if (= align GUI_ALIGNLEFT)
                w0
                ;; If not aligned left, then we can see the end of the string, so shrink width when cursor not visible
                (- w0 (- (glgui:stringwidth (string-append label1 "|") fnt) (glgui:stringwidth label1 fnt))))
              w0)))
    (if bgcolor (if r (glgui:draw-rounded-box x0 y w0 h bgcolor) (glgui:draw-box x0 y w0 h bgcolor)))
    (if (and label clearoninput)
      (let* ((sw (min w (glgui:stringwidth (string-append label0 "|") fnt)))
             (xp (if (= align GUI_ALIGNLEFT) 0
                  (if (= align GUI_ALIGNRIGHT) (- w sw) (/ (- w sw) 2.)))))
         (glgui:draw-box xp y sw h White)))
    (if label (glgui:label-aligned-draw (if r (+ x 5) x) y (if r (- w 10) w) h label fnt (if clearoninput Black color) align showstart))
  ))

(define (glgui:label-wrapped-draw g wgt)
  (let* ((fnt (glgui-widget-get g wgt 'font))
         (x (flo (glgui-widget-get-dyn g wgt 'x)))
         (y (flo (glgui-widget-get-dyn g wgt 'y)))
         (w (flo (glgui-widget-get-dyn g wgt 'w)))
         (h0 (flo (glgui-widget-get-dyn g wgt 'h)))
         (label0 (glgui-widget-get g wgt 'label))
         (password (glgui-widget-get g wgt 'password))
         (label (if label0 (if password (make-string (string-length label0) #\*) label0) #f))
         (lblh (if label (flo (glgui:fontheight fnt)) 0.))
         (h (if (fl= h0 0.) (fl+ y lblh) h0))
         (r (glgui-widget-get g wgt 'rounded))
         (color (glgui-widget-get-dyn g wgt 'color))
         (bgcolor (glgui-widget-get-dyn g wgt 'bgcolor))
         (align (glgui-widget-get g wgt 'align))
         (direction (glgui-widget-get g wgt 'direction)))
    (if bgcolor (if r (glgui:draw-rounded-box x y w h bgcolor) (glgui:draw-box x y w h bgcolor)))
    (if (and label (fx> (string-length label) 0))
      (let ((labelsplit ((if (= direction GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) label w fnt))
            (hline lblh))
        (let loop ((i 0) (yline (fl+ y h (fl- hline))))
          (if (fx= i (length labelsplit)) 
            (if (fl= h0 0.) (glgui-widget-set! g wgt 'h (fl+ y h (fl- yline))) #t)
            (begin 
              (glgui:label-aligned-draw (if r (fl+ x 5.) x) yline 
                (if r (fl- w 10.) w) hline (list-ref labelsplit i) fnt color align)
              (loop (fx+ i 1) (fl- yline hline))
            )
        ))
    ))
))

(define (glgui:label-input g wgt type mx my)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (h (fix (glgui-widget-get-dyn g wgt 'h)))
         (clickable (glgui-widget-get g wgt 'enableinput))
         (focus (glgui-widget-get g wgt 'focus))
         (cb (glgui-widget-get g wgt 'callback))
         (clearoninput (glgui-widget-get g wgt 'clearoninput))
         (aftercharcb (glgui-widget-get g wgt 'aftercharcb))
         (password (glgui-widget-get g wgt 'password))
         (inside (and (fx> mx x) (fx< mx (fx+ x w)) (fx> my y) (fx< my (fx+ y h)))))
    (if (and clickable inside (fx= type EVENT_BUTTON1UP))
      (begin
        (glgui-widget-setglobal! g 'focus #f)
        (glgui-widget-set! g wgt 'focus #t)
      ))
    (if (and focus (fx= type EVENT_KEYRELEASE))
       (let* ((tmp (glgui-widget-get g wgt 'label))
              (label (if tmp (if clearoninput "" tmp) ""))
              (len (string-length label)))
         (cond
	   ((and (fx= mx EVENT_KEYENTER) cb)
              (cb g wgt type mx my) 
	      (if (not clickable) (set! label "")))
           ((fx= mx EVENT_KEYBACKSPACE)
              (if (> (string-length label) 0)
                (set! label (substring label 0 (- (string-length label) 1)))))
           ((fx> mx 31)
              (set! label (string-append label (string (integer->char mx))))))
         (glgui-widget-set! g wgt 'clearoninput #f)
         (glgui-widget-set! g wgt 'label label)
         ;; If length of string changed, do after char callback
         (if (and aftercharcb (not (fx= (string-length label) len))) (aftercharcb g wgt type mx my))
         (if (and cb (number? password) (= (string-length label) password)) (begin
           (cb g wgt type mx my) (glgui-widget-set! g wgt 'label "")))
      ))
  (if clickable inside #f)
 ))

(define (glgui-label g x y w h label fnt color . bgcolor)
  (glgui-widget-add g
     'x x
     'y y
     'w w 
     'h h
     'rounded #f
     'font fnt
     'label label
     'callback #f
     'aftercharcb #f    ;; A callback that is called after every character and after delete
     'color color
     'hidden #f
     'direction GUI_LEFTTORIGHT
     'password #f
     'enableinput #f   ;; This way we can make clickable labels
     'focus #f
     'clearoninput #f
     'showstart #f   ;; If true than centered text that is longer than the label width will show the start instead of end and left aligned text will too if focus = true
     'bgcolor (if (fx= (length bgcolor) 1) (car bgcolor) #f)
     'align GUI_ALIGNLEFT
     'draw-handle  glgui:label-draw
     'input-handle glgui:label-input
  ))

(define (glgui-inputlabel g x y w h label fnt color . bgcolor)
  (let ((wgt (glgui-label g x y w h label fnt color (if (fx= (length bgcolor) 1) (car bgcolor) #f))))
    (glgui-widget-set! g wgt 'enableinput #t)
    wgt
  ))

(define (glgui-label-wrapped g x y w h label fnt color . bgcolor)
  (let ((wgt (glgui-label g x y w h label fnt color (if (fx= (length bgcolor) 1) (car bgcolor) #f))))
    (glgui-widget-set! g wgt 'draw-handle glgui:label-wrapped-draw)
    wgt
  ))
;; eof
