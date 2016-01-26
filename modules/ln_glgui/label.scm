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
         (x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
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
         (label (if label0 (if password (make-string (string-length label0) #\*) label0) #f))
         (labelw (if label0 (glgui:stringwidth label fnt) 0))
         (showstart (glgui-widget-get g wgt 'showstart))
         (align0 (glgui-widget-get g wgt 'align))
         ;; If being edited, make sure right end of string is visible if show start is false
         (align (if (and focus (not (= align0 GUI_ALIGNRIGHT)) (not showstart) label (> labelw w)) GUI_ALIGNRIGHT align0)))
    (if bgcolor (if r (glgui:draw-rounded-box x y w h bgcolor) (glgui:draw-box x y w h bgcolor)))
    (if (and label clearoninput)
      (let* ((sw (min w labelw))
             (xp (if (= align GUI_ALIGNLEFT) 0
                  (if (= align GUI_ALIGNRIGHT) (- w sw) (/ (- w sw) 2.)))))
         (glgui:draw-box xp y sw h White)
      ))
    (if label (glgui:label-aligned-draw (if r (+ x 5) x) y (if r (- w 10) w)
      h label fnt (if clearoninput Black color) align showstart))
    (if (and focus (fx= blinkfactor 0))
      (let* ((label-len (if label0 (glgui:stringwidth-lst label fnt) (list 0)))
             (curserpos (glgui-widget-get g wgt 'focuspos))
             (curserw (apply + (sublist label-len 0 curserpos)))
             (blinkpos (cond
               ((= align GUI_ALIGNLEFT) (+ x curserw))
               ((= align GUI_ALIGNCENTER) (- (+ x (/ w 2) curserw) (/ labelw 2)))
               (else (- (+ x w curserw) labelw))
               ))
             (blinkh (glgui:fontheight fnt))
             (blinky (+ y (/ (- h blinkh) 2.))))
        (glgui:draw-box blinkpos blinky 1 blinkh color)
      )
    )
  ))

(define (glgui:label-wrapped-draw g wgt)
  (let* ((fnt (glgui-widget-get g wgt 'font))
         (x (flo (glgui-widget-get-dyn g wgt 'x)))
         (y (flo (glgui-widget-get-dyn g wgt 'y)))
         (w (flo (glgui-widget-get-dyn g wgt 'w)))
         (h0 (flo (glgui-widget-get-dyn g wgt 'h)))
         (label0 (glgui-widget-get g wgt 'label))
         (password (glgui-widget-get g wgt 'password))
         (focus (glgui-widget-get g wgt 'focus))
         (blinkfactor (modulo (fix ##now) 2))
         (label (if label0 (if password (make-string (string-length label0) #\*) label0) #f))
         (labelw (if label0 (glgui:stringwidth label fnt) 0))
         (lblh (if label (flo (glgui:fontheight fnt)) 0.))
         (h (if (fl= h0 0.) (fl+ y lblh) h0))
         (r (glgui-widget-get g wgt 'rounded))
         (color (glgui-widget-get-dyn g wgt 'color))
         (bgcolor (glgui-widget-get-dyn g wgt 'bgcolor))
         (showstart (glgui-widget-get g wgt 'showstart))
         (align (glgui-widget-get g wgt 'align))
         (direction (glgui-widget-get g wgt 'direction)))
    (if bgcolor (if r (glgui:draw-rounded-box x y w h bgcolor) (glgui:draw-box x y w h bgcolor)))
    (if (and label (fx> (string-length label) 0))
      (let* ((labelsplit0 ((if (= direction GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) label w fnt))
             (hline lblh)
             (linecount (fix (/ h lblh)))
             (focuspos (glgui-widget-get g wgt 'focuspos))
             (labelsplit (if (fx> (length labelsplit0) linecount)
                           (if focus
                             (let ((index (- (length labelsplit0) linecount)))
                               ;; Remove hidden characters from focuspos count
                               (set! focuspos (- focuspos (apply + (map string-length (list-head labelsplit0 index)))))
                               (list-tail labelsplit0 index))
                             (list-head labelsplit0 linecount))
                           labelsplit0))
             (labelsplit-len (length labelsplit)))
        (let loop ((i 0) (yline (fl+ y h (fl- hline))) (charct 0))
          (if (fx= i labelsplit-len)
            (if (fl= h0 0.) (glgui-widget-set! g wgt 'h (fl+ y h (fl- yline))) #t)
            (let* ((linelabel (list-ref labelsplit i))
                   (linelabel-len (string-length linelabel)))
              (if (and focus (fx= blinkfactor 0) (fx>= focuspos charct)
                       ((if (fx= i (- labelsplit-len 1)) fx<= fx<) focuspos (fx+ charct linelabel-len)))
                (let* ((label-len (glgui:stringwidth-lst linelabel fnt))
                       (curserpos (fx- focuspos charct))
                       (curserw (apply + (sublist label-len 0 curserpos)))
                       (blinkpos (cond
                         ((= align GUI_ALIGNLEFT) (+ x curserw))
                         ((= align GUI_ALIGNCENTER) (- (+ x (/ w 2) curserw) (/ (apply + label-len) 2)))
                         (else (- (+ x w curserw) labelw))
                       )))
                  (glgui:draw-box blinkpos yline 1 hline color)
                )
              )
              (glgui:label-aligned-draw (if r (fl+ x 5.) x) yline
                (if r (fl- w 10.) w) hline (list-ref labelsplit i) fnt color align)
              (loop (fx+ i 1) (fl- yline hline) (fx+ charct linelabel-len))
            )
        ))
    )
    ;; Draw cursor if in focus, even if there is no text
    (if (and focus (fx= blinkfactor 0))
      (let ((blinkpos (cond
                        ((= align GUI_ALIGNLEFT) x)
                        ((= align GUI_ALIGNCENTER) (+ x (/ w 2)))
                        (else (- (+ x w) 1))))
            (lineh (flo (glgui:fontheight fnt))))
        (glgui:draw-box blinkpos (fl+ y h (fl- lineh)) 1 lineh color))))
))

(define (glgui:label-input g wgt type mx0 my0)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (h (fix (glgui-widget-get-dyn g wgt 'h)))
         (mx (fix mx0))
         (my (fix my0))
         (wrapped? (glgui-widget-get g wgt 'wrapped?))
         (clickable (glgui-widget-get g wgt 'enableinput))
         (focus (glgui-widget-get g wgt 'focus))
         (focuspos (glgui-widget-get g wgt 'focuspos))
         (cb (glgui-widget-get g wgt 'callback))
         (clearoninput (glgui-widget-get g wgt 'clearoninput))
         (aftercharcb (glgui-widget-get g wgt 'aftercharcb))
         (onfocuscb (glgui-widget-get g wgt 'onfocuscb))
         (password (glgui-widget-get g wgt 'password))
         (fnt (glgui-widget-get g wgt 'font))
         (label0 (glgui-widget-get g wgt 'label))
         (label (if label0 (if password (make-string (string-length label0) #\*) label0) #f))
         (label-len (if label0 (glgui:stringwidth-lst label fnt) (list 0)))
         (labelw (if label0 (glgui:stringwidth label fnt) 0))
         (labelh (if label0 (glgui:fontheight fnt) 0))
         (align0 (glgui-widget-get g wgt 'align))
         (align (if (and focus (not (= align0 GUI_ALIGNRIGHT)) label (> labelw w)) GUI_ALIGNRIGHT align0))
         (inside (and (fx> mx x) (fx< mx (fx+ x w 5)) (fx> my y) (fx< my (fx+ y h)))))
    (if (and clickable inside (fx= type EVENT_BUTTON1UP))
      (begin
        (glgui-widget-setglobal! g 'focus #f)
        (glgui-widget-set! g wgt 'focus #t)
        (if (and onfocuscb (not focus)) (onfocuscb g wgt type mx my))
      ))
    (if (and label inside focus (fx= type EVENT_BUTTON1DOWN))
      (if wrapped?
        (let* ((direction (glgui-widget-get g wgt 'direction))
               (ls0 ((if (= direction GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) label w fnt))
               (ls (if (fx= (length ls0) 0) '("") ls0))
               (maxrows (fix (/ h labelh)))
               (hiddenrows (max (- (length ls) maxrows) 0))
               (lastrow (- (length ls) 1))
               (row0 (max 0 (+ (floor (/ (- (+ y h) my) labelh)) hiddenrows)))
               ;; If past last row, put cursor on last row at last character
               (row (if (> row0 lastrow)
                      (begin
                        (set! mx (fix (if (= direction GUI_RIGHTTOLEFT) x (+ x w))))
                        lastrow)
                      row0))
               (lrow (list-ref ls row))
               (lroww (glgui:stringwidth lrow fnt))
               (lrow-len (glgui:stringwidth-lst lrow fnt))
               (prevchars (apply + (map string-length (sublist ls 0 row)))))
          (let ((xpos
            (cond
              ((= align0 GUI_ALIGNLEFT) (- mx x 5))
              ((= align0 GUI_ALIGNCENTER) (+ (- mx x 5 (/ w 2)) (/ lroww 2)))
              (else (+ (- mx x w 5) lroww))
            )))
            (let loop ((i 0))
              (if (or (fx= i (string-length lrow))
                      (< xpos (apply + (sublist lrow-len 0 i))))
                (glgui-widget-set! g wgt 'focuspos (+ prevchars i))
                (loop (fx+ i 1))
              )
            ))
        )
        (let ((xpos
          (cond
            ((= align GUI_ALIGNLEFT) (- mx x 5))
            ((= align GUI_ALIGNCENTER) (+ (- mx x 5 (/ w 2)) (/ labelw 2)))
            (else (+ (- mx x w 5) labelw))
          )))
          (let loop ((i 0))
            (if (or (fx= i (string-length label))
                    (< xpos (apply + (sublist label-len 0 i))))
              (glgui-widget-set! g wgt 'focuspos i)
              (loop (fx+ i 1))
            )
         ))
      ))
    (if (and focus (fx= type EVENT_KEYRELEASE))
      (let* ((tmp (glgui-widget-get g wgt 'label))
              (label (if tmp (if clearoninput "" tmp) ""))
              (len (string-length label)))
         (cond
           ((fx= mx EVENT_KEYLEFT)
             (glgui-widget-set! g wgt 'focuspos (max 0 (fx- focuspos 1)))
           )
           ((fx= mx EVENT_KEYRIGHT)
             (glgui-widget-set! g wgt 'focuspos (min (string-length label) (fx+ focuspos 1)))
           )
           ((fx= mx EVENT_KEYHOME)
            (if wrapped?
              (let* ((direction (glgui-widget-get g wgt 'direction))
                     (ls ((if (= direction GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) label w fnt))
                     (ls-len (length ls)))
                (let loop ((row 0) (prevchars 0) (prevcharsold 0))
                  (if (or (< focuspos prevchars) (fx= row ls-len))
                    (glgui-widget-set! g wgt 'focuspos prevcharsold)
                    (loop (fx+ row 1) (apply + (map string-length (sublist ls 0 (fx+ row 1)))) prevchars)
                  )
                )
              )
              (glgui-widget-set! g wgt 'focuspos 0)
            )
            (glgui-widget-set! g wgt 'focusset #t)
           )
           ((fx= mx EVENT_KEYEND)
            (if wrapped?
              (let* ((direction (glgui-widget-get g wgt 'direction))
                     (ls ((if (= direction GUI_RIGHTTOLEFT) string-split-width-rtl string-split-width) label w fnt))
                     (ls-len (length ls)))
                (let loop ((row 0) (prevchars 0))
                  (if (or (< focuspos prevchars) (fx= row ls-len))
                    (glgui-widget-set! g wgt 'focuspos (if (fx= row ls-len) prevchars (fx- prevchars 1)))
                    (loop (fx+ row 1) (apply + (map string-length (sublist ls 0 (fx+ row 1)))))
                  )
                )
              )
              (glgui-widget-set! g wgt 'focuspos (string-length label))
            )
            (glgui-widget-set! g wgt 'focusset #t)
           )
           ((and (fx= mx EVENT_KEYENTER) cb)
             (cb g wgt type mx my)
	           (if (not clickable) (set! label ""))
           )
           ((fx= mx EVENT_KEYBACKSPACE)
              (if (and (> (string-length label) 0) (> focuspos 0)) (begin
                (set! label (string-append (substring label 0 (fx- focuspos 1)) (substring label focuspos len)))
                (glgui-widget-set! g wgt 'focuspos (max 0 (fx- focuspos 1)))
              ))
             (glgui-widget-set! g wgt 'focusset #t)
           )
           ((fx= mx EVENT_KEYDELETE)
              (if (and (> (string-length label) 0) (< focuspos (string-length label))) (begin
                (set! label (string-append (substring label 0 focuspos) (substring label (fx+ focuspos 1) len)))
                (glgui-widget-set! g wgt 'focusset #t)
              ))
           )
           ((fx> mx 31)
              (set! label (string-append (substring label 0 focuspos)
                (string (integer->char mx)) (substring label focuspos len)))
              (glgui-widget-set! g wgt 'focuspos (max 0 (fx+ focuspos 1)))
              (glgui-widget-set! g wgt 'focusset #t)
           )
         )
         (glgui-widget-set! g wgt 'clearoninput #f)
         (if (not (or (fx= mx EVENT_KEYLEFT) (fx= mx EVENT_KEYRIGHT)))
           (glgui-widget-set! g wgt 'label label)
         )
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
     'onfocuscb #f    ;; A callback that is called when the label is clicked and gets focus
     'color color
     'hidden #f
     'direction GUI_LEFTTORIGHT
     'password #f
     'enableinput #f   ;; This way we can make clickable labels
     'focus #f
     'focuspos (if label (string-length label) 0)
     'clearoninput #f
     'showstart #f   ;; If true than centered text that is longer than the label width will show the start instead of end and left aligned text will too if focus = true
     'bgcolor (if (fx= (length bgcolor) 1) (car bgcolor) #f)
     'align GUI_ALIGNLEFT
     'draw-handle  glgui:label-draw
     'input-handle glgui:label-input
     'update-handle glgui:label-update
  ))

(define (glgui:label-update g w id val)
  (if (eq? id 'label) (begin
    (if (not (glgui-widget-get g w 'focusset))
      (let ((str (glgui-widget-get g w 'label)))
        (glgui-widget-set! g w 'focuspos (if (string? str) (string-length str) 0)))
    )
    (glgui-widget-set! g w 'focusset #f)
  )))

(define (glgui-inputlabel g x y w h label fnt color . bgcolor)
  (let ((wgt (glgui-label g x y w h label fnt color (if (fx= (length bgcolor) 1) (car bgcolor) #f))))
    (glgui-widget-set! g wgt 'enableinput #t)
    wgt
  ))

(define (glgui-label-wrapped g x y w h label fnt color . bgcolor)
  (let ((wgt (glgui-label g x y w h label fnt color (if (fx= (length bgcolor) 1) (car bgcolor) #f))))
    (glgui-widget-set! g wgt 'wrapped? #t)
    (glgui-widget-set! g wgt 'draw-handle glgui:label-wrapped-draw)
    wgt
  ))
;; eof
