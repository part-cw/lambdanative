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
;; keypad widget

(define delchar (integer->char EVENT_KEYBACKSPACE))
(define retchar (integer->char EVENT_KEYENTER))

(define keypad:numeric `( (
  ( #\1 #\2 #\3 )
  ( #\4 #\5 #\6 )
  ( #\7 #\8 #\9 )
  ( (,delchar ,glgui_keypad_delete.img) #\0 (,retchar ,glgui_keypad_return.img))
)))

(define keypad:numfloat `( (
  ( #\1 #\2 #\3 )
  ( #\4 #\5 #\6 )
  ( #\7 #\8 #\9 )
  ( (,delchar ,glgui_keypad_delete.img 0.5) (#\. "." 0.5) #\0 (,retchar ,glgui_keypad_return.img))
)))

(define keypad:numcolon `( (
  ( #\7 #\8 #\9 )
  ( #\4 #\5 #\6 )
  ( #\1 #\2 #\3 )
  ((,delchar ,glgui_keypad_delete.img 0.5) (#\: ":" 0.5) #\0 (,retchar ,glgui_keypad_return.img))
)))

(define keypad:numdash `( (
  ( #\7 #\8 #\9 )
  ( #\4 #\5 #\6 )
  ( #\1 #\2 #\3 )
  ((,delchar ,glgui_keypad_delete.img 0.5) (#\- "-" 0.5) #\0 (,retchar ,glgui_keypad_return.img))
)))

(define keypad:calculator `((
  ( #\C  (#\X "MC")  (#\Y "MR") (#\Z "M+") )
  ( (,delchar ,glgui_keypad_delete.img) (#\S "+/-") #\% #\/ )
  ( #\7 #\8 #\9 (#\* "X"))
  ( #\4 #\5 #\6 #\-)
  ( #\1 #\2 #\3 #\+)
  ( (#\0 "0" 2.)  #\. (#\= "=" 1. ,DarkOrange))
)))

(define keypad:default `(
  ( (#\q #\w #\e #\r #\t #\y #\u #\i #\o #\p)
    (#\a #\s #\d #\f #\g #\h #\j #\k #\l)
 ((shift ,glgui_keypad_shift.img 1.5) #\z #\x #\c #\v #\b #\n #\m (,delchar ,glgui_keypad_delete.img 1.5))
   ((toggle ,glgui_keypad_toggle.img 1.5)  #\, (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
  ( (#\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P)
    (#\A #\S #\D #\F #\G #\H #\J #\K #\L)
 ((shift ,glgui_keypad_shift_on.img 1.5) #\Z #\X #\C #\V #\B #\N #\M (,delchar ,glgui_keypad_delete.img 1.5))
   ((toggle ,glgui_keypad_toggle.img 1.5)  #\, (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
  ( (#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
    (#\@ #\# #\$ #\% #\& #\( #\) #\- #\\)
 ((shift "*-+" 1.5) #\! #\; #\: #\' #\" #\? #\/ (,delchar ,glgui_keypad_delete.img 1.5))
   ((toggle ,glgui_keypad_toggleChar.img 1.5)  #\, (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
  ( (#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
    (#\^ #\[ #\] #\{ #\} #\< #\>)
 ((shift "@#$" 1.5)  #\* #\- #\+ #\= #\_ #\~ #\| (,delchar ,glgui_keypad_delete.img 1.5))
   ((toggle ,glgui_keypad_toggleChar.img 1.5)  #\, (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
))

;; A simplified keypad with only one set of punctuation characters (no shift there) which don't include many that would
;; be tricky to deal with in the code: " ' \ etc - also they fit in one texture for font sizes up to 22
(define keypad:simplified `(
  ( (#\q #\w #\e #\r #\t #\y #\u #\i #\o #\p)
    (#\a #\s #\d #\f #\g #\h #\j #\k #\l)
 ((shift ,glgui_keypad_shift.img 1.5) #\z #\x #\c #\v #\b #\n #\m (,delchar ,glgui_keypad_delete.img 1.5))
   ((toggle ,glgui_keypad_toggle.img 1.5)  #\, (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
  ( (#\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P)
    (#\A #\S #\D #\F #\G #\H #\J #\K #\L)
 ((shift ,glgui_keypad_shift_on.img 1.5) #\Z #\X #\C #\V #\B #\N #\M (,delchar ,glgui_keypad_delete.img 1.5))
   ((toggle ,glgui_keypad_toggle.img 1.5)  #\, (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
  ( (#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
    (#\@ #\# #\$ #\% #\& #\* #\- #\+ #\( #\))
    (#\! #\" #\' #\: #\; #\/ #\? (,delchar ,glgui_keypad_delete.img 1.5))
   ((toggle ,glgui_keypad_toggleChar.img 1.5)  #\, (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
))

;; An email keypad contain just those characters allowed in an email address and with the @ sign available on the first page
(define keypad:email `(
  ( (#\q #\w #\e #\r #\t #\y #\u #\i #\o #\p)
    (#\a #\s #\d #\f #\g #\h #\j #\k #\l)
 ((shift ,glgui_keypad_shift.img 1.5) #\z #\x #\c #\v #\b #\n #\m (,delchar ,glgui_keypad_delete.img 1.5))
   ((toggle ,glgui_keypad_toggle.img 1.5)  #\@ (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
  ( (#\Q #\W #\E #\R #\T #\Y #\U #\I #\O #\P)
    (#\A #\S #\D #\F #\G #\H #\J #\K #\L)
 ((shift ,glgui_keypad_shift_on.img 1.5) #\Z #\X #\C #\V #\B #\N #\M (,delchar ,glgui_keypad_delete.img 1.5))
   ((toggle ,glgui_keypad_toggle.img 1.5)  #\@ (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
  ( (#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
    (#\! #\# #\$ #\% #\^ #\& #\* #\{ #\})
    ( #\- #\_ #\= #\+ #\/ #\? #\' #\| #\~ (,delchar ,glgui_keypad_delete.img 1.0))
   ((toggle ,glgui_keypad_toggleChar.img 1.5)  #\@ (#\space " " 4.) #\. (,retchar ,glgui_keypad_return.img 1.5))
  )
))

(define (keypad:keycolor key btcolor)
  (if (and (list? key) (> (length key) 3)) (cadddr key) btcolor))

(define (keypad:keywidth key)
  (if (and (list? key) (> (length key) 2)) (caddr key) 1.))

(define (keypad:keyimage key)
  (if (and (list? key) (> (length key) 1)) (cadr key) (string key)))

(define (keypad:keycmd key)
  (if (list? key) (car key) key))

(define (keypad:rowwidth padrow)
  (let loop ((p padrow)(l 0.))
    (if (= (length p) 0) l
      (loop (cdr p)
        (+ l (keypad:keywidth (car p)))))))

(define (keypad:renderkey x y wchar h key fnt highlight floatinghighlight rounded btcolor fgcolor persistcap)
  (let* ((w (* wchar (keypad:keywidth key)))
        (keycmd (keypad:keycmd key))
        (keyimg (if (and (eq? keycmd 'shift) persistcap) glgui_keypad_shift_lock.img (keypad:keyimage key)))
        (highlighted (and highlight (char? keycmd) (char=? keycmd highlight))))
    ((if rounded glgui:draw-rounded-box glgui:draw-box) (+ x 2) (+ y 3) (- w 4) (- h 6)
        (if highlighted White (keypad:keycolor key btcolor)))
    (if (string? keyimg)
      (glgui:draw-text-center x y w h keyimg fnt (if highlighted Black fgcolor))
      (glgui:draw-pixmap-center x y w h keyimg (if highlighted Black fgcolor))
    )
    (if (and highlighted floatinghighlight) (begin
      (glgui:draw-box (+ x 1) (+ y 1 h) (- w 2) (- (* 1.5 h) 2) White)
      (if (string? keyimg)
        (glgui:draw-text-center x (+ y (* 1.5 h)) w h keyimg fnt Black)
        (glgui:draw-pixmap-center x (+ y (* 1.5 h)) w h keyimg Black)
      )
    ))
 w))

(define (keypad:render x y w h pad fnt highlight floatinghighlight rounded btcolor fgcolor persistcap)
  (let* ((ncols (flo (length (car pad))))
         (nrows (flo (length pad)))
         (wchar (flo (/ w ncols)))
         (hchar (flo (/ h nrows))))
    (let rowloop ((rowdata pad))
      (if (fx> (length rowdata) 0)
        (let* ((yy (+ y (* hchar (- (length rowdata) 1))))
               (units (keypad:rowwidth (car rowdata)))
               (padx  (/ (- w (* units wchar)) 2.)))
           (let colloop ((xx (+ x padx)) (coldata (car rowdata)))
             (if (> (length coldata) 0)
               (colloop (+ xx (keypad:renderkey xx yy wchar hchar (car coldata)
                 fnt highlight floatinghighlight rounded btcolor fgcolor persistcap))
                 (cdr coldata))))
           (rowloop (cdr rowdata)))))
  ))

(define (keypad:lookup mx my x y w h pad)
  (let* ((ncols (flo (length (car pad))))
         (nrows (flo (length pad)))
         (wchar (/ w ncols))
         (hchar (/ h nrows))
         (mn (fix (- nrows 1 (fix (/ (- my y) hchar))))))
    (if (and (> my y) (< my (+ y h))) ;; (and (> mn -1) (< mn nrows))
      (let* ((units (keypad:rowwidth (list-ref pad mn)))
             (padx  (/ (- w (* units wchar)) 2.)))
        (let loop ((xx (+ x padx))(data (list-ref pad mn)))
          (if (= (length data) 0) #f
            (let ((keyw (* wchar (keypad:keywidth (car data)))))
              (if (and (> mx xx) (< mx (+ xx keyw))) (car data)
                (loop (+ xx keyw) (cdr data)))))))
      #f)))

(define (glgui:keypad-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (keypad (glgui-widget-get-dyn g wgt 'keypad))
         (shift  (glgui-widget-get g wgt 'shift))
         (toggle (glgui-widget-get g wgt 'toggle))
         (hideonreturn (glgui-widget-get g wgt 'hideonreturn))
         (armed (glgui-widget-get g wgt 'armed))
         (persistcap (glgui-widget-get g wgt 'persistcap))
         (shiftstarttime (glgui-widget-get g wgt 'shiftstarttime))
         (npad (+ (if toggle 2 0) (if shift 1 0)))
         (key (keypad:lookup mx my x y w h (list-ref keypad npad)))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
    (cond
      ((fx= type EVENT_BUTTON1DOWN)
        (let* ((keycmd (if key (keypad:keycmd key) #f))
               (keyevent (if (and (char? keycmd) (not (char=? keycmd #\nul))) (char->integer keycmd) #f)))
          (if (equal? keycmd 'shift)
            ;; Shift button pressed
            (begin
              (if (and (not persistcap) shiftstarttime (< (- ##now shiftstarttime) 0.3) (< npad 2))
                ;; Turning on cap-locks
                (begin
                  (glgui-widget-set! g wgt 'shift #t)
                  (glgui-widget-set! g wgt 'persistcap #t))
                ;; Otherwise toggling shift
                (glgui-widget-set! g wgt 'shift (not shift)))
              ;; Turning off cap-locks
              (if persistcap (glgui-widget-set! g wgt 'persistcap #f))
              ;; First tap (for double tap to turn on cap-locks)
              (glgui-widget-set! g wgt 'shiftstarttime ##now)))
          (if (equal? keycmd 'toggle) (begin
            (glgui-widget-set! g wgt 'toggle (not toggle))
            (glgui-widget-set! g wgt 'shift #f)
          ))
          (if keyevent (begin
            (event-push EVENT_KEYPRESS keyevent 0)
            (glgui-widget-set! g wgt 'highlight keycmd)
          ))

          ;; If button down somewhere over keyboard, arm for key release
          (if inside (glgui-widget-set! g wgt 'armed #t))
        ))
      ((and (fx= type EVENT_BUTTON1UP) armed)
        (glgui-widget-set! g wgt 'armed #f)
        (glgui-widget-set! g wgt 'removehighlight #t)
        (let ((keycmd (if key (keypad:keycmd key) #f)))
          ;; Check if Caps is locked. If not and the key pressed isn't the shift key, then set shift back
          ;; (for letter keypad only)
          (if (and (not persistcap) (< npad 2) (not (equal? keycmd 'shift))) (glgui-widget-set! g wgt 'shift #f))
          (if (and hideonreturn (char=? keycmd retchar))  (begin
            (glgui-widget-set! g wgt 'hidden #t)
            (if (procedure? hideonreturn) (hideonreturn))
          ))
          (let ((keyevent (if (and (char? keycmd) (not (char=? keycmd #\nul))) (char->integer keycmd) #f)))
            (if keyevent (event-push EVENT_KEYRELEASE keyevent 0)))
          ))
      ((and (fx= type EVENT_MOTION) (glgui-widget-get g wgt 'highlight) armed)
        (let ((keycmd (if key (keypad:keycmd key) #f)))
          (if (and (char? keycmd) (not (char=? keycmd #\nul))) (glgui-widget-set! g wgt 'highlight keycmd))
        ))
      ((and (fx= type EVENT_KEYRELEASE) (fx= mx EVENT_KEYBACK) hideonreturn)
        (glgui-widget-set! g wgt 'hidden #t)
        (if (procedure? hideonreturn) (hideonreturn))))
   ;; Return the key, but if false, return whether event was from inside rectangle dimensions
   (if key key inside)
 ))

(define (glgui:keypad-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (bgcolor (glgui-widget-get g wgt 'bgcolor))
         (keypad (glgui-widget-get-dyn g wgt 'keypad))
         (fnt (glgui-widget-get-dyn g wgt 'fnt))
         (shift  (glgui-widget-get g wgt 'shift))
         (toggle (glgui-widget-get g wgt 'toggle))
         (floatinghighlight (glgui-widget-get g wgt 'floatinghighlight))
         (highlight (glgui-widget-get g wgt 'highlight))
         (rounded (glgui-widget-get g wgt 'rounded))
         (btcolor (glgui-widget-get g wgt 'btcolor))
         (fgcolor (glgui-widget-get g wgt 'fgcolor))
         (removehighlight (glgui-widget-get g wgt 'removehighlight))
         (persistcap (glgui-widget-get g wgt 'persistcap))
         (npad (+ (if toggle 2 0) (if shift 1 0))))
  ;; Can't have caplocks without shift
  (if (and persistcap (not shift)) (glgui-widget-set! g wgt 'persistcap #f))
  (if bgcolor (glgui:draw-box x y w h bgcolor))
  (keypad:render x y w h (list-ref keypad npad) fnt highlight floatinghighlight rounded btcolor fgcolor persistcap)
  ;; After drawing remove highlights if this is set
  (if removehighlight
     (begin
       (glgui-widget-set! g wgt 'highlight #f)
       (glgui-widget-set! g wgt 'removehighlight #f)))))

(define (glgui-keypad g x y w h fnt . keypad)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'fnt fnt
     'hidden #f
     'toggle #f
     'shift #f
     'highlight #f
     'floatinghighlight #t
     'removehighlight #f
     'hideonreturn #f
     'armed #f
     'shiftstarttime ##now
     'persistcap #f
     'keypad (if (fx= (length keypad) 1) (car keypad) keypad:default)
     'bgcolor (color-fade Black 0.5)
     'btcolor (color-shade White 0.5)
     'fgcolor White
     'rounded #t
     'draw-handle  glgui:keypad-draw
     'input-handle glgui:keypad-input
  ))

;; eof
