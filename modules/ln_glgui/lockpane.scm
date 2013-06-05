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

;; Lockpane with a splash screen image
(define lockpane:focuscolor (color-shade White 0.25))

(define lockpane:keypad `( (
  ( #\1 #\2 #\3 )
  ( #\4 #\5 #\6 )
  ( #\7 #\8 #\9 )
  ( (,delchar ,glgui_keypad_delete.img) #\0 #\space)
)))

(define (lockpane:callback cb)
  (lambda (g wgt t x y) 
    (let ((res (if cb (cb g wgt t x y) #f))
          (keyw (glgui-get g 'LockPane_keypadw)))
      (glgui-widget-set! g wgt 'bgcolor (if res lockpane:focuscolor (color-shade Red 0.7)))
      (glgui-widget-set! g wgt 'label #f)
      (glgui-widget-set! g keyw 'highlight #f)
    )))

(define (make-glgui-lockpane x y w h splash fnt callback)
  (let* ((gui (make-glgui x y))
         (keyh (* 5 32))
         (bg (glgui-box gui 0 0 w h (color-shade White 0.2)))
         (keypadw (glgui-keypad gui 0 0 w keyh fnt lockpane:keypad))
         (splashw (if (list? splash)
           (glgui-image gui 0  keyh w (- h keyh) splash White)
           (glgui-box gui 0  keyh w (- h keyh) Black)))
         (inputw (glgui-label gui 8 (+ keyh 8) (- w 16) 32 "" fnt White lockpane:focuscolor)))
    (glgui-set! gui 'LockPane_keypadw keypadw)
    (if (procedure? splash) (begin
      (glgui-widget-set! gui splashw 'draw-handle splash)
      (glgui-widget-set! gui splashw 'input-handle #f)
    ))
    (glgui-widget-set! gui inputw 'focus #t)
    (glgui-widget-set! gui inputw 'password 4) ;; number of characters
    (glgui-widget-set! gui inputw 'callback (lockpane:callback callback))
    (glgui-widget-set! gui inputw 'align GUI_ALIGNCENTER)
    (glgui-widget-set! gui keypadw 'floatinghighlight #f)
    (glgui-widget-set! gui keypadw 'rounded #f)
    (glgui-widget-set! gui keypadw 'bgcolor #f)
    (glgui-widget-set! gui keypadw 'btcolor (color-shade White 0.15))
  gui))

;; eof