#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2013, University of British Columbia
All rights reserved.

istribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* istributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* istributions in binary form must reproduce the above
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
;; unicode example using the AnnapurnaSIL devanagari font

(main
;; initialization
  (lambda (w0 h0)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let* ((w 320)(h 480)(dh (/ (- h 88) 5)))
      (glgui-menubar gui 0 (- h 44) w 44)
      (glgui-menubar gui 0 0 w 44)
      (glgui-image  gui 0 (- h 32) w 24 title.img White)
      (glgui-image  gui 0 (+ 44 (* 1 dh)) w 32 deva_string.img White)
      (glgui-image  gui 0 (+ 44 (* 2 dh)) w 32 ascii_string.img White)
      (let ((wgt (glgui-label gui 0 (+ 44 (* 3 dh)) w 32 "एक हफ्ते में सात दिन" deva_18.fnt White )))
        (glgui-widget-set! gui wgt 'align GUI_ALIGNCENTER))
      (let ((wgt (glgui-label gui 0 (+ 44 (* 4 dh)) w 32 "LambaNative Unicode font:" ascii_18.fnt White )))
        (glgui-widget-set! gui wgt 'align GUI_ALIGNCENTER))
    )
  )
;; events
  (lambda (t x y) 
    (if (= t EVENT_KEYPRESS) (begin 
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
