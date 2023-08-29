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
;; helloworld example

(define gui #f)

(define scan-results '())
(define NUM-RESULTS 10)

(define (result-init gui x y w h)
  (let ((w (glgui-label gui x y w h "Hi" ascii_18.fnt White)))
    (glgui-widget-set! gui w 'align GUI_ALIGNCENTER)
    (set! scan-results (append scan-results (list w)))))

(define (results-init count gui x y w h)
  (if (fx> count 0) (begin
    (result-init gui x y w h)
    (results-init (- count 1) gui x (- y 20) w h))))

(define (pad-strlist lst len)
  (if (fx>= (length lst) len)
    lst
    (pad-strlist (append lst (list "")) len)))

(define (results-btn-cb g w t x y)
  (let ((results (btle-scanresults)))
    (log-system results)
    (for-each
      (lambda (w res) (glgui-widget-set! gui w 'label res))
      scan-results (pad-strlist results NUM-RESULTS))))

(define (scan-btn-cb g w t x y)
  (btle-startscan))

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (glgui-menubar gui 0 (- (glgui-height-get) 44) (glgui-width-get) 44)
    (glgui-menubar gui 0 0 (glgui-width-get) 44)
    (glgui-pixmap  gui 8 (- (glgui-height-get) 32) title.img)
    (let* ((bw 150) (bh 50)
        (bx (/ (- (glgui-width-get) bw) 2.))
        (by (- (glgui-height-get) 125))
        (tw 200) (th 20)
        (tx (/ (- (glgui-width-get) tw) 2.))
        (ty (- by 110)))
      (glgui-button-string gui bx by bw bh "Start Scan" ascii_18.fnt scan-btn-cb)
      (glgui-button-string gui bx (- by 50) bw bh "Show Results" ascii_18.fnt results-btn-cb)
      (results-init NUM-RESULTS gui tx ty tw th))
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
