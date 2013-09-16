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

;; device orientation example

(define vgui #f)
(define hgui #f)

(define vfish #f)
(define hfish #f)

(define (fish-animate)
  (let ((now (time->seconds (current-time)))
        (a (glgui-widget-get vgui vfish 'angle)))
    (glgui-widget-set! vgui vfish 'angle (fl* 10. (flcos (fl* 5. now))))
    (glgui-widget-set! hgui hfish 'angle (fl* 10. (flcos (fl* 5. now))))
  ))

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (set! vgui (make-glgui))
    (set! hgui (make-glgui))
    (let ((w 320)(h 480))
      (glgui-box vgui 0 0 w (/ h 2) Blue)
      (set! vfish (glgui-sprite vgui 'x (- (/ w 2) 32) 'y (/ h 4) 'image fish-icon.img 'color White))
      (glgui-box hgui 0 0 h (/ w 2) Blue)
      (set! hfish (glgui-sprite hgui 'x (- (/ h 2) 32) 'y (/ w 4) 'image fish-icon.img 'color Yellow))
    )
  )
;; events
  (lambda (t x y) 
    (orientation-event t x y)
    (if (= t EVENT_KEYPRESS) (begin (if (= x EVENT_KEYESCAPE) (terminate))))
    (fish-animate)
    (glgui-event (if (orientation-landscape?) hgui vgui) t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
