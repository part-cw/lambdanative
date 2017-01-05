#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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

;; VOIP demo
(define gui #f)
(define ipstring "")
(define iplabel #f)
(define losslabel #f)

(define keypad:voipme `((
  (#\1 #\2 #\3)
  (#\4 #\5 #\6)
  (#\7 #\8 #\9)
  ((,delchar ,glgui_keypad_delete.img) #\. #\0)
  ((#\H "HANGUP" 1.5 ,Red) (#\C "PICKUP" 1.5 ,Green))
 )))

(define keypadw #f)

(define (process-events t x y)
  (if (= t EVENT_KEYPRESS)
    (cond
      ((= x EVENT_KEYESCAPE) (terminate))
      ((and (phone-onhook?) (= x EVENT_KEYBACKSPACE))
         (let ((l ipstring))
           (if (> (string-length l) 0)
             (set! ipstring (substring l 0 (- (string-length l) 1))))))
      ((or (= x EVENT_KEYENTER) (= x 67)) ;; dial
         (if (phone-ringing?)
           (phone-pickup)
           (phone-dial ipstring)
         ))
      ((= x 72) ;; hangup
         (phone-hangup))
      (else
        (if (and (phone-onhook?) (fx< (string-length ipstring) 15))  (begin
        (let ((c (list->string (list (integer->char x)))))
          (set! ipstring (string-append ipstring c))
          (with-output-to-file (string-append (system-directory) (system-pathseparator) "remoteip")
            (lambda () (write ipstring)))
        ))))
  )))

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (let ((w (glgui-width-get))
          (h (glgui-height-get)))
      (set! gui (make-glgui))
      (glgui-menubar gui 0 (- (glgui-height-get) 44) (glgui-width-get) 44)
      (glgui-image gui 0 (- (glgui-height-get) 36) w 36 title.img White)
      (set! keypadw (glgui-keypad gui 0 0 320 320 ascii_32.fnt keypad:voipme))
      (let ((wgt (glgui-label gui 0 (- h 36 20 10) w 16 (ipaddr->string (host-ipaddr)) ascii_16.fnt Gray)))
        (glgui-widget-set! gui wgt 'align GUI_ALIGNCENTER))
      (set! iplabel (glgui-label gui 0 w w (- h w 36 20) "" ascii_32.fnt White))
      (set! losslabel (glgui-label gui 0 (+ w 10) w 16 "" ascii_16.fnt Gray))
      (glgui-widget-set! gui losslabel 'align GUI_ALIGNCENTER)
      (glgui-widget-set! gui iplabel 'align GUI_ALIGNCENTER)
      (glgui-widget-set! gui keypadw 'floatinghighlight #f)
   )
  (let ((logdir (string-append (system-directory) (system-pathseparator) "log")))
    (if (not (file-exists? logdir)) (create-directory logdir)))
  (let ((tmpfile (string-append (system-directory) (system-pathseparator) "remoteip")))
    (if (file-exists? tmpfile) (begin
      (set! ipstring (with-input-from-file tmpfile (lambda () (read))))
      (glgui-widget-set! gui iplabel 'label ipstring))))
  (phone-init)
  )
;; events
  (lambda (t x y)
    (process-events t x y)
    (glgui-widget-set! gui iplabel 'label
      (cond ((phone-ringing?) "*RINGING*")
            ((phone-connected?) "CONNECTED")
            ((phone-dialing?) "DIALING..")
            (else ipstring)))
    (let ((br (opusvoip-getrecvbytes))
          (bs (opusvoip-getsendbytes)))
      (glgui-widget-set! gui losslabel 'label (string-append
         "REVC: " (number->string br) "  SEND: " (number->string bs))))
    (if (fx= t EVENT_REDRAW) (thread-sleep! 0.05))
    (glgui-event gui t x y))
;; termination
  (lambda () (phone-close)
     #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)
;;eof
