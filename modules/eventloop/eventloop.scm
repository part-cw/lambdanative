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

(c-declare  #<<end-of-c-declare

#include "LNCONFIG.h"
#ifdef ANDROID
  void ln_android_finish();
#endif

void android_finish(){
#ifdef ANDROID
  ln_android_finish();
#endif
}

end-of-c-declare
)

;; event related constants
(define EVENT_MOTION ((c-lambda () int "___result = EVENT_MOTION;")))
(define EVENT_KEYPRESS ((c-lambda () int "___result = EVENT_KEYPRESS;")))
(define EVENT_KEYRELEASE ((c-lambda () int "___result = EVENT_KEYRELEASE;")))
(define EVENT_BUTTON1UP ((c-lambda () int "___result = EVENT_BUTTON1UP;")))
(define EVENT_BUTTON1DOWN ((c-lambda () int "___result = EVENT_BUTTON1DOWN;")))
(define EVENT_BUTTON2UP ((c-lambda () int "___result = EVENT_BUTTON2UP;")))
(define EVENT_BUTTON2DOWN ((c-lambda () int "___result = EVENT_BUTTON2DOWN;")))
(define EVENT_BUTTON3UP ((c-lambda () int "___result = EVENT_BUTTON3UP;")))
(define EVENT_BUTTON3DOWN ((c-lambda () int "___result = EVENT_BUTTON3DOWN;")))
(define EVENT_CLOSE ((c-lambda () int "___result = EVENT_CLOSE;")))
(define EVENT_REDRAW ((c-lambda () int "___result = EVENT_REDRAW;")))
(define EVENT_INIT ((c-lambda () int "___result = EVENT_INIT;")))
(define EVENT_TERMINATE ((c-lambda () int "___result = EVENT_TERMINATE;")))
(define EVENT_BATTERY ((c-lambda () int "___result = EVENT_BATTERY;")))
(define EVENT_SUSPEND ((c-lambda () int "___result = EVENT_SUSPEND;")))
(define EVENT_RESUME ((c-lambda () int "___result = EVENT_RESUME;")))
(define EVENT_ORIENTATION ((c-lambda () int "___result = EVENT_ORIENTATION;")))
(define EVENT_MULTITOUCH ((c-lambda () int "___result = EVENT_MULTITOUCH;")))
(define EVENT_DEBUG ((c-lambda () int "___result = EVENT_DEBUG;")))
(define EVENT_IDLE ((c-lambda () int "___result = EVENT_IDLE;")))
(define EVENT_NOTIFICATION ((c-lambda () int "___result = EVENT_NOTIFICATION;")))

;; keyboard related constants
(define EVENT_KEYENTER ((c-lambda () int "___result = EVENT_KEYENTER;")))
(define EVENT_KEYTAB ((c-lambda () int "___result = EVENT_KEYTAB;")))
(define EVENT_KEYBACKSPACE ((c-lambda () int "___result = EVENT_KEYBACKSPACE;")))
(define EVENT_KEYDELETE ((c-lambda () int "___result = EVENT_KEYDELETE;")))
(define EVENT_KEYRIGHT ((c-lambda () int "___result = EVENT_KEYRIGHT;")))
(define EVENT_KEYLEFT ((c-lambda () int "___result = EVENT_KEYLEFT;")))
(define EVENT_KEYUP ((c-lambda () int "___result = EVENT_KEYUP;")))
(define EVENT_KEYDOWN ((c-lambda () int "___result = EVENT_KEYDOWN;")))
(define EVENT_KEYESCAPE ((c-lambda () int "___result = EVENT_KEYESCAPE;")))
(define EVENT_KEYBACK ((c-lambda () int "___result = EVENT_KEYBACK;")))
(define EVENT_KEYMENU ((c-lambda () int "___result = EVENT_KEYMENU;")))
(define EVENT_KEYHOME ((c-lambda () int "___result = EVENT_KEYHOME;")))
(define EVENT_KEYEND ((c-lambda () int "___result = EVENT_KEYEND;")))

;; orientation
(define GUI_LANDSCAPE ((c-lambda () int "___result = GUI_LANDSCAPE;")))
(define GUI_SEASCAPE ((c-lambda () int "___result = GUI_SEASCAPE;")))
(define GUI_PORTRAIT ((c-lambda () int "___result = GUI_PORTRAIT;")))
(define GUI_UPSIDEDOWN ((c-lambda () int "___result = GUI_UPSIDEDOWN;")))

(define app:runflag 1)
(define app:width #f)
(define app:height #f)

(define app:screenwidth #f)
(define app:screenheight #f)
(define app:xscale 1.0)
(define app:yscale 1.0)
(define app:scale? #f)

(define hook:init #f)
(define hook:terminate #f)
(define hook:event #f)

(define hook:suspend #f)
(define hook:resume #f)

(define app:mustinit #t)
(define app:suspended #f)

;; Android specials
(define app:android? (string=? (system-platform) "android"))
(define android-finish (c-lambda () void "android_finish"))

(define event:fifo '())
(define (event-push t x y)
  (set! event:fifo (append event:fifo (list (list t x y)))))
(define (event-pop)
  (if (fx> (length event:fifo) 0)
     (let ((ret (car event:fifo)))
        (set! event:fifo (cdr event:fifo)) ret) #f))

(define eventloop:mutex (make-mutex))
(define (eventloop:grab!) (mutex-lock! eventloop:mutex))
(define (eventloop:release!) (mutex-unlock! eventloop:mutex))

(c-define (c-event t x y) (int int int) void "scm_event" ""
  (eventloop:grab!)
  (set! ##now (current-time-seconds))
  (let ((xtra (if (not app:mustinit) (event-pop) #f)))
    (if xtra (apply hook:event xtra))
    (cond
      ((fx= t EVENT_REDRAW)
        (hook:event t 0 0)
        (if app:android? (##thread-heartbeat!))
      )
      ((fx= t EVENT_IDLE)
        (if app:suspended (begin
          (hook:event t 0 0)
          (if app:android? (##thread-heartbeat!))
        ))
      )
      ((or (fx= t EVENT_BUTTON1DOWN) (fx= t EVENT_BUTTON1UP)
           (fx= t EVENT_BUTTON2DOWN) (fx= t EVENT_BUTTON2UP)
           (fx= t EVENT_BUTTON3DOWN) (fx= t EVENT_BUTTON3UP)
           (fx= t EVENT_MOTION))
         ;; handle potential scaling (running stretched on a device)
         (hook:event t (if app:scale? (fix (* app:xscale x)) x)
                       (if app:scale? (fix (* app:yscale y)) y))
      )
      ((fx= t EVENT_INIT)
        ;; prevent multiple inits
        (if app:mustinit (begin
          (set! app:width x)
          (set! app:height y)
          (set! app:screenwidth x)
          (set! app:screenheight y)
          (if (procedure? hook:init) (hook:init x y))
          (set! app:mustinit #f)
        ))
      )
      ((fx= t EVENT_TERMINATE)
        (log-system "System shutdown")
        (if (procedure? hook:terminate) (hook:terminate))
        (if app:android? (android-finish)))
      ((fx= t EVENT_SUSPEND)
        (if (and (not app:mustinit) (not app:suspended)) (begin
          (set! app:suspended #t)
          (if (procedure? hook:suspend) (hook:suspend))
        )))
      ((fx= t EVENT_RESUME)
        (if (and (not app:mustinit) app:suspended) (begin
          (set! app:suspended #f)
          (if (procedure? hook:resume) (hook:resume))
          )))
      (else
        (if (and (not app:mustinit) (procedure? hook:event)) (hook:event t x y)))
  ))
  (eventloop:release!))

(c-define (c-width) () int "scm_width" ""
   (if (number? app:width) app:width 0))

(c-define (c-height) () int "scm_height" ""
  (if (number? app:height) app:height 0))

(c-define (c-screenwidth) () int "scm_screenwidth" ""
   (if (number? app:screenwidth) app:screenwidth 0))

(c-define (c-screenheight) () int "scm_screenheight" ""
  (if (number? app:screenheight) app:screenheight 0))

(c-define (c-runflag) () int "scm_runflag" ""
  (if (number? app:runflag) app:runflag 0))

;; change default size (don't go full screen!)
(define (make-window w h)
  (let ((xscale (/ (flo w) (flo app:screenwidth)))
        (yscale (/ (flo h) (flo app:screenheight))))
  (set! app:width w)
  (set! app:height h)
  (if (or (string=? (system-platform) "ios")
          (string=? (system-platform) "bb10")
          (string=? (system-platform) "playbook")
          (string=? (system-platform) "android")) (begin
    (set! app:xscale xscale)
    (set! app:yscale yscale)
    (set! app:scale? #t)
  ))
))

;; assign scheme entry points
(define (ln-main p1 p2 p3 . px)
  (set! hook:init p1)
  (set! hook:event p2)
  (set! hook:terminate (lambda () (p3) (force-terminate)))
  (if (> (length px) 0) (set! hook:suspend (car px)))
  (if (> (length px) 1) (set! hook:resume (cadr px)))
)
;; override gambit main definition for backwards compatibility
(set! main ln-main)

(define (terminate)
  (if (procedure? hook:terminate) (hook:terminate))
  (if app:android? (android-finish))
)

;; eof
