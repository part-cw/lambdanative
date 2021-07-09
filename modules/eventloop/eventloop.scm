#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2020, University of British Columbia
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

(declare
 (block)
 (not interrupts-enabled))

(c-declare  #<<end-of-c-declare

#include "LNCONFIG.h"
#ifdef ANDROID
  void ln_android_finish();
  void ln_android_run_mediascanner();
#endif

void android_finish(){
#ifdef ANDROID
  ln_android_finish();
#endif
}

static void android_run_mediascanner(){
#ifdef ANDROID
  ln_android_run_mediascanner();
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
(define EVENT_JSCM_RESULT ((c-lambda () int "___result = EVENT_JSCM_RESULT;")))
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

;; keyboard modifier key constants
(define MODIFIER_CTRL_MAC ((c-lambda () int "___result = MODIFIER_CTRL_MAC;")))
(define MODIFIER_CTRL     ((c-lambda () int "___result = MODIFIER_CTRL;")))
(define MODIFIER_ALT      ((c-lambda () int "___result = MODIFIER_ALT;")))
(define MODIFIER_SHIFT    ((c-lambda () int "___result = MODIFIER_SHIFT;")))
(define MODIFIER_CAPS     ((c-lambda () int "___result = MODIFIER_CAPS;")))
(define MODIFIER_FN       ((c-lambda () int "___result = MODIFIER_FN;")))

;; orientation
(define GUI_LANDSCAPE ((c-lambda () int "___result = GUI_LANDSCAPE;")))
(define GUI_SEASCAPE ((c-lambda () int "___result = GUI_SEASCAPE;")))
(define GUI_PORTRAIT ((c-lambda () int "___result = GUI_PORTRAIT;")))
(define GUI_UPSIDEDOWN ((c-lambda () int "___result = GUI_UPSIDEDOWN;")))

(define app:runflag 1)
(define app:width #f)
(define app:height #f)
(define app:forcefullscreen #f)

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
(define android-mediascanner-done? #f)
(define (android-run-mediascanner)
  (set! android-mediascanner-done? #f)
  ((c-lambda () void "android_run_mediascanner")))

(define event:fifo '())
(define (event-push t x y)
  (set! event:fifo (append event:fifo (list (list t x y)))))
(define (event-pop)
  (if (null? event:fifo)
    #f
    (let ((ret (car event:fifo)))
      (set! event:fifo (cdr event:fifo))
      ret)))

(define on-jscm-result
  (let ((mux (make-mutex 'on-jscm-result)))
    (mutex-specific-set! mux #f)
    (lambda args
      (cond
       ((null? args) ;; return receiver procedure
        (lambda (t x y)
          (let ((proc (mutex-specific mux)))
            (when proc
              (mutex-specific-set! mux #f)
              (proc t x y)
              (mutex-unlock! mux)))))
       ((let ((proc (car args))) (and (procedure? proc) proc)) =>
        ;; set `proc` as inner receiver
        (lambda (proc)
          (mutex-lock! mux)
          (mutex-specific-set! mux proc)
          #t))
       (else (log-error "illegal arguments" on-jscm-result args))))))

(define eventloop-open-channel)
(define eventloop-close-channel)
(define eventloop-await-channel)
(define eventloop-notify-channel!
  (let ((channels (make-table test: eqv?))
        (mux (make-mutex 'eventloop-open-channel)))
    (define (close channel)
      (mutex-lock! mux)
      (let ((x (cond
                ((mutex? channel) (mutex-specific channel))
                (else channel))))
        (table-set! channels x))
      (mutex-unlock! mux))
    (define (open #!optional (receiver #f) #!key
                  (kind (cond
                         ((not receiver) 'mutex)
                         (else 'once)))
                  (fail raise))
      (mutex-lock! mux #f #f)
      (let* ((id (mutex-specific mux))
             (result #f)
             (handler
              (case kind
                ((once)
                 (lambda (t x y)
                   (close x)
                   (receiver t x y)))
                ((multiple) receiver)
                ((mutex block) ;; special case of 'once likely useful
                 (set! result (make-mutex receiver))
                 (mutex-specific-set! result id)
                 (mutex-lock! result #f #f) ;; be careful NOT take onwership
                 (lambda (t x y)
                   (close x)
                   (cond
                    ((eqv? (mutex-state result) 'not-owned)
                     (receive results
                         (cond
                          ((procedure? receiver) (receiver t x y))
                          (else y))
                       (mutex-specific-set! result results)
                       (mutex-unlock! result))))))
                (else (error "unhandled argument kind" eventloop-open-channel kind)))))
        (mutex-specific-set! mux (+ id 1))
        (table-set! channels id handler)
        (mutex-unlock! mux)
        (or result id)))
    (define (await result)
      (mutex-lock! result)
      (let ((results (mutex-specific result)))
        (mutex-unlock! mux)
        (apply values results)))
    (mutex-specific-set! mux 1)
    (set! eventloop-open-channel open)
    (set! eventloop-close-channel close)
    (set! eventloop-await-channel await)
    (lambda (t x y)
      (let ((receiver (table-ref channels x #f)))
        (when receiver (receiver t x y))))))

(cond-expand
 (android
  (c-declare
   ;; calls GLState.fromNativeStart()
   "extern void android_GLState_start();")
  (define-macro (android-glstate-start)
    '((c-lambda () void "android_GLState_start")))
  (c-declare
   ;; calls GLState.fromNativeInitDraw()
   "extern void microgl_draw_before();")
  (define-macro (microgl-draw-before)
    '((c-lambda () void "microgl_draw_before"))))
 (else
  (define-macro (android-glstate-start)
    #!void)
  (define-macro (microgl-draw-before)
    #!void)))

(define (eventloop-handle-event t x y)
  (set! ##now (current-time-seconds))
  (let ((xtra (if (not app:mustinit) (event-pop) #f)))
    (if xtra (apply hook:event xtra))
    (cond
     ((eqv? t EVENT_REDRAW)
      (hook:event t 0 0))
     ((eqv? t EVENT_IDLE)
      (hook:event t 0 0)
      (cond-expand
       (win32
        ;; better wait here than in C.
        (thread-sleep! 0.05))
       (else #!void)))
     ((or (eqv? t EVENT_BUTTON1DOWN) (eqv? t EVENT_BUTTON1UP)
          (eqv? t EVENT_BUTTON2DOWN) (eqv? t EVENT_BUTTON2UP)
          (eqv? t EVENT_BUTTON3DOWN) (eqv? t EVENT_BUTTON3UP)
          (eqv? t EVENT_MOTION))
      ;; handle potential scaling (running stretched on a device)
      (hook:event t (if app:scale? (fix (* app:xscale x)) x)
                  (if app:scale? (fix (* app:yscale y)) y))
      )
     ((eqv? t EVENT_JSCM_RESULT)
      (case x
        ((0) ((on-jscm-result) t x y))
        (else (eventloop-notify-channel! t x y))))
     ((eqv? t EVENT_INIT)
      (android-glstate-start)
      ;; prevent multiple inits
      (when (and app:mustinit (> x 0) (> y 0))
        (set! app:mustinit #f)
        (eventloop-window-dimensions! x y)
        (eventloop-screen-dimensions! x y)
        (if (procedure? hook:init) (hook:init x y))
        ;;(debug 'init2 (list 'done w: app:width h: app:height sw: app:screenwidth sh: app:screenheight))
        (cond
         (app:forcefullscreen (microgl-fullscreen app:screenwidth app:screenheight))
         (else (microgl-window app:width app:height)))))
     ((eqv? t EVENT_TERMINATE)
      (log-system "System shutdown")
      (terminate))
     ((eqv? t EVENT_SUSPEND)
      (when (and (not app:mustinit) (not app:suspended))
        (set! app:suspended #t)
        (if (procedure? hook:suspend) (hook:suspend))))
     ((eqv? t EVENT_RESUME)
      (when (and (not app:mustinit) app:suspended)
        (set! app:suspended #f)
        (if (procedure? hook:resume) (hook:resume))))
     (else
      (if (and (not app:mustinit) (procedure? hook:event)) (hook:event t x y))))))

(define eventloop-thread #f)
(define (eventloop t x y)
  (continuation-capture
   (lambda (context)
     (with-exception-catcher
      (lambda (exn)
        (handle-debug-exception exn eventloop context: context)
        (exit 23))
      (lambda ()
        (define sigport (##open-predefined 1 'sigport ((c-lambda () int "___return(ffi_event_params.fd[0]);"))))
        (android-glstate-start)
        (eventloop-handle-event t x y)
        (do ()
            (#f)
          ;; unlock signals "done" into "hook.c"
          ((c-lambda () void "ln_gambit_unlock"))
          (read-char sigport)
          (let ((t ((c-lambda () int "___return(ffi_event_params.t);")))
                (x ((c-lambda () int "___return(ffi_event_params.x);")))
                (y ((c-lambda () int "___return(ffi_event_params.y);"))))
            ;;(debug 'handling (list t x y))
            (eventloop-handle-event t x y))))))))

(cond-expand
 ((or android linux macosx ios openbsd bb10 playbook netbsd)
  (c-define
   (c-event t x y) (int int int) void "scm_event" ""
   (let ()
     (declare (not interrupts-enabled))
     (cond
      ((and (eqv? EVENT_INIT t) (not eventloop-thread))
       (set! eventloop-thread (current-thread))
       (eventloop t x y))
      (else
       #f #;(thread-send eventloop-thread t))))))
 (else ;; backward compatible
  ;;
  ;; Note: we MUST BE reasonable sure that this is only ever called
  ;; from a single native thread.
  (c-define
   (c-event t x y) (int int int) void "scm_event" ""
   (eventloop-handle-event t x y))))


(c-declare
 #<<c-declare-end

 static int scm_exported_parameters[] =
 {
  1, // runflag
  0, // app width
  0, // app height
  0, // force fullscreen
  0, // screen width
  0,  // screen height
  };
int scm_runflag() { return scm_exported_parameters[0];}
int scm_width() { return scm_exported_parameters[1];}
int scm_height() { return scm_exported_parameters[2];}
int scm_forcefullscreen() { return scm_exported_parameters[3];}
int scm_screenwidth() { return scm_exported_parameters[4];}
int scm_screenheight() { return scm_exported_parameters[5];}
c-declare-end
)

(define (eventloop-window-dimensions! x y)
  (set! app:width x)
  (set! app:height y)
  ((c-lambda (int) void "scm_exported_parameters[1]=___arg1;") (if (number? x) x 0))
  ((c-lambda (int) void "scm_exported_parameters[2]=___arg1;") (if (number? y) y 0)))

(define (c-forcefullscreen! x)
  ((c-lambda (int) void "scm_exported_parameters[3]=___arg1;") (if x 1 0))
  (set! app:forcefullscreen x))

(define (eventloop-screen-dimensions! x y)
  (set! app:screenwidth x)
  (set! app:screenheight y)
  ((c-lambda (int) void "scm_exported_parameters[4]=___arg1;") (if (number? x) x 0))
  ((c-lambda (int) void "scm_exported_parameters[5]=___arg1;") (if (number? y) y 0)))

(define (c-runflag! x)
  ((c-lambda (int) void "scm_exported_parameters[0]=___arg1;") x)
  (set! app:runflag x))

(c-define (c-mediascanner-callback) () void "scm_mediascanner_callback" ""
  (set! android-mediascanner-done? #t))

;; change default size (don't go full screen!)
(define (make-window w h . force-fullscreen)
  (c-forcefullscreen! (and (pair? force-fullscreen) (car force-fullscreen)))
  (let* ((flip? (and app:forcefullscreen (> app:screenwidth app:screenheight)))
         (xscale (/ (flo (if flip? h w)) (flo app:screenwidth)))
         (yscale (/ (flo (if flip? w h)) (flo app:screenheight))))
    (if flip?
        (eventloop-window-dimensions! h w)
        (eventloop-window-dimensions! w h))
    (if (or app:forcefullscreen
            (string=? (system-platform) "ios")
            (string=? (system-platform) "bb10")
            (string=? (system-platform) "playbook")
            (string=? (system-platform) "android"))
        (begin
          (set! app:xscale xscale)
          (set! app:yscale yscale)
          (set! app:scale? #t)))))

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

(define (terminate . nomediascanner)
  (if app:android?
    (let ((run-mediascanner? (if (= (length nomediascanner) 1) (not (car nomediascanner)) #t)))
      (if run-mediascanner? (android-run-mediascanner))
      (android-finish)
      (if run-mediascanner?
        (let loop ()
          (if (not android-mediascanner-done?) (begin (thread-sleep! 0.1) (loop)))))))
  (if (procedure? hook:terminate) (hook:terminate))
)

;; eof
