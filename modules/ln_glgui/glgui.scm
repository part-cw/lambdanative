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
;; widget based GUI

;; ----
;; event propagation
;; this controls whether events are sent to all widgets, or just the one
;; whos input handler returns true first

(define glgui:propagate #f)

(define (glgui-propagate-set! p)
  (set! glgui:propagate p))

;; ----
;; suspend/resume
;; Note: no opengl code here - may be called outside of the GL context thread

(define glgui:active #t)

(define (glgui-suspend) (set! glgui:active #f))

(define (glgui-resume)
  (set! glgui:active #t)
  (set! glCore:needsinit #t) ;; reinitialize OpenGL pipeline
  (thread-sleep! 0.1) ;; this is needed on android??
)

;; ----

;; alignment
(define GUI_ALIGNLEFT  1)

(define GUI_ALIGNRIGHT  2)

(define GUI_ALIGNCENTER  3)

;; alignment
(define GUI_ALIGNTOP  4)

(define GUI_ALIGNBOTTOM  5)

;; text direction
(define GUI_LEFTTORIGHT  6)

(define GUI_RIGHTTOLEFT  7)

;; 20100629 modal dialog support
(define glgui:modalstate #f)

(define (glgui-modal-set! v)
  (set! glgui:modalstate v))

;; guis can be rotated into different orientations
;; 0 = no rotation
;; 1 = 90 deg clockwise
;; 2 = 90 deg counter clockwise
;; 3 = upside down
(define glgui:rotate 0)

(define (glgui-orientation-set! o)
;;  (let ((def (if (fx> (glgui-width-get) (glgui-height-get)) GUI_LANDSCAPE GUI_PORTRAIT)))
  (let ((def (if (> app:width app:height) GUI_LANDSCAPE GUI_PORTRAIT)))
    (cond
      ((fx= def o) (set! glgui:rotate 0))
      ((fx= o GUI_LANDSCAPE) (set! glgui:rotate 1))
      ((fx= o GUI_PORTRAIT) (set! glgui:rotate 1))
      ((fx= o GUI_SEASCAPE) (set! glgui:rotate
        (if (fx= def GUI_LANDSCAPE) 3 2)))
      ((fx= o GUI_UPSIDEDOWN) (set! glgui:rotate
        (if (fx= def GUI_PORTRAIT) 3 2)))
    )))

(define (glgui-width-get)
  (cond
    ((fx= glgui:rotate 0) app:width)
    ((fx= glgui:rotate 1) app:height)
    ((fx= glgui:rotate 2) app:height)
    ((fx= glgui:rotate 3) app:width)
  ))

(define (glgui-height-get)
  (cond
    ((fx= glgui:rotate 0) app:height)
    ((fx= glgui:rotate 1) app:width)
    ((fx= glgui:rotate 2) app:width)
    ((fx= glgui:rotate 3) app:height)
  ))


;; colors

(define GUI_CLEAR '(0. 0. 0. 0.))

(define GUI_FG White)

(define GUI_FGSELECT Orange)

(define GUI_BG DarkSlateGray)

(define GUI_BGSELECT DimGray)

(define (glgui:renderloop g1 . gx)
  (let ((guis (append (list g1) gx)))
    (for-each (lambda (g)
        (let* ((xofs (glgui-get g 'xofs))
               (yofs (glgui-get g 'yofs)))
          (glPushMatrix)
          (glTranslatef (flo xofs) (flo yofs) 0.)
          (for-each (lambda (wgt)
            (let* ((h (glgui-widget-get g wgt 'hidden))
                   (m (glgui-widget-get g wgt 'modal))
                   (p (glgui-widget-get g wgt 'draw-handle))
                   (wl2 (glgui-widget-get g wgt 'widget-list)))
              (if (and (if glgui:modalstate #t (not m)) (not h))
                (if (procedure? p) (p g wgt)
                  (if (list? wl2)
                    (let ((xofs (glgui-get wgt 'xofs))
                       (yofs (glgui-get wgt 'yofs))
                       (w (glgui-get wgt 'w #f))
                       (h (glgui-get wgt 'h #f)))
                       (glCoreClipPush 0 0 w h)
                       (glgui:renderloop wgt)
                       (glCoreClipPop)
               ))))))
             (glgui-get g 'widget-list))
          (glPopMatrix)))
    guis)))

;; render one or more guis to a window
;; 20100804: support gui offset
(define (glgui:render g1 . gx)
  (glCoreInit)  ;; setup the OpenGL pipeline
  (glPushMatrix)
  (cond
;;  ------ rotation code
    ((fx= glgui:rotate 1)
      (glRotatef -90. 0. 0. 1.)
      (glTranslatef (flo (- app:height)) 0. 0.))
    ((fx= glgui:rotate 2)
      (glRotatef 90. 0. 0. 1.)
      (glTranslatef 0. (flo (- app:width)) 0.))
    ((fx= glgui:rotate 3)
      (glRotatef 180. 0. 0. 1.)
      (glTranslatef (flo (- app:width)) (flo (- app:height)) 0.))
;;  ------
  )
  (apply glgui:renderloop (append (list g1) gx))
  (glPopMatrix)
)

(define (glgui:for-each-done func lst)
  (let ((len (length lst)))
    (let loop ((i 0)(done #f))
      (if (or (fx= i len) done) done
        (loop (fx+ i 1) (func (list-ref lst i)))))))

(define (glgui:inputloop t x0 y0 . gs)
    (let ((guis (reverse gs)))
      (glgui:for-each-done (lambda (gui)
        (let* ((xofs (glgui-get gui 'xofs))
               (yofs (glgui-get gui 'yofs))
               (cx (if (or (fx= t EVENT_KEYPRESS) (fx= t EVENT_KEYRELEASE)) x0 (- x0 xofs)))
               (cy (if (or (fx= t EVENT_KEYPRESS) (fx= t EVENT_KEYRELEASE)) y0 (- y0 yofs)))
               (x (if (or (fx= t EVENT_KEYPRESS) (fx= t EVENT_KEYRELEASE)) x0
                    (+ (- xofs) (cond
                      ((fx= glgui:rotate 0) x0)
                      ((fx= glgui:rotate 1) (- app:height y0))
                      ((fx= glgui:rotate 2) y0)
                      ((fx= glgui:rotate 3) (- app:width x0))
                      ))))
               (y (if (or (fx= t EVENT_KEYPRESS) (fx= t EVENT_KEYRELEASE)) y0
                    (+ (- yofs) (cond
                      ((fx= glgui:rotate 0) y0)
                      ((fx= glgui:rotate 1) x0)
                      ((fx= glgui:rotate 2) (- app:width x0))
                      ((fx= glgui:rotate 3) (- app:height y0))
                      ))))
               (widget-list (reverse (glgui-get gui 'widget-list))))
            (glgui:for-each-done (lambda (wgt)
               (let* ((h (glgui-widget-get gui wgt 'hidden))
                      (propagate (glgui-widget-get gui wgt 'propagate)) ;; per widget event fall through
                      (m (glgui-widget-get gui wgt 'modal))
                      (minput (glgui-widget-get gui wgt 'modalinput))  ;; allow input to non-modal in modal mode!
                      (p (glgui-widget-get gui wgt 'input-handle))
                      (container? (glgui-widget-get gui wgt 'container))
                      (r (if (and glgui:modalstate (or m minput) (not h))
                            (if (procedure? p) (p gui wgt t x y) (if container? (glgui:inputloop t cx cy wgt) #f))
                          (if (and (not glgui:modalstate) (not m) (not h))
                            (if (procedure? p) (p gui wgt t x y) (if container? (glgui:inputloop t cx cy wgt) #f)) #f))))
                  (and (not (or (fx= t EVENT_BUTTON1UP) (fx= t EVENT_KEYPRESS) (fx= t EVENT_KEYRELEASE) propagate glgui:propagate)) r)))
               widget-list)))
            guis)))

;; glgui-timings-set! change
;;
;; (glgui-timings-set! #!key (frame-period-max #f) (frame-period-min #f) (frame-period-custom #f))
;;
;; Keyword arguments - if given - customize the frame-redraw timings.
;;
;; frame-period-max: maximum period to wait
;; frame-period-min: minimum period
;;
;; frame-period-custom: A 1-ari procedure receiving the count of
;; consecutive EVENT_REDRAW's returning the delay to wait.
;;
;; If frame-period-custom is #f a heuristic increasing delay between from
;; max/min (currently linear, which maybe will be changed to an
;; exponential backoff) will be used.  If it is set, min/max are ignored.
;;
;; The heuristic assumes an interactive application which is often
;; idle.  Once an non-redraw event is seen, interaction is assumed and
;; frames are drawn frequently.  Without it is assumed idle and system
;; load is reduced.
;;
;; Examples:
;;
;;  (glgui-timings-set! frame-period-max: 2 frame-period-min: 0.01) -- start with 100/sec down to 0.5/sec
;;  (glgui-timings-set! frame-period-max: 0.5 frame-period-min: 0.05) -- 20/sec down to 2/sec
;;  (glgui-timings-set! frame-period-custom: (lambda (x) 0.01))  -- constant 100/sec
;;
;;  When concerned about a precise frame rate take a hint from gambit
;;  manual wrt. `thread-sleep!` and wait until a point in time no
;;  matter how long it took to process events and render the GUI:
;;
;; (glgui-timings-set!
;;  frame-period-custom:
;;  (let ((last-frame-time (current-time-seconds))
;;        (frame-period 0.05))
;;    (lambda (x) ;; ignoring `x`
;;      (let ((next (+ last-frame-time frame-period)))
;;        (set! last-frame-time next)
;;        (seconds->time next)))))

(define glgui-timings-set!)

;; glgui-wakeup! - a thunk, which when called will immediately unblock
;; the thread waiting in glgui-event.  Should be called if other
;; threads notice the event loop should proceed.  Immediately resets
;; frame-period to frame-period-min.
(define glgui-wakeup!)

;; process an input event
;; 20100519: allow multiple guis
;; 20100804: support gui offset

(define glgui-event
  (let ((frame-period-max-value 0.5) ;; How long to sleep at most in redraw.
        (step 0.05) ;; delay increase
        (consecutive-redraw-count 1)
        (customized-moment #f) ;; may be a procedure returning the wait time/moment
        (wait-mutex (make-mutex 'glgui-event))
        (wait-cv (make-condition-variable 'glgui-event)))
    (define (timings-set! #!key (frame-period-max #f) (frame-period-min #f) (frame-period-custom #f))
      (define (legal? x) (and (number? x) (positive? x)))
      (if (legal? frame-period-max) (set! frame-period-max-value frame-period-max))
      (if (legal? frame-period-min) (set! step frame-period-min))
      (if (or (not frame-period-custom) (procedure? frame-period-custom))
          (set! customized-moment frame-period-custom)))
    (define (wakeup!)
      (condition-variable-signal! wait-cv))
    (define (reset-wait!)
      (set! consecutive-redraw-count 1))
    (define (wait-for-time-or-signal!)
      ;; wait for delay or signal from other thread
      (if (let ((moment (if customized-moment
                            (customized-moment consecutive-redraw-count)
                            (min frame-period-max-value (* consecutive-redraw-count step)))))
              (mutex-unlock! wait-mutex wait-cv moment))
          (reset-wait!)
          (set! consecutive-redraw-count (fx+ consecutive-redraw-count 1))))
    (define (glgui-event guis t x0 y0)
      (if (and glgui:active app:width app:height)
          (let ((gs (if (list? guis) guis (list guis))))
            (if (fx= t EVENT_REDRAW)
                (when (mutex-lock! wait-mutex 0)
                  (apply glgui:render gs)
                  (wait-for-time-or-signal!))
                (begin
                  (reset-wait!)
                  (apply glgui:inputloop (append (list t x0 y0) gs)))))
          (if (fx= t EVENT_REDRAW)
              (wait-for-time-or-signal!)
              (if customized-moment
                  (thread-sleep! (customized-moment 1))
                  (begin
                    (thread-sleep! step)
                    (reset-wait!))))))
    (set! glgui-wakeup! wakeup!)
    (set! glgui-timings-set! timings-set!)
    glgui-event))

(define (glgui-timings-at-sec! sec)
  (define (wait-for-sec _) (seconds->time (+ ##now sec)))
  (define (no-wait _) 0)
  (cond-expand
   ((or android ios)
    ;; TBD: convey the time value to signaling code.
    ;; switch delays to zero
    (glgui-timings-set! frame-period-custom: no-wait))
   (else (glgui-timings-set! frame-period-custom: wait-for-sec))))

(define (glgui-timings-at-10msec!) (glgui-timings-at-sec! 0.01))

(cond-expand
 ((or android ios)
  ;; 50 Hz should be enough - TBD: not yet effective
  (glgui-timings-at-sec! 0.02))
 (else))

;; provide a screen shot
(define (glgui-screenshot)
  (let ((w (glgui-width-get))
        (h (glgui-height-get)))
    (glCoreReadPixels 0 0 w h)))

;; eof
