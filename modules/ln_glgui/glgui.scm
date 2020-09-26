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

;; process an input event
;; 20100519: allow multiple guis
;; 20100804: support gui offset

(define glgui-frame-period-max (make-parameter 0.5)) ;; How long to sleep at most in redraw.

;; glgui-wakeup! - a thunk, which when called will immediately unblock
;; the thread waiting in glgui-event.  Should be called if other
;; threads notice the event loop should proceed.
(define glgui-wakeup!)

(define glgui-event
  (let ((consecutive-redraw-count 1)
        (step 0.05)
        (wait-mutex (make-mutex 'glgui-event))
        (wait-cv (make-condition-variable 'glgui-event)))
    (define (wakeup!)
      (condition-variable-signal! wait-cv))
    (define (reset-wait!)
      (set! consecutive-redraw-count 1))
    (define (wait-for-time-or-signal!)
      (if (let ((moment (min (glgui-frame-period-max) (* consecutive-redraw-count step))))
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
              (reset-wait!))))
    (set! glgui-wakeup! wakeup!)
    glgui-event))

;; provide a screen shot
(define (glgui-screenshot)
  (let ((w (glgui-width-get))
        (h (glgui-height-get)))
    (glCoreReadPixels 0 0 w h)))

;; eof
