#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2018, University of British Columbia
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

;; HANDLERS

;; Input handler for CONTENT, not frame!
(define (glgui:framed-container-input g wgt type mx my)
  (let* ((xofs         (glgui-widget-get-dyn g wgt 'xofs))
         (yofs         (glgui-widget-get-dyn g wgt 'yofs))
         (content-w    (glgui-widget-get g wgt   'w))
         (content-h    (glgui-widget-get g wgt   'h))
         (frame        (glgui-widget-get g wgt   'frame))
         (frame-x      (glgui-widget-get g frame 'xofs))
         (frame-y      (glgui-widget-get g frame 'yofs))
         (frame-w      (glgui-widget-get g frame 'w))
         (frame-h      (glgui-widget-get g frame 'h))
         (cb           (glgui-widget-get g wgt    'callback))
         (drag_oldx    (glgui-widget-get g wgt    'drag_oldx))
         (drag_oldy    (glgui-widget-get g wgt    'drag_oldy))
         (dragged?     (glgui-widget-get g wgt    'dragged?))
         (draggable_x? (glgui-widget-get g wgt    'draggable_x?))
         (draggable_y? (glgui-widget-get g wgt    'draggable_y?))
         (inside (and (> mx frame-x) (< mx (+ frame-x frame-w))
                      (> my frame-y) (< my (+ frame-y frame-h)))))
    (cond
      ((and (fx= type EVENT_MOTION) drag_oldx drag_oldy) ;; drag
        (if (or (> (abs (- mx drag_oldx)) 5) (> (abs (- my drag_oldy)) 5)) (begin
          (glgui-widget-set! g wgt 'dragged? #t)
          (glgui:framed-container-subwidgets-dearm! frame)
          (if (and draggable_x?
                   (or (and (>= mx drag_oldx) (<= (+ xofs (- mx drag_oldx)) frame-x))
                       (and (<= mx drag_oldx) (>= (+ xofs (- mx drag_oldx)) (- frame-x (- content-w frame-w))))))
              (glgui-widget-set! g frame 'scroll-x (- frame-x (+ xofs (- mx drag_oldx)))))
          (if (and draggable_y?
                   (or (and (>= my drag_oldy) (<= (+ yofs (- my drag_oldy)) frame-y))
                       (and (<= my drag_oldy) (>= (+ yofs (- my drag_oldy)) (- frame-y (- content-h frame-h))))))
              (glgui-widget-set! g frame 'scroll-y (- (+ yofs (- my drag_oldy) content-h) frame-y frame-h)))
          (glgui-widget-set! g wgt 'drag_oldx (fix mx))
          (glgui-widget-set! g wgt 'drag_oldy (fix my)))))
      ((and inside (fx= type EVENT_BUTTON1DOWN)) ;; touch down
        (glgui-widget-set! g wgt 'drag_oldx (fix mx))
        (glgui-widget-set! g wgt 'drag_oldy (fix my))
        (glgui-widget-set! g wgt 'dragged? #f))
      ((and drag_oldx drag_oldy (fx= type EVENT_BUTTON1UP)) ;; touch release
        (if (and inside dragged?)
            (begin (glgui-widget-set! g wgt 'dragged? #f)
                   (glgui-widget-set! g wgt 'drag_oldx #f))
                   (glgui-widget-set! g wgt 'drag_oldy #f)))
      (else
        (if (not inside)
            (begin (glgui-widget-set! g wgt 'drag_oldx #f)
                   (glgui-widget-set! g wgt 'drag_oldy #f)))))
    (if (and (not dragged?) (or inside (fx= type EVENT_KEYPRESS) (fx= type EVENT_KEYRELEASE)))
        (glgui:inputloop type mx my frame)
        inside)))

;; Update handler for FRAME, not content
(define (glgui:framed-container-update g frame id val)
  (let ((frame-x (glgui-widget-get g frame 'xofs))
        (frame-y (glgui-widget-get g frame 'yofs))
        (frame-w (glgui-widget-get g frame 'w))
        (x-scrollbar (glgui-widget-get g frame 'x-scrollbar))
        (y-scrollbar (glgui-widget-get g frame 'y-scrollbar)))
    (cond ((eq? id 'scrollcolor)
            (glgui-widget-set! g x-scrollbar 'color val)
            (glgui-widget-set! g y-scrollbar 'color val))
          ((eq? id 'scrollwidth)
            (glgui-widget-set! g x-scrollbar 'h val)
            (glgui-widget-set! g y-scrollbar 'w val)
            (glgui-widget-set! g x-scrollbar 'y (- frame-y val))
            (glgui-widget-set! g y-scrollbar 'x (+ frame-x frame-w val)))
          ((eq? id 'scrollrounded)
            (glgui-widget-set! g x-scrollbar 'rounded val)
            (glgui-widget-set! g y-scrollbar 'rounded val))
          ((eq? id 'hidden)
            (glgui:framed-container-hidden-set! g frame val))
          ((eq? id 'scroll-x)
            (glgui:framed-container-scroll-x-set! g frame val))
          ((eq? id 'scroll-y)
            (glgui:framed-container-scroll-y-set! g frame val))
          ((eq? id 'content-w)
            (glgui:framed-container-content-w-set! g frame val))
          ((eq? id 'content-h)
            (glgui:framed-container-content-h-set! g frame val))
          ((eq? id 'xofs)
            (glgui:framed-container-position-x-set! g frame val))
          ((eq? id 'yofs)
            (glgui:framed-container-position-y-set! g frame val)))))

;; UPDATE-HANDLE FUNCTIONS

;; Set hidden for content and scrollbars
(define (glgui:framed-container-hidden-set! g frame b)
  (let ((content     (glgui-widget-get g frame 'content))
        (x-scrollbar (glgui-widget-get g frame 'x-scrollbar))
        (y-scrollbar (glgui-widget-get g frame 'y-scrollbar)))
    (glgui-widget-set! g content 'hidden b)
    (if b (begin (glgui-widget-set! g x-scrollbar 'hidden #t)
                 (glgui-widget-set! g y-scrollbar 'hidden #t))
          (glgui:framed-container-scrollbars-set! g frame))))

;; Set how far right the widget should be scrolled to
(define (glgui:framed-container-scroll-x-set! g frame scroll-x)
  (let* ((content          (glgui-widget-get g frame   'content))
         (scroll-x-old     (glgui-widget-get g frame   'scroll-x))
         (frame-x          (glgui-widget-get g frame   'xofs))
         (content-xofs-old (glgui-widget-get g content 'xofs))
         (content-xofs-new (- frame-x scroll-x))
         (shift            (- content-xofs-new content-xofs-old)))
    (glgui-widget-set! g content 'xofs content-xofs-new)
    (glgui:framed-container-subwidgets-shift! frame 'x shift)
    (glgui:framed-container-scrollbars-set! g frame)))

;; Set how far down the widget should be scrolled to
(define (glgui:framed-container-scroll-y-set! g frame scroll-y)
  (let* ((content          (glgui-widget-get g frame   'content))
         (scroll-y-old     (glgui-widget-get g frame   'scroll-y))
         (frame-y          (glgui-widget-get g frame   'yofs))
         (frame-h          (glgui-widget-get g frame   'h))
         (content-h        (glgui-widget-get g content 'h))
         (content-yofs-old (glgui-widget-get g content 'yofs))
         (content-yofs-new (- (+ frame-y frame-h scroll-y) content-h))
         (shift            (- content-yofs-new content-yofs-old)))
    (glgui-widget-set! g content 'yofs content-yofs-new)
    (glgui:framed-container-subwidgets-shift! frame 'y shift)
    (glgui:framed-container-scrollbars-set! g frame)))

;; Set width of content
(define (glgui:framed-container-content-w-set! g frame content-w)
  (let* ((content (glgui-widget-get g frame 'content)))
    (glgui-widget-set! g content 'w content-w)
    (glgui:framed-container-scrollbars-set! g frame)))

;; Set height of content
(define (glgui:framed-container-content-h-set! g frame content-h)
  (let* ((content   (glgui-widget-get g frame   'content))
         (frame-y   (glgui-widget-get g frame   'yofs))
         (frame-h   (glgui-widget-get g frame   'h))
         (scroll-y  (glgui-widget-get g frame   'scroll-y))
         (content-yofs-new (- (+ frame-y frame-h scroll-y) content-h)))
    (glgui-widget-set! g content 'h content-h)
    (glgui-widget-set! g content 'yofs content-yofs-new)
    (glgui:framed-container-scrollbars-set! g frame)))

;; Set x-position of everything after frame-x has been changed
(define (glgui:framed-container-position-x-set! g frame frame-x)
  (let* ((content  (glgui-widget-get g frame 'content))
         (scroll-x (glgui-widget-get g frame 'scroll-x))
         (content-xofs-new (- frame-x scroll-x)))
    (glgui-widget-set! g content 'yofs content-xofs-new)
    (glgui:framed-container-scrollbars-set! g frame)))

;; Set y-position of everything after frame-y has been changed
(define (glgui:framed-container-position-y-set! g frame frame-y)
  (let* ((content   (glgui-widget-get g frame   'content))
         (scroll-y  (glgui-widget-get g frame   'scroll-y))
         (frame-h   (glgui-widget-get g frame   'h))
         (content-h (glgui-widget-get g content 'h))
         (content-yofs-new (- (+ frame-y frame-h scroll-y) content-h)))
    (glgui-widget-set! g content 'yofs content-yofs-new)
    (glgui:framed-container-scrollbars-set! g frame)))

;; PUBLIC FUNCTIONS

;; If right side of content is to the left  of the right side of frame, snap to right side of frame
;; If left  side of content is to the right of the left  side of frame, snap to left  side of frame
(define (glgui-framed-container-position-x-snap! g frame)
  (let* ((content     (glgui-widget-get g frame 'content))
         (frame-x     (glgui-widget-get g frame   'xofs))
         (frame-w     (glgui-widget-get g frame   'w))
         (content-x   (glgui-widget-get g content 'xofs))
         (content-w   (glgui-widget-get g content 'w))
         (scrollable? (> content-w frame-w))
         (too-left?   (< (+ content-x content-w) (+ frame-x frame-w)))
         (too-right?  (> content-x frame-x)))
    (if (and too-left? scrollable?)
        (glgui-widget-set! g frame 'scroll-x (- content-w frame-w)))
    (if (or too-right? (and too-left? (not scrollable?)))
        (glgui-widget-set! g frame 'scroll-x 0))))

;; If bottom of content is above bottom of frame, snap to bottom of frame
;; If top    of content is below top    of frame, snap to top of frame
(define (glgui-framed-container-position-y-snap! g frame)
  (let* ((content     (glgui-widget-get g frame   'content))
         (frame-y     (glgui-widget-get g frame   'yofs))
         (frame-h     (glgui-widget-get g frame   'h))
         (content-y   (glgui-widget-get g content 'yofs))
         (content-h   (glgui-widget-get g content 'h))
         (scrollable? (> content-h frame-h))
         (too-high?   (> content-y frame-y))
         (too-low?    (< (+ content-y content-h) (+ frame-y frame-h))))
    (if (and too-high? scrollable?)
        (glgui-widget-set! g frame 'scroll-y (- content-h frame-h)))
    (if (or too-low? (and too-high? (not scrollable?)))
        (glgui-widget-set! g frame 'scroll-y 0))))

;; PRIVATE FUNCTIONS

;; Shift subwidgets by adding shift
;; dir == 'x or 'y
(define (glgui:framed-container-subwidgets-shift! frame dir shift)
  (for-each (lambda (subwgt)
    (let ((sub-dir (glgui-widget-get frame subwgt dir)))
      (glgui-widget-set! frame subwgt dir (+ sub-dir shift))))
    (glgui-get frame 'widget-list)))

;; De-arm all subwidgets manually by triggering input handler with button up event outside of bounds
(define (glgui:framed-container-subwidgets-dearm! frame)
  (for-each (lambda (subwgt)
    (let ((sub-x   (glgui-widget-get frame subwgt 'x))
          (sub-y   (glgui-widget-get frame subwgt 'y))
          (armed   (glgui-widget-get frame subwgt 'armed))
          (handler (glgui-widget-get frame subwgt 'input-handle)))
      (if (and armed (procedure? handler)) (handler frame subwgt EVENT_BUTTON1UP sub-x sub-y))))
    (glgui-get frame 'widget-list)))

;; The ratios scrollbar-w/h : frame-w/h and frame-w/h : content-w/h are equal
;; If the left/bottom of content is dx/dy beyond/below the left/bottom of frame,
;; then the left/bottom of y-scrollbar should be dx/dy * r after/above the left of frame,
;; where r = frame-w/h / content-w/h
(define (glgui:framed-container-scrollbars-set! g frame)
  (let* ((content     (glgui-widget-get g frame   'content))
         (x-scrollbar (glgui-widget-get g frame   'x-scrollbar))
         (y-scrollbar (glgui-widget-get g frame   'y-scrollbar))
         (frame-w     (glgui-widget-get g frame   'w))
         (frame-h     (glgui-widget-get g frame   'h))
         (frame-x     (glgui-widget-get g frame   'xofs))
         (frame-y     (glgui-widget-get g frame   'yofs))
         (content-w   (glgui-widget-get g content 'w))
         (content-h   (glgui-widget-get g content 'h))
         (content-x   (glgui-widget-get g content 'xofs))
         (content-y   (glgui-widget-get g content 'yofs))
         (x-scroll?   (> content-w frame-w))
         (y-scroll?   (> content-h frame-h))
         (x-scrollbar-w (/ (* frame-w frame-w) content-w))
         (y-scrollbar-h (/ (* frame-h frame-h) content-h))
         (x-scrollbar-x (+ frame-x (* (- frame-x content-x) (/ frame-w content-w))))
         (y-scrollbar-y (+ frame-y (* (- frame-y content-y) (/ frame-h content-h)))))
    (glgui-widget-set! g x-scrollbar 'w x-scrollbar-w)
    (glgui-widget-set! g y-scrollbar 'h y-scrollbar-h)
    (glgui-widget-set! g x-scrollbar 'x x-scrollbar-x)
    (glgui-widget-set! g y-scrollbar 'y y-scrollbar-y)
    (glgui-widget-set! g x-scrollbar 'hidden (not x-scroll?))
    (glgui-widget-set! g y-scrollbar 'hidden (not y-scroll?))))


;; A container that behaves like a 2D scrollbox
;; frame is the actual container in which widgets are placed
;;  frame-w, frame-h are the dimensions of the visible area
;; content receives drag input and tells frame's widgets to move within frame
;;  content-w, content-h are the dimensions of the actual content
;; If content-* > frame-*, then we can drag to pan over the widgets
;; Returns frame for adding widgets; content not directly exposed
;; N.B. framed-container-input is the callback for events on content, not frame
(define (glgui-framed-container g x y frame-w frame-h content-w content-h)
  (let* ((content (glgui-container g x (- y (- content-h frame-h)) content-w content-h))
         (scrollbar-w (/ (* frame-w frame-w) content-w))
         (scrollbar-h (/ (* frame-h frame-h) content-h))
         (x-scroll?   (> content-w frame-w))
         (y-scroll?   (> content-h frame-h))
         (x-scrollbar (glgui-box g x (- y 4) scrollbar-w 4 DimGrey))
         (y-scrollbar (glgui-box g (+ x frame-w 4) (+ y (- frame-h scrollbar-h)) 4 scrollbar-h DimGrey))
         (frame (glgui-widget-add g
            'x 0
            'y 0
            'xofs x
            'yofs y
            'w frame-w
            'h frame-h
            'content content
            'content-w content-w
            'content-h content-h
            'x-scrollbar x-scrollbar
            'y-scrollbar y-scrollbar
            'scrollcolor DimGrey
            'scrollwidth 4
            'scrollrounded #f
            'scroll-x 0
            'scroll-y 0
            'parent g
            'widget-list '()
            'widget-count 0
            'container #t
            'hidden #f
            'draw-handle #f
            'input-handle (lambda xargs #f)
            'draggable_x? #f
            'draggable_y? #f
            'drag_keep 10
            'update-handle glgui:framed-container-update)))
    (glgui-widget-set! g x-scrollbar 'hidden (not x-scroll?))
    (glgui-widget-set! g y-scrollbar 'hidden (not y-scroll?))
    (glgui-widget-set! g content 'frame         frame)
    (glgui-widget-set! g content 'update-handle #f)
    (glgui-widget-set! g content 'input-handle  glgui:framed-container-input)
    (glgui-widget-set! g content 'draggable_x?  #t)
    (glgui-widget-set! g content 'draggable_y?  #t)
    frame))

;; eof
