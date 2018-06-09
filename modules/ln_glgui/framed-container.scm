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
                   (<= (+ xofs (- mx drag_oldx)) frame-x)
                   (>= (+ xofs (- mx drag_oldx)) (- frame-x (- content-w frame-w))))
              (glgui-framed-container-content-ofs-set! g frame (+ xofs (- mx drag_oldx)) 'xofs))
          (if (and draggable_y?
                   (<= (+ yofs (- my drag_oldy)) frame-y)
                   (>= (+ yofs (- my drag_oldy)) (- frame-y (- content-h frame-h))))
              (glgui-framed-container-content-ofs-set! g frame (+ yofs (- my drag_oldy)) 'yofs))
          (glgui-widget-set! g wgt 'drag_oldx (fix mx))
          (glgui-widget-set! g wgt 'drag_oldy (fix my)))))
      ((or (and inside (fx= type EVENT_BUTTON1DOWN)) ;; touch down
           (and inside (fx= type EVENT_MOTION) (not drag_oldx) (not drag_oldy))) ;; if moving but no oldxy set, set them
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
    (cond ((eq? id 'hidden)
            (glgui:framed-container-hidden-set! g frame val))
          ((eq? id 'scrollcolor)
            (if x-scrollbar (glgui-widget-set! g x-scrollbar 'color val))
            (if y-scrollbar (glgui-widget-set! g y-scrollbar 'color val)))
          ((eq? id 'scrollwidth)
            (if x-scrollbar
              (begin (glgui-widget-set! g x-scrollbar 'h val)
                     (glgui-widget-set! g x-scrollbar 'y (- frame-y val))))
            (if y-scrollbar
              (begin (glgui-widget-set! g y-scrollbar 'w val)
                     (glgui-widget-set! g y-scrollbar 'x (+ frame-x frame-w val)))))
          ((eq? id 'scrollrounded)
            (if x-scrollbar (glgui-widget-set! g x-scrollbar 'rounded val))
            (if y-scrollbar (glgui-widget-set! g y-scrollbar 'rounded val))))))

;; Grows (or shrinks if delta negative) content in given dimension
;; and updates scrollbars correspondingly
;; dim == 'w or 'h
(define (glgui-framed-container-content-grow g frame delta dim)
  (let* ((content (glgui-widget-get g frame 'content))
         (ofs     (if (eq? dim 'w) 'xofs 'yofs))
         (old-ofs (glgui-widget-get g content ofs))
         (new-ofs (if (eq? ofs 'yofs) (- old-ofs delta) old-ofs))
         (old-dim (glgui-widget-get g content dim))
         (new-dim (+ old-dim delta)))
    (glgui-widget-set! g content dim new-dim)
    (glgui-widget-set! g content ofs new-ofs)
    (if (not (glgui-framed-container-content-position-valid? g frame))
        (glgui-framed-container-content-ofs-reset! g frame)
        (glgui:framed-container-scrollbars-set! g frame))))

;; Checks to see if content is in a valid position,
;; i.e. if the top isn't too low or if the bottom isn't too high, same with the sides
;; ofs-set! may cause content to be in an invalid position which makes it undraggable
(define (glgui-framed-container-content-position-valid? g frame)
  (let* ((content   (glgui-widget-get g frame   'content))
         (content-x (glgui-widget-get g content 'xofs))
         (content-y (glgui-widget-get g content 'yofs))
         (content-w (glgui-widget-get g content 'w))
         (content-h (glgui-widget-get g content 'h))
         (frame-x   (glgui-widget-get g frame 'xofs))
         (frame-y   (glgui-widget-get g frame 'yofs))
         (frame-w   (glgui-widget-get g frame 'w))
         (frame-h   (glgui-widget-get g frame 'h)))
    (and (<= content-x frame-x)
         (>= content-x (- frame-x (- content-w frame-w)))
         (<= content-y frame-y)
         (>= content-y (- frame-y (- content-h frame-h))))))

;; Set position back to initial settings
;; The upper-left corner of content will be aligned with frame
(define (glgui-framed-container-content-ofs-reset! g frame)
  (let* ((content    (glgui-widget-get g frame   'content))
         (frame-h    (glgui-widget-get g frame   'h))
         (content-h  (glgui-widget-get g content 'h))
         (new-xofs   (glgui-widget-get g frame   'xofs))
         (frame-yofs (glgui-widget-get g frame   'yofs))
         (new-yofs   (- frame-yofs (- content-h frame-h))))
    (glgui-framed-container-content-ofs-set! g frame new-xofs 'xofs)
    (glgui-framed-container-content-ofs-set! g frame new-yofs 'yofs)))

;; Set new position for content and widgets in frame
;; ofs == 'xofs or 'yofs
(define (glgui-framed-container-content-ofs-set! g frame new-ofs ofs)
  (let* ((content (glgui-widget-get g frame 'content))
         (old-ofs (glgui-widget-get g content ofs))
         (dofs (- new-ofs old-ofs)))
    (glgui-widget-set! g content ofs new-ofs)
    (glgui:framed-container-subwidgets-shift! frame dofs (if (eq? ofs 'xofs) 'x 'y))
    (glgui:framed-container-scrollbars-set! g frame)))

;; Shift subwidgets by adding shift
;; dir == 'x or 'y
(define (glgui:framed-container-subwidgets-shift! frame shift dir)
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

(define (glgui:framed-container-hidden-set! g frame b)
  (let ((content     (glgui-widget-get g frame 'content))
        (x-scrollbar (glgui-widget-get g frame 'x-scrollbar))
        (y-scrollbar (glgui-widget-get g frame 'y-scrollbar)))
    (glgui-widget-set! g content 'hidden b)
    ;; For some reason, unhiding containers will reset their positions
    ;; but unhiding other widgets does not change their positions
    (if b (begin (glgui-widget-set! g x-scrollbar 'hidden #t)
                 (glgui-widget-set! g y-scrollbar 'hidden #t))
          (glgui:framed-container-scrollbars-set! g frame))))

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
  (let* ((frame   (glgui-container g x y frame-w frame-h))
         (content (glgui-container g x (- y (- content-h frame-h)) content-w content-h))
         (scrollbar-w (/ (* frame-w frame-w) content-w))
         (scrollbar-h (/ (* frame-h frame-h) content-h))
         (x-scroll? (> content-w frame-w))
         (y-scroll? (> content-h frame-h))
         (x-scrollbar (glgui-box g x (- y 4) scrollbar-w 4 DimGrey))
         (y-scrollbar (glgui-box g (+ x frame-w 4) (+ y (- frame-h scrollbar-h)) 4 scrollbar-h DimGrey)))
    (glgui-widget-set! g x-scrollbar 'hidden x-scroll?)
    (glgui-widget-set! g y-scrollbar 'hidden y-scroll?)
    (glgui-widget-set! g frame   'update-handle glgui:framed-container-update)
    (glgui-widget-set! g frame   'input-handle  (lambda xargs #f))
    (glgui-widget-set! g frame   'content       content)
    (glgui-widget-set! g frame   'x-scrollbar   x-scrollbar)
    (glgui-widget-set! g frame   'y-scrollbar   y-scrollbar)
    (glgui-widget-set! g frame   'scrollcolor   DimGrey)
    (glgui-widget-set! g frame   'scrollwidth   4)
    (glgui-widget-set! g frame   'scrollrounded #f)
    (glgui-widget-set! g content 'update-handle #f)
    (glgui-widget-set! g content 'input-handle  glgui:framed-container-input)
    (glgui-widget-set! g content 'draggable_x?  #t)
    (glgui-widget-set! g content 'draggable_y?  #t)
    (glgui-widget-set! g content 'frame         frame)
    frame))

;; eof
