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
;; Chat widget

(define (glgui:chat-split-allstrings g wgt)
  (let ((lst (glgui-widget-get g wgt 'list))
        (w (glgui-widget-get-dyn g wgt 'w))
        (fnt (glgui-widget-get g wgt 'font)))
    (let loop ((i 0) (strlst (list)))
      (if (= i (length lst)) strlst
        (loop (+ i 1) (append strlst (list (string-split-width (caddr (list-ref lst i)) (* w 0.8) fnt))))
      )
    )
  )
)

(define (glgui:chat-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (strlst (glgui-widget-get-dyn g wgt 'strlst))
         (lst (glgui-widget-get-dyn g wgt 'list))
         (n (length lst))
         (dh (glgui-widget-get g wgt 'dh))
         (fnt (glgui-widget-get g wgt 'font))
         (bg1 (glgui-widget-get g wgt 'bgcol1))
         (bg2 (glgui-widget-get g wgt 'bgcol2))
         (ofs (fix (floor (glgui-widget-get g wgt 'offset))))
         (deltat (glgui-widget-get g wgt 'deltatime)))
    ;; Update the strlst
    (if (or (not strlst) (not (fx= (length strlst) (length lst))))
      (begin 
        (set! strlst (glgui:chat-split-allstrings g wgt))
        (glgui-widget-set! g wgt 'strlst strlst)
        (set! ofs 0)
        (glgui-widget-set! g wgt 'offset ofs)
        (glgui-widget-set! g wgt 'valmax (length lst))
      )
    )
    ;; Now put the elements on the screen
    (if (fx> (length lst) 0)
      (let loop ((i ofs) (y0 y) (strings (list-ref strlst ofs)) (msg (list-ref lst ofs)))
        (if (and (< i n) (< (+ y0 (* dh (length strings))) (+ y h)))
          (let* ((dhline (* dh (length strings)))
                 (dx (if (fx= (cadddr msg) 1) (+ x (* w 0.2)) x))
                 (dy (+ y0 dhline (- dh) 1))
                 (dw (* w 0.8))
                 (bgcolor (if (fx= (cadddr msg) 1) bg1 bg2)))
            ;; Draw background color box
            (glgui:draw-box dx y0 (- dw 7) (- dhline 1) bgcolor)
            ;; Add the complete message text
            (let loop ((k 0))
              (if (= k (length strings)) #t
                (begin 
                  (glgui:draw-text-left dx (- dy (* k dh)) dw dh (list-ref strings k) fnt White)
                  (loop (+ k 1))
                )
              )
            ) 
            ;; Add timestamps in between messages 
            (if (fx< i (- n 1))
              (begin 
                ;; if older time difference larger than threshold
                (if (> (- (car msg) (car (list-ref lst (fx+ i 1)))) deltat)
                  (begin
                    (if (< (+ y0 (* dh (length strings)) dh) (+ y h))
                      (glgui:draw-text-center x (+ y0 dhline) w dh (seconds->string (car msg) 
                        (if (= (floor (/ ##now 86400)) (floor (/ (car msg) 86400))) "%H:%M:%S" "%Y-%m-%d %H:%M:%S")) fnt White)
                    )
                    (set! y0 (+ y0 dh))
                  )
                )
                ;; Extra space if person changes
                (if (not (fx= (cadddr msg) (cadddr (list-ref lst (fx+ i 1)))))
                  (set! y0 (+ y0 2))
                )
              )
              ;; and also if it is the first line
              (if (< (+ y0 (* dh (length strings)) dh) (+ y h)) 
                (begin
                  (glgui:draw-text-center x (+ y0 dhline) w dh (seconds->string (car msg) 
                    (if (= (floor (/ ##now 86400)) (floor (/ (car msg) 86400))) "%H:%M:%S" "%Y-%m-%d %H:%M:%S")) fnt White)
                  (glgui-widget-set! g wgt 'valmax ofs) 
                )
              )
            )
            (loop (fx+ i 1) (+ y0 dhline) 
                  (if (fx= (fx+ i 1) n) (list) (list-ref strlst (fx+ i 1))) 
                  (if (fx= (fx+ i 1) n) (list) (list-ref lst (fx+ i 1))))
          )
          (set! n (- i ofs))
        )
      )
    )
    ;; This is the slider
    (let* ((nlist (length lst))
           (sw 5) (sx (+ x w -6))
           (sh (if (< n nlist) (* h (/ n nlist)) h))
           (sy (+ y (if (= sh h) 0 (* h (/ ofs nlist))))))
      (glgui:draw-box sx sy sw sh DimGray)
    )
))

(define (glgui:chat-input g wgt type mx my)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (h (fix (glgui-widget-get-dyn g wgt 'h)))
         (cb (glgui-widget-get g wgt 'callback))
         (dh (glgui-widget-get g wgt 'dh))
         (lst (glgui-widget-get g wgt 'list))
         (n (length lst))
         (val (glgui-widget-get g wgt 'offset))
         (valmin 0)
         (valmax_temp (glgui-widget-get g wgt 'valmax))
         (valmax (if valmax_temp valmax_temp n))
         (old (glgui-widget-get g wgt 'old))
         (drag (glgui-widget-get g wgt 'drag))
         (oldy (glgui-widget-get g wgt 'oldy))
         (fsty (glgui-widget-get g wgt 'fsty))
         (space (/ (modulo h dh) 2.))
         (inside (and (fx> (fix mx) x) (fx< (fix mx) (fx+ x w)) (fx> (fix my) y) (fx< (fix my) (fx+ y h)))))
     (cond
       ((and (fx= type EVENT_MOTION) old) ;; drag
         (if (> (abs (- my fsty)) 5) (begin
           (glgui-widget-set! g wgt 'offset (max valmin (min valmax
              (+ val (* (- valmax valmin) (/ (flo (* 1. (- oldy my))) (flo h)))))))
           (glgui-widget-set! g wgt 'oldy (fix my))
           (glgui-widget-set! g wgt 'drag #t)
        ))
       )
       ((and inside (fx= type EVENT_BUTTON1DOWN)) ;; touch down
         (glgui-widget-set! g wgt 'oldy (fix my))
         (glgui-widget-set! g wgt 'fsty (fix my))
         (glgui-widget-set! g wgt 'old #t)
         (glgui-widget-set! g wgt 'drag #f)
       )
       ((and old (fx= type EVENT_BUTTON1UP)) ;; touch release
         (glgui-widget-set! g wgt 'offset (round val)) ;; snap to place?
         (if (and (not drag) inside cb)
           (let ((cur (round (+ val (/ (- (+ y h) (+ my space)) dh) -.5))))
             (if (and (>= cur 0) (>= cur val) (< cur (length lst))) (begin
               (glgui-widget-set! g wgt 'current cur)
               (if (procedure? cb) (cb g wgt type mx my))
            ))))
         (glgui-widget-set! g wgt 'old #f)
         (glgui-widget-set! g wgt 'drag #f)
       )
       (else (if (not inside) (glgui-widget-set! g wgt 'old #f)))
    )
  inside
))

(define (glgui-chat g x y w h dh lst fnt cb)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'callback cb
     'draw-handle  glgui:chat-draw
     'input-handle glgui:chat-input 
     'hidden #f
     'list lst
     'font fnt
     'offset 0
     'current -1
     'dh dh        ;; height of line
     'old  #f
     'drag #f
     'oldy  0
     'fsty  0
     'bgcol1 (color-shade White 0.2)
     'bgcol2 (color:shuffle #x021c4dff)
     'deltatime 300 ;; 5min
   ))
;; eof