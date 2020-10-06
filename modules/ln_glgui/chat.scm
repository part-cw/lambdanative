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

(define glgui:chat-group-colors
  (list DarkGreen DarkOrange DarkBlue DarkRed DarkViolet)
)

;; Get color associated with given name
(define (glgui:chat-color-name g wgt name)
  (let* ((namecolors (glgui-widget-get g wgt 'namecolors))
         (colorpos (glgui-widget-get g wgt 'colorpos))
         (colorref (assoc (if (list? name) (car name) name) namecolors)))
    ;; If color already assigned to name, return assigned color
    (if (list? colorref)
      (cadr colorref)
      (if (list? name)
        ;; If user provided a color for name, assign that color to name
        (begin
          (glgui-widget-set! g wgt 'namecolors (cons name namecolors))
          (cadr name)
        )
        ;; Else, assign a color to name using default color list
        (if (fx>= colorpos (length glgui:chat-group-colors))
          (begin
            (glgui-widget-set! g wgt 'namecolors (cons (list name (list-ref glgui:chat-group-colors 0)) namecolors))
            (glgui-widget-set! g wgt 'colorpos 1)
            (list-ref glgui:chat-group-colors 0)
          )
          (begin
            (glgui-widget-set! g wgt 'namecolors (cons (list name (list-ref glgui:chat-group-colors colorpos)) namecolors))
            (glgui-widget-set! g wgt 'colorpos (fx+ colorpos 1))
            (list-ref glgui:chat-group-colors colorpos)
          )
        )
      )
    )
  )
)

(define (glgui:chat-split-allstrings g wgt)
  (let ((lst (glgui-widget-get g wgt 'list))
        (w (glgui-widget-get-dyn g wgt 'w))
        (fnt (glgui-widget-get g wgt 'font)))
    (let loop ((i 0) (strlst (list)))
      (if (= i (length lst)) strlst
        (let ((msg (list-ref lst i)))
          (loop (+ i 1) (append strlst (list (string-split-width (caddr msg) (if (fx= (cadddr msg) 2) w (* w 0.8)) fnt))))
        )
      )
    )
  )
)

(define (glgui:chat-draw g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (strlst-cache (glgui-widget-get-dyn g wgt 'strlst-cache))
         (strlst (or (and (vector? strlst-cache) (vector-ref strlst-cache 1)) '()))
         (lst (or (glgui-widget-get-dyn g wgt 'list) '()))
         (n (length lst))
         (dh (glgui-widget-get g wgt 'dh))
         (fnt (glgui-widget-get g wgt 'font))
         (bg1 (glgui-widget-get g wgt 'bgcol1))
         (bg2 (glgui-widget-get g wgt 'bgcol2))
         (bg3 (glgui-widget-get g wgt 'bgcol3))
         (txtcol (glgui-widget-get g wgt 'txtcol))
         (tscol (glgui-widget-get g wgt 'tscol))
         (group (glgui-widget-get g wgt 'group))
         (ofs (fix (floor (glgui-widget-get g wgt 'offset))))
         (deltat (glgui-widget-get g wgt 'deltatime)))
    (unless ;; Cache hit
        (and (vector? strlst-cache) (eq? (vector-ref strlst-cache 0) lst))
      ;; Update the strlst caching changes
      (set! strlst (glgui:chat-split-allstrings g wgt))
      (glgui-widget-set! g wgt 'strlst-cache (vector lst strlst))
      ;; (glgui-widget-set! g wgt 'strlst strlst)
      (set! ofs 0)
      (glgui-widget-set! g wgt 'offset ofs)
      (glgui-widget-set! g wgt 'valmax (length lst)))
    ;; Now put the elements on the screen
    (when (pair? lst)
      (let loop ((i ofs) (y0 y) (strings (list-ref strlst ofs)) (msg (list-ref lst ofs)))
        (if (and (< i n) (< (+ y0 (* dh (length strings))) (+ y h)))
          (let* ((dhline (* dh (length strings)))
                 (sender (cadddr msg))
                 (dx (if (fx= sender 1) (+ x (* w 0.2)) x))
                 (dy (+ y0 dhline (- dh) 1))
                 (dw (if (fx= sender 2) w (* w 0.8)))
                 (bgcolor (cond ((fx= sender 1) bg1) ((fx= sender 0) bg2) (else bg3)))
                 (name (cadr msg)))
            ;; Draw background color box
            (glgui:draw-box dx y0 (- dw 7) (- (+ 0 (if (and group
                                                          (or (and (fx< i (- n 1))
                                                                   (not (equal? name (cadr (list-ref lst (fx+ i 1))))))
                                                              (fx= i (- n 1)))
                                                          (not (fx= sender 1))) dh 0) dhline) 1) bgcolor)
            ;; Add the complete message text
            (let loop ((k 0))
              (if (= k (length strings)) #t
                (begin
                  (glgui:draw-text-left dx (- dy (* k dh) 1) dw dh (list-ref strings k) fnt txtcol)
                  (loop (+ k 1))
                )
              )
            )
            ;; Add timestamps in between messages
            (if (fx< i (- n 1))
              (begin
                ;; Add name if receiving user changes
                (if (and group
                         (not (equal? name (cadr (list-ref lst (fx+ i 1)))))
                         (not (fx= sender 1)))
                  (begin
                    (if (< (+ y0 (* dh (length strings)) dh) (+ y h))
                      (glgui:draw-text-left x (+ y0 (* (length strings) dh)) w dh (if (list? name) (car name) name) fnt (glgui:chat-color-name g wgt name))
                    )
                    (set! y0 (+ y0 dh))
                  )
                )
                ;; if older time difference larger than threshold
                (if (> (- (car msg) (car (list-ref lst (fx+ i 1)))) deltat)
                  (begin
                    (if (< (+ y0 (* dh (length strings)) dh) (+ y h))
                      (glgui:draw-text-center x (+ y0 dhline) w dh (seconds->string (car msg)
                        (if (= (floor (/ ##now 86400)) (floor (/ (car msg) 86400))) "%H:%M:%S" "%Y-%m-%d %H:%M:%S")) fnt tscol)
                    )
                    (set! y0 (+ y0 dh))
                  )
                )
                ;; Extra space if person changes
                (if (or (not (fx= sender (cadddr (list-ref lst (fx+ i 1)))))
                        (not (equal? name (cadr (list-ref lst (fx+ i 1))))))
                  (set! y0 (+ y0 5))
                )
              )
              ;; and also if it is the first line
              (if (< (+ y0 (* dh (length strings)) dh) (+ y h))
                (begin
                  (if (and group (not (fx= sender 1)))
                    (begin
                      (glgui:draw-text-left x (+ y0 dhline) w dh (if (list? name) (car name) name) fnt (glgui:chat-color-name g wgt name))
                      (set! y0 (+ y0 dh))
                    )
                  )
                  (glgui:draw-text-center x (+ y0 dhline) w dh (seconds->string (car msg)
                    (if (= (floor (/ ##now 86400)) (floor (/ (car msg) 86400))) "%H:%M:%S" "%Y-%m-%d %H:%M:%S")) fnt tscol)
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
  (let* ((mx-fixed (fix mx)) ;; Q: Can `mx` or `my` ever not be fixnums?
         (my-fixed (fix my)) ;; converting them once here for both readability and speed
         (x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (h (fix (glgui-widget-get-dyn g wgt 'h)))
         (cb (glgui-widget-get g wgt 'callback))
         (dh (glgui-widget-get g wgt 'dh))
         (lst (glgui-widget-get g wgt 'list))
         (n (length lst)) ;; length is O(n) - try to avoid
         (val (glgui-widget-get g wgt 'offset))
         (valmin 0)
         (valmax_temp (glgui-widget-get g wgt 'valmax))
         (valmax (if valmax_temp valmax_temp n))
         (old (glgui-widget-get g wgt 'old)) ;; boolean: "touched/clicked"
         (drag (glgui-widget-get g wgt 'drag)) ;; boolean: "touched/clicked" and "motion seen"
         (oldy (glgui-widget-get g wgt 'oldy)) ;; y coordinate upon "touched/clicked" start
         (fsty (glgui-widget-get g wgt 'fsty)) ;; y coordinate upon "touched/clicked" start
         (space (/ (modulo h dh) 2.))
         (inside (and (fx> mx-fixed x) (fx< mx-fixed (fx+ x w)) (fx> my-fixed y) (fx< my-fixed (fx+ y h)))))
    (cond
     ((and (fx= type EVENT_MOTION) old) ;; drag
      (if (> (abs (- my fsty)) 5)
          (let* ((calculated (+ val (* (- valmax valmin) (/ (flo (* 1. (- oldy my))) (flo h))))) ;; Why these `flo`?
                 (clipped (max valmin (min valmax calculated))))
            (glgui-widget-set! g wgt 'offset clipped)
            (glgui-widget-set! g wgt 'oldy my-fixed)
            (if (not drag) (glgui-widget-set! g wgt 'drag #t)))))
     ((and inside (fx= type EVENT_BUTTON1DOWN)) ;; touch down
      (glgui-widget-set! g wgt 'oldy my-fixed)
      (glgui-widget-set! g wgt 'fsty my-fixed)
      (glgui-widget-set! g wgt 'old #t)
      (glgui-widget-set! g wgt 'drag #f))
     ((and old (fx= type EVENT_BUTTON1UP)) ;; touch release
      (cond
       ((not inside) ;; cancel drag
        (if old (glgui-widget-set! g wgt 'old #f))
        (if drag (glgui-widget-set! g wgt 'drag #f)))
       (else
        (glgui-widget-set! g wgt 'offset (round val)) ;; snap to place?
        (if (and (not drag) inside cb)
            (let ((cur (round (+ val (/ (- (+ y h) (+ my space)) dh) -.5))))
              (if (and (>= cur 0) (>= cur val) (< cur n)) (begin
                (glgui-widget-set! g wgt 'current cur)
                (if (procedure? cb) (cb g wgt type mx my))))))
        (if old (glgui-widget-set! g wgt 'old #f))
        (if drag (glgui-widget-set! g wgt 'drag #f)))))
      (else ;; neither button1 nor motion
       (if (not inside) (begin
        (if old (glgui-widget-set! g wgt 'old #f))
        (if drag (glgui-widget-set! g wgt 'drag #f))))))
    ;; return whether or not input was applicable
    inside))

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
     'bgcol3 (color:shuffle #xb8860bff)
     'txtcol White
     'tscol White
     'deltatime 300 ;; 5min
     'group #f
     'colorpos 0
     'namecolors '()
   ))
;; eof
