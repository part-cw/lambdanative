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

;; XY plot widget
(include "xyplot_wuline.scm")

(define (glgui:plot-make-textures g wgt)
  (let* ((x (glgui-widget-get-dyn g wgt 'xval))
         (y (glgui-widget-get-dyn g wgt 'yval))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (ylen (length y))
         (stacked? (and (fx> ylen 0) (list? (car y))))
         (lines? (glgui-widget-get-dyn g wgt 'lines?))
         (axis (glgui-widget-get-dyn g wgt 'axis)))
    (if stacked?
      (let loop ((i 0) (ret (list)))
        (if (fx= i ylen) 
          ret
          (let ((x0 (if (pair? x) (list-ref x i) x)))
            (loop (fx+ i 1) (append ret (list
              (if (if (pair? lines?) (list-ref lines? i) lines?)
                (glgui:plot-makeline x0 (list-ref y i) w h axis)
                (glgui:plot-makexy x0 (list-ref y i) w h axis)
              )
            )))
          )
        )
      )
      (if lines?
        (list (glgui:plot-makeline x y w h axis))
        (list (glgui:plot-makexy x y w h axis))
      )
    )
  ))


(define (glgui:plot-makeline xl yl w h axis)
  (let* ((x (list->vector xl))
         (y (list->vector yl))
         (ylen (vector-length y))
         (xmin (if axis (car axis) (list-min xl)))
         (xmax (if axis (cadr axis) (list-max xl)))
         (ymin (if axis (caddr axis) (list-min yl)))
         (ymax (if axis (cadddr axis) (list-max yl)))
         (xr (* 1.01 (- xmax xmin)))
         (yr (* 1.01 (- ymax ymin)))
         (w0 (fix (expt 2. (ceiling (/ (log w) (log 2.))))))
         (h0 (fix (expt 2. (ceiling (/ (log h) (log 2.))))))
         (data (make-u8vector (* w0 h0))))
    (let loop ((i 1))
      (if (fx>= i ylen)
        (list w h (glCoreTextureCreate w0 h0 data) 0. 0. (/ w w0) (/ h h0))
        (let* ((x0 (vector-ref x (fx- i 1)))
               (y0 (vector-ref y (fx- i 1)))
               (xs0 (exact-round (* (/ (- x0 xmin) xr) w)))
               (ys0 (exact-round (* (/ (- y0 ymin) yr) h)))
               (x1 (vector-ref x i))
               (y1 (vector-ref y i))
               (xs1 (exact-round (* (/ (- x1 xmin) xr) w)))
               (ys1 (exact-round (* (/ (- y1 ymin) yr) h))))
          (texture-addline data w0 xs0 ys0 xs1 ys1)
          (texture-addpixel data w0 xs0 ys0 1.)
          (texture-addpixel data w0 xs1 ys1 1.)
          (loop (fx+ i 1))
        )
      )
    )
  ))

(define (glgui:plot-makexy xl yl w h axis)
  (let* ((x (list->vector xl))
         (y (list->vector yl))
         (ylen (vector-length y))
         (xmin (if axis (car axis) (list-min xl)))
         (xmax (if axis (cadr axis) (list-max xl)))
         (ymin (if axis (caddr axis) (list-min yl)))
         (ymax (if axis (cadddr axis) (list-max yl)))
         (xr (* 1.01 (- xmax xmin)))
         (yr (* 1.01 (- ymax ymin)))
         (w0 (fix (expt 2. (ceiling (/ (log w) (log 2.))))))
         (h0 (fix (expt 2. (ceiling (/ (log h) (log 2.))))))
         (data (make-u8vector (* w0 h0))))
    (let loop ((i 0))
      (if (fx= i ylen)
        (list w h (glCoreTextureCreate w0 h0 data) 0. 0. (/ w w0) (/ h h0))
        (let* ((x0 (vector-ref x i))
               (y0 (vector-ref y i))
               (xs (exact-round (* (/ (- x0 xmin) xr) w)))
               (ys (exact-round (* (/ (- y0 ymin) yr) h))))
          (texture-addpixel data w0 xs ys 1.)
          (texture-addpixel data w0 (fx+ xs 1) ys 0.5)
          (texture-addpixel data w0 (fx- xs 1) ys 0.5)
          (texture-addpixel data w0 xs (fx+ ys 1) 0.5)
          (texture-addpixel data w0 xs (fx- ys 1) 0.5)
          (loop (fx+ i 1))
        )
      )
    )
  ))

(define (glgui:plot-draw g wgt)
  (if (glgui-widget-get-dyn g wgt 'new-texture?) (begin
    (glgui-widget-set! g wgt 'textures (glgui:plot-make-textures g wgt))
    (glgui-widget-set! g wgt 'new-texture? #f)
  ))
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (c (glgui-widget-get-dyn g wgt 'color))
         (ax (glgui-widget-get-dyn g wgt 'axis))
         (axc (glgui-widget-get-dyn g wgt 'axiscolor))
         (t (glgui-widget-get g wgt 'textures)))
    (if ax
      (let ((yl (+ y (* h (/ (- 0 (caddr ax)) (- (cadddr ax) (caddr ax))))))
            (xl (+ x (* w (/ (- 0 (car ax)) (- (cadr ax) (car ax)))))))
        (glgui:draw-box x yl w 1 axc)
        (glgui:draw-box xl y 1 h axc)
      ))
    (let loop ((i 0))
      (if (or (not (pair? t)) (fx= i (length t))) #t
        (let ((cl (if (pair? c) (list-ref c i) c)))
          (glCoreColor cl)
          (apply glCoreTextureDraw (append (list x y w h) (cddr (list-ref t i)) (list 0.)))
          (loop (fx+ i 1))
        )
      )
    )
  ))

(define (glgui:plot-update g wgt id val)
  (if (or (eq? id 'xval) (eq? id 'yval) (eq? id 'axis))
    (glgui-widget-set! g wgt 'new-texture? #t)
  ))

(define (glgui:plot-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (armed (glgui-widget-get g wgt 'armed))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (cond
       ((and (procedure? cb) inside (= type EVENT_BUTTON1DOWN))
          (glgui-widget-set! g wgt 'armed #t))
       ((and armed (procedure? cb) inside (= type EVENT_BUTTON1UP))
          (glgui-widget-set! g wgt 'armed #f)
          (cb g wgt type mx my)))
  inside
))

(define (glgui-plot g x y w h xval yval color lines? . axis)
  (glgui-widget-add g
    'x x
    'y y
    'w w
    'h h
    'callback #f
    'color color
    'xval xval
    'yval yval
    'lines? lines?
    'axis (if (fx= (length axis) 1) (car axis) #f)
    'axiscolor (color-fade White 0.25)
    'hidden #f
    'draw-handle glgui:plot-draw
    'input-handle glgui:plot-input
    'update-handle glgui:plot-update
    'new-texture? #t
  )
)

(define (glgui-lineplot g x y w h xval yval color . axis)
  (if (fx= (length axis) 1)
    (glgui-plot g x y w h xval yval color #t (car axis))
    (glgui-plot g x y w h xval yval color #t)
  )
)
(define (glgui-scatterplot g x y w h xval yval color . axis)
  (if (fx= (length axis) 1)
    (glgui-plot g x y w h xval yval color #f (car axis))
    (glgui-plot g x y w h xval yval color #f)
  )
)
;; eof
