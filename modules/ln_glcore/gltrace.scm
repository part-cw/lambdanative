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
;; GL texture trace
;; generic framework for drawing graphs as dynamic textures

;; three different modes
(define GLTRACE_RESET 1)
(define GLTRACE_OVERWRITE 2)
(define GLTRACE_SHIFT 3)

;; draw a pixel at the specified memory address
(define (gltrace:pixel data addr alpha)
  (if (and (fx>= addr 0) (fx< addr (u8vector-length data)))
     (u8vector-set! data addr alpha)))

;; draw a antialiased line from x,y0 to x+1,y1 a la Xiaolin Wu 
(define (gltrace:wuline data w0 x y0 y1)
  (let* ((addr (fx+ (fx* y0 w0) x))
        (ymin (fxmin y0 y1))
        (ymax (fxmax y0 y1))
        (yincr (if (> y0 y1) (fx- w0) w0))
        (gradient (fl/ 1. (fl- (flo ymax) (flo ymin)))))
    (if (fx= y0 y1) (gltrace:pixel data addr 255)
    (let loop ((a addr)(y ymin)(g 0.))
      (if (fx< y ymax) (begin
          (gltrace:pixel data a (fix (fl* 255. (fl- 1. g))))
          (gltrace:pixel data (fx+ a 1) (fix (fl* 255. g)))
          (loop (fx+ a yincr) (fx+ y 1)(fl+ g gradient))))))))

(define (make-gltrace w h mode vmin vmax vlo vhi)
  (let* ((t (make-table))
         (w0 (fix (expt 2. (ceiling (/ (log w) (log 2.))))))
         (h0 (fix (expt 2. (ceiling (/ (log h) (log 2.))))))
         (wr (/ w w0 1.))
         (hr (/ h h0 1.))
         (texture (glCoreTextureCreate w0 h0 (make-u8vector (* w0 h0) 0))))
    (table-set! t 'w (fix w))
    (table-set! t 'wr wr)
    (table-set! t 'h (fix h))
    (table-set! t 'hr hr)
    (table-set! t 'w0 w0)
    (table-set! t 'h0 h0)
    (table-set! t 'vmin vmin)
    (table-set! t 'vmax vmax)
    (table-set! t 'vlo vlo)
    (table-set! t 'vhi vhi)
    (table-set! t 'texture texture)
    (table-set! t 'image (list w h texture 0. 0. wr hr))
    (table-set! t 'data (glCoreTextureData texture))
    (table-set! t 'x (if (fx= mode GLTRACE_SHIFT) (- (fix w) 1) 0))
    (table-set! t 'y #f)
    (table-set! t 'mode mode)
    (table-set! t 'values (make-f32vector w +nan.0))
    t ))

(define (gltrace:clear t)
  (u8vector-fill! (table-ref t 'data) 0)
  (f32vector-fill! (table-ref t 'values) +nan.0)
  (table-set! t 'x (if (fx= (table-ref t 'mode) GLTRACE_SHIFT) (- (table-ref t 'w) 1) 0))
  (table-set! t 'y #f))

;; erase vertical line at x
(define (gltrace:zap t x)
  (let ((w0 (table-ref t 'w0))
        (h (table-ref t 'h0))   ;;; XXX
        (data (table-ref t 'data)))
    (let loop ((y 0)) 
      (if (fx< y h) (begin
        (u8vector-set! data (fx+ x (fx* w0 y)) #x00)
        (loop (fx+ y 1)))))))

;; shift trace left
(define (gltrace:shift t)
  (let* ((w0 (table-ref t 'w0))
        (h0 (table-ref t 'h0))
     ;; 20110208: attempting to fix incomplete shift
        (h (min (+ (table-ref t 'h) 1) h0))
        (w (table-ref t 'w))
        (data (table-ref t 'data)))
   (let ((values (table-ref t 'values)))
     (subf32vector-move! values 1 w values 0))    
   (let loop ((y 0))
     (if (fx< y h) (let ((ofs (fx* w0 y)))
       (subu8vector-move! data (fx+ ofs 1) (fx+ ofs w) data ofs) ;; XXX -1
       (loop (fx+ y 1)))))
   (gltrace:zap t (fx- w 1))))

;; preparation for new x pos
(define (gltrace:advance-x t)
  (let* ((m (table-ref t 'mode))
         (w (table-ref t 'w))
         (x (table-ref t 'x))
         (newx (if (fx= x (- w 1))
           (if (fx= m GLTRACE_SHIFT) x 0) (fx+ x 1))))
  (cond
    ((fx= m GLTRACE_RESET) (if (fx= newx 0) (gltrace:clear t)))
    ((fx= m GLTRACE_OVERWRITE) (gltrace:zap t newx))
    ((fx= m GLTRACE_SHIFT) (if (fx= x (- w 1)) (gltrace:shift t)))) ;; newx
  (table-set! t 'y #f) (table-set! t 'x newx) newx))

(define (gltrace-add t val)
  (if val
     (let* ((x (table-ref t 'x))
            (vmin (table-ref t 'vmin))
            (vmax (table-ref t 'vmax))
            (sval (min 1. (max 0. (/ (- val vmin) (- vmax vmin)))))
            (h (table-ref t 'h))
            (w0 (table-ref t 'w0))
            (newy (min h (max 0 (inexact->exact (round (* h sval))))))
            (prvy (table-ref t 'y))
            (oldy (if prvy prvy newy))
            (data (table-ref t 'data))
            (newx (gltrace:advance-x t)))
       (if (and (fx> x 0) (fx>= newx x) oldy) (begin
          (gltrace:wuline data w0 (if (fx= x newx) (fx- x 1) x) oldy newy)
       ))
       (f32vector-set! (table-ref t 'values) x (flo val))
       (table-set! t 'y newy))
    (begin
      (gltrace:advance-x t)
      (f32vector-set! (table-ref t 'values) (table-ref t 'x) +nan.0)
    )
  ))

(define (gltrace-rescale t vmin vmax)
  (table-set! t 'vmin vmin)
  (table-set! t 'vmax vmax)
  (table-set! t 'vlo vmin)
  (table-set! t 'vhi vmax)
  (u8vector-fill! (table-ref t 'data) 0)
  (let loop ((x 0) (oldy 0))
    (if (fx= x (f32vector-length (table-ref t 'values)))
      #t
      (let* ((val1 (f32vector-ref (table-ref t 'values) x))
             (val (if (nan? val1) 0. val1))
             (sval (min 1. (max 0. (/ (- val vmin) (- vmax vmin)))))
             (h (table-ref t 'h))
             (w0 (table-ref t 'w0))
             (data (table-ref t 'data))
             (newy (min h (max 0 (inexact->exact (round (* h sval)))))))
        (if (not (or (fx= x 0) (nan? val1))) (gltrace:wuline data w0 x oldy newy))
        (loop (fx+ x 1) newy)
      )
    ))
)

(define (gltrace-draw t x y w h)
  (let ((wr (table-ref t 'wr))
        (hr (table-ref t 'hr))
        (texture (table-ref t 'texture)))
    (glCoreTextureDraw x y w h texture 0. 0. wr hr 0.)))

(define (gltrace-update t)
   (glCoreTextureUpdate (table-ref t 'texture)))

;; eof
