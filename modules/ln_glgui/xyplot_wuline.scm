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

;; Xiaolin Wu's line algorithm
(define (texture-addpixel vec vecw x y c)
  (let ((addr (fx+ (fx* y vecw) x))
        (val (fix (fl* 255. c))))
    (if (and (fx>= addr 0) (fx< addr (u8vector-length vec)))
      (u8vector-set! vec addr val)
    )
  ))
 
(define (wuline:ipart x) (exact-floor x))
(define (wuline:round x) (exact-round x))
(define (wuline:fpart x)
  (if (fl< x 0.)
    (fl- 1. (fl- x (flfloor x)))
    (fl- x (flfloor x))
  ))
(define (wuline:rfpart x) (fl- 1. (wuline:fpart x)))

(define (texture-addline t tw x0 y0 x1 y1)
  (let ((steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
    (if steep (let ((tmp #f))
      (set! tmp x0) (set! x0 y0) (set! y0 tmp)
      (set! tmp x1) (set! x1 y1) (set! y1 tmp)
    ))
    (if (> x0 x1) (let ((tmp #f))
      (set! tmp x0) (set! x0 x1) (set! x1 tmp)
      (set! tmp y0) (set! y0 y1) (set! y1 tmp)
    ))

    (let* ((dx (- x1 x0)) (dy (- y1 y0))
           (gradient (flo (/ dy dx)))
           (intery0 #f) (xpxl1 #f) (xpxl2 #f))
      ;; handle first endpoint
      (let* ((xend (wuline:round x0))
             (yend (fl+ (flo y0) (fl* gradient (flo (fx- xend x0)))))
             (xgap (wuline:rfpart (fl+ (flo x0) 0.5)))
             (ypxl1 (wuline:ipart yend)))
        (set! xpxl1 xend)
        (if steep
          (begin
            (texture-addpixel t tw ypxl1 xpxl1 (fl* (wuline:rfpart yend) xgap))
            (texture-addpixel t tw (fx+ ypxl1 1) xpxl1 (fl* (wuline:fpart yend) xgap))
          )
          (begin
            (texture-addpixel t tw xpxl1 ypxl1 (fl* (wuline:rfpart yend) xgap))
            (texture-addpixel t tw xpxl1 (fx+ ypxl1 1) (fl* (wuline:fpart yend) xgap))
          )
        )
        (set! intery0 (fl+ yend gradient))
      )
      ;; handle second endpoint
      (let* ((xend (wuline:round x1))
             (yend (fl+ (flo y1) (fl* gradient (flo (fx- xend x1)))))
             (xgap (wuline:fpart (fl+ (flo x1) 0.5)))
             (ypxl2 (wuline:ipart yend)))
        (set! xpxl2 xend)
        (if steep
          (begin
            (texture-addpixel t tw ypxl2 xpxl2 (fl* (wuline:rfpart yend) xgap))
            (texture-addpixel t tw (fx+ ypxl2 1) xpxl2 (fl* (wuline:fpart yend) xgap))
          )
          (begin
            (texture-addpixel t tw xpxl2 ypxl2 (fl* (wuline:rfpart yend) xgap))
            (texture-addpixel t tw xpxl2 (fx+ ypxl2 1) (fl* (wuline:fpart yend) xgap))
          )
        )
      )
 
      ;; main loop
      (let loop ((x (fx+ xpxl1 1)) (intery intery0))
        (if (fx> x (fx- xpxl2 1)) #f
          (begin
            (if steep 
              (begin
                (texture-addpixel t tw (wuline:ipart intery)  x (wuline:rfpart intery))
                (texture-addpixel t tw (fx+ (wuline:ipart intery) 1) x (wuline:fpart intery))
              )
              (begin
                (texture-addpixel t tw x (wuline:ipart intery) (wuline:rfpart intery))
                (texture-addpixel t tw x (fx+ (wuline:ipart intery) 1) (wuline:fpart intery))
              )
            )
            (loop (fx+ x 1) (fl+ intery gradient))
          )
        )
      )
    )
  ))
;; eof
