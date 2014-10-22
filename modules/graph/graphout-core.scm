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

;; graphing core primitive functions
;; these functions contains device specific declarations

;; set line dash
(define (graphout:dash g type mul)
  (let ((o (table-ref g 'output)))
    #t
  ))

;; set line width
(define (graphout:linewidth g w)
  (let ((o (table-ref g 'output)))
    (cond
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_SetLineWidth page w)
        )
      )
      ((= o GRAPH_SVG)
        (table-set! g 'svg-stroke-width w)
      )
    )))


(define (graphout:rgb->hex v)
  (let ((s (number->string (fix (* 255. v)) 16)))
    (if (fx= (string-length s) 1) (string-append "0" s) s)))

;; set pen color
(define (graphout:rgbcolor g red green blue)
  (let ((o (table-ref g 'output)))
    (cond
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_SetRGBStroke page (flo red) (flo green) (flo blue))
          (HPDF_Page_SetRGBFill page (flo red) (flo green) (flo blue))
        )
      )
     ((= o GRAPH_SVG)
       (table-set! g 'svg-color (string-append
         "#" (graphout:rgb->hex red) (graphout:rgb->hex green) (graphout:rgb->hex blue)))
     )
    )))

;; close a path
(define (graphout:closepath g)
  (let ((o (table-ref g 'output)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_ClosePath page)
        )
      )
      ((= o GRAPH_SVG) 
        (let ((svg-path (table-ref g 'svg-path "")))
          (table-set! g 'svg-path (string-append svg-path "Z"))
        )
      )
    )))

;; draw path outline
(define (graphout:stroke g)
  (let ((o (table-ref g 'output)))
    (cond 
       ((= o GRAPH_PDF)
         (let ((page (table-ref g 'hpage)))
           (HPDF_Page_Stroke page)
         )
       )
       ((= o GRAPH_SVG)
          (let ((svg (table-ref g 'svg))
                (stroke-color (table-ref g 'svg-color "#000000"))
                (stroke-width (number->string (table-ref g 'svg-stroke-width 1)))
                (path (table-ref g 'svg-path)))
            (table-set! g 'svg (append svg (list `(path (@ (d ,path) (fill "none") (stroke ,stroke-color) (stroke-width ,stroke-width)) ""))))
            (table-set! g 'svg-path "")
          )
       )
    )))

;; closepath, then stroke
(define (graphout:closepathstroke g)
  (graphout:closepath g)
  (graphout:stroke g))

;; save current state
(define (graphout:gsave g)
  (let ((o (table-ref g 'output)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_GSave page)
        )
      )
    )))

;; restore current state
(define (graphout:grestore  g)
  (let ((o (table-ref g 'output)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_GRestore page)
        )
      )
    )))

;; move pen in device coords
(define (graphout:devmove g x y)
  (let ((o (table-ref g 'output)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo x) (flo y))
        )
      )
      ((= o GRAPH_SVG)
        (let ((svg-path (table-ref g 'svg-path ""))
              (h (table-ref g 'devymax)))
          (table-set! g 'svg-path (string-append svg-path "M " (number->string (fix x)) " " (number->string (fix (- h y))) " "))
        )
      )
    )
    (table-set! g 'devxpen x)
    (table-set! g 'devypen y)
  ))

;; move the pen position in user coordinates
(define (graphout:moveto g x y)
  (graphout:devmove g (graphout:curx->devx g x) (graphout:cury->devy g y)))

;; draw in device coords
(define (graphout:devline g x y)
  (let ((o (table-ref g 'output)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_LineTo page (flo x) (flo y))
        )
      )
      ((= o GRAPH_SVG)
        (let ((svg-path (table-ref g 'svg-path ""))
              (h (table-ref g 'devymax)))
          (table-set! g 'svg-path (string-append svg-path "L " (number->string (fix x)) " " (number->string (fix (- h y))) " "))
         )
      )
    )
    (table-set! g 'devxpen x)
    (table-set! g 'devypen y)
  ))

;; draw in user coordinates
(define (graphout:lineto g x y)
  (graphout:devline g (graphout:curx->devx g x) (graphout:cury->devy g y)))

(define (graphout:line g x1 y1 x2 y2)
  (let ((o (table-ref g 'output))
        (dx1 (graphout:curx->devx g x1))
        (dy1 (graphout:cury->devy g y1))
        (dx2 (graphout:curx->devx g x2))
        (dy2 (graphout:cury->devy g y2)))
    (cond
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_Stroke page)
        )
      )
      ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
              (h (table-ref g 'devymax))
              (sx1 (number->string dx1))
              (sy1 (number->string (- h dy1)))
              (sx2 (number->string dx2))
              (sy2 (number->string (- h dy2)))
              (stroke-color (table-ref g 'svg-color "#000000"))
              (stroke-width (number->string (table-ref g 'svg-stroke-width 1))))
          (table-set! g 'svg (append svg (list
            `(line (@ (x1 ,sx1) (y1 ,sy1) (x2 ,sx2) (y2 ,sy2)
                      (style ,(string-append "stroke: " stroke-color ";"
                         "stroke-width: " stroke-width ";"))) ""))))
        )
      )
    )
  ))

(define (graphout:box g x1 y1 x2 y2)
  (let ((o (table-ref g 'output))
        (dx1 (graphout:curx->devx g x1))
        (dy1 (graphout:cury->devy g y1))
        (dx2 (graphout:curx->devx g x2))
        (dy2 (graphout:cury->devy g y2)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_LineTo page (flo dx1) (flo dy2))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (number->string (min dx1 dx2)))
               (sy (number->string (min (- h dy1) (- h dy2))))
               (sw (number->string (abs (- dx1 dx2))))
               (sh (number->string (abs (- dy1 dy2))))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (table-ref g 'svg-stroke-width 1))
               (element `(rect (@ (x ,sx) (y ,sy) (width ,sw) (height ,sh)
                                  (style ,(string-append
                                    "stroke: " stroke-color ";"
                                    "stroke-width: " (number->string stroke-width) ";"
                                    "fill: none;"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

(define (graphout:solidbox g x1 y1 x2 y2)
  (let ((o (table-ref g 'output))
        (dx1 (graphout:curx->devx g x1))
        (dy1 (graphout:cury->devy g y1))
        (dx2 (graphout:curx->devx g x2))
        (dy2 (graphout:cury->devy g y2)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_LineTo page (flo dx1) (flo dy2))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (number->string (min dx1 dx2)))
               (sy (number->string (min (- h dy1) (- h dy2))))
               (sw (number->string (abs (- dx1 dx2))))
               (sh (number->string (abs (- dy1 dy2))))
               (fill-color (table-ref g 'svg-color "#000000"))
               (fill-opacity (number->string (table-ref g 'svg-fill-opacity 1.)))
               (stroke-width (table-ref g 'svg-stroke-width 1))
               (element `(rect (@ (x ,sx) (y ,sy) (width ,sw) (height ,sh)
                                  (style ,(string-append
                                    "stroke: none;"
                                    "fill: " fill-color ";"
                                    "fill-opacity: " fill-opacity ";"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )

    )
  ))

(define (graphout:triangle g x1 y1 x2 y2 x3 y3)
  (let ((o (table-ref g 'output))
        (dx1 (graphout:curx->devx g x1))
        (dy1 (graphout:cury->devy g y1))
        (dx2 (graphout:curx->devx g x2))
        (dy2 (graphout:cury->devy g y2))
        (dx3 (graphout:curx->devx g x3))
        (dy3 (graphout:cury->devy g y3)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_LineTo page (flo dx3) (flo dy3))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx1 (number->string dx1))
               (sy1 (number->string (- h dy1)))
               (sx2 (number->string dx2))
               (sy2 (number->string (- h dy2)))
               (sx3 (number->string dx3))
               (sy3 (number->string (- h dy3)))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (table-ref g 'svg-stroke-width 1))
               (element `(polygon (@ (points ,(string-append
                           sx1 "," sy1 " " sx2 "," sy2 " " sx3 "," sy3))
                           (style ,(string-append
                             "stroke: " stroke-color ";"
                             "stroke-width: " (number->string stroke-width) ";"
                             "fill: none;"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

(define (graphout:solidtriangle g x1 y1 x2 y2 x3 y3)
  (let ((o (table-ref g 'output))
        (dx1 (graphout:curx->devx g x1))
        (dy1 (graphout:cury->devy g y1))
        (dx2 (graphout:curx->devx g x2))
        (dy2 (graphout:cury->devy g y2))
        (dx3 (graphout:curx->devx g x3))
        (dy3 (graphout:cury->devy g y3)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_LineTo page (flo dx3) (flo dy3))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx1 (number->string dx1))
               (sy1 (number->string (- h dy1)))
               (sx2 (number->string dx2))
               (sy2 (number->string (- h dy2)))
               (sx3 (number->string dx3))
               (sy3 (number->string (- h dy3)))
               (fill-color (table-ref g 'svg-color "#000000"))
               (fill-opacity (number->string (table-ref g 'svg-fill-opacity 1.)))
               (element `(polygon (@ (points ,(string-append
                           sx1 "," sy1 " " sx2 "," sy2 " " sx3 "," sy3))
                           (style ,(string-append
                             "stroke: none;"
                             "fill: " fill-color ";"
                             "fill-opacity: " fill-opacity ";"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

(define (graphout:circle g x y devradius)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_Circle page (flo dx) (flo dy) (flo devradius))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (number->string dx))
               (sy (number->string (- h dy)))
               (sr (number->string (fix devradius)))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (number->string (table-ref g 'svg-stroke-width 1)))
               (element `(circle (@ (cx ,sx) (cy ,sy) (r ,sr)
                           (stroke ,stroke-color) (stroke-width ,stroke-width) (fill "none")) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

(define (graphout:solidcircle g x y devradius)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_Circle page (flo dx) (flo dy) (flo devradius))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((= o GRAPH_SVG)
         (let* ((svg (table-ref g 'svg))
                (h (table-ref g 'devymax))
                (sx (number->string dx))
                (sy (number->string (- h dy)))
                (sr (number->string (fix devradius)))
                (fill-color (table-ref g 'svg-color "#000000"))
                (fill-opacity (number->string (table-ref g 'svg-fill-opacity 1.)))
                (element `(circle (@ (cx ,sx) (cy ,sy) (r ,sr)
                           (stroke "none") (fill ,fill-color) (fill-opacity ,fill-opacity)) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

;; MARKER CODE
;; markers are special because the size is specified in points.

(define (graphout:markercircle g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_Circle page (flo dx) (flo dy) (/ dw 2.0))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (number->string dx))
               (sy (number->string (- h dy)))
               (sr (number->string (fix dw)))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (number->string (table-ref g 'svg-stroke-width 1)))
               (element `(circle (@ (cx ,sx) (cy ,sy) (r ,sr)
                           (stroke ,stroke-color) (stroke-width ,stroke-width) (fill "none")) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

(define (graphout:markersolidcircle g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_Circle page (flo dx) (flo dy) (/ dw 2.0))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((= o GRAPH_SVG)
         (let* ((svg (table-ref g 'svg))
                (h (table-ref g 'devymax))
                (sx (number->string dx))
                (sy (number->string (- h dy)))
                (sr (number->string dw))
                (fill-color (table-ref g 'svg-color "#000000"))
                (fill-opacity (number->string (table-ref g 'svg-fill-opacity 1.)))
                (element `(circle (@ (cx ,sx) (cy ,sy) (r ,sr)
                           (stroke "none") (fill ,fill-color) (fill-opacity ,fill-opacity)) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

(define (graphout:markerbox g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (+ dx (/ dw 2.)) (+ dy (/ dw 2.)))
          (HPDF_Page_LineTo page (+ dx (/ dw 2.)) (- dy (/ dw 2.)))
          (HPDF_Page_LineTo page (- dx (/ dw 2.)) (- dy (/ dw 2.)))
          (HPDF_Page_LineTo page (- dx (/ dw 2.)) (+ dy (/ dw 2.)))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (number->string dx))
               (sy (number->string (- h dy)))
               (sw (number->string dw))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (table-ref g 'svg-stroke-width 1))
               (element `(rect (@ (x ,sx) (y ,sy) (width ,sw) (height ,sw)
                                  (style ,(string-append
                                    "stroke: " stroke-color ";"
                                    "stroke-width: " (number->string stroke-width) ";"
                                    "fill: none;"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

(define (graphout:markersolidbox g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (+ dx (/ dw 2.)) (+ dy (/ dw 2.)))
          (HPDF_Page_LineTo page (+ dx (/ dw 2.)) (- dy (/ dw 2.)))
          (HPDF_Page_LineTo page (- dx (/ dw 2.)) (- dy (/ dw 2.)))
          (HPDF_Page_LineTo page (- dx (/ dw 2.)) (+ dy (/ dw 2.)))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
     ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (number->string dx))
               (sy (number->string (- h dy)))
               (sw (number->string dw))
               (fill-color (table-ref g 'svg-color "#000000"))
               (fill-opacity (number->string (table-ref g 'svg-fill-opacity 1.)))
               (stroke-width (table-ref g 'svg-stroke-width 1))
               (element `(rect (@ (x ,sx) (y ,sy) (width ,sw) (height ,sw)
                                  (style ,(string-append
                                    "stroke: none;"
                                    "fill: " fill-color ";"
                                    "fill-opacity: " fill-opacity ";"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

(define (graphout:markertriangle g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx)  (+ dy (* dw 0.667)))
          (HPDF_Page_LineTo page (+ dx (/ dw 1.7321)) (- dy (* dw 0.333)))
          (HPDF_Page_LineTo page (- dx (/ dw 1.7321)) (- dy (* dw 0.333)))
          (HPDF_Page_ClosePathStroke page)
        )
      )
     ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx1 (number->string dx))
               (sy1 (number->string (- h (+ dy (* dw 0.667)))))
               (sx2 (number->string (+ dx (/ dw 1.7321))))
               (sy2 (number->string (- h (- dy (* dw 0.333)))))
               (sx3 (number->string (- dx (/ dw 1.7321))))
               (sy3 (number->string (- h (- dy (* dw 0.333)))))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (table-ref g 'svg-stroke-width 1))
               (element `(polygon (@ (points ,(string-append
                           sx1 "," sy1 " " sx2 "," sy2 " " sx3 "," sy3))
                           (style ,(string-append
                             "stroke: " stroke-color ";"
                             "stroke-width: " (number->string stroke-width) ";"
                             "fill: none;"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
  ))

(define (graphout:markersolidtriangle g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (cond 
      ((= o GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx)  (+ dy (* dw 0.667)))
          (HPDF_Page_LineTo page (+ dx (/ dw 1.7321)) (- dy (* dw 0.333)))
          (HPDF_Page_LineTo page (- dx (/ dw 1.7321)) (- dy (* dw 0.333)))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((= o GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx1 (number->string dx))
               (sy1 (number->string (- h (+ dy (* dw 0.667)))))
               (sx2 (number->string (+ dx (/ dw 1.7321))))
               (sy2 (number->string (- h (- dy (* dw 0.333)))))
               (sx3 (number->string (- dx (/ dw 1.7321))))
               (sy3 (number->string (- h (- dy (* dw 0.333)))))
               (fill-color (table-ref g 'svg-color "#000000"))
               (fill-opacity (number->string (table-ref g 'svg-fill-opacity 1.)))
               (element `(polygon (@ (points ,(string-append
                           sx1 "," sy1 " " sx2 "," sy2 " " sx3 "," sy3))
                           (style ,(string-append
                             "stroke: none;"
                             "fill: " fill-color ";"
                             "fill-opacity: " fill-opacity ";"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
    )
))

;; eof
