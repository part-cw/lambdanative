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
  (graph:log 3 "graphout:dash " g " " type " " mul)
  (let ((o (table-ref g 'output)))
    (case o
      (else (graph:hook g o 'DASH type mul))
    )
  ))

;; set line width
(define (graphout:linewidth g w)
  (graph:log 3 "graphout:linewidth " g " " w)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_SetLineWidth page w)
        )
      )
      ((GRAPH_SVG)
        (table-set! g 'svg-stroke-width w)
      )
      (else (graph:hook g o 'LINEWIDTH w))
    )))

(define (graphout:svgflo n) (float->zeropaddedstring (flo n) 1))
(define (graphout:svgfix n) (number->string (fix n)))

(define (graphout:rgb->hex v)
  (let ((s (number->string (fix (* 255. v)) 16)))
    (if (fx= (string-length s) 1) (string-append "0" s) s)))

;; set pen color
(define (graphout:rgbcolor g red green blue alpha)
  (graph:log 3 "graphout:rgbcolor " g " " red " " green " " blue " " alpha)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_SetRGBStroke page (flo red) (flo green) (flo blue))
          (HPDF_Page_SetRGBFill page (flo red) (flo green) (flo blue))
        )
      )
     ((GRAPH_SVG)
       (table-set! g 'svg-color (string-append
         "#" (graphout:rgb->hex red) (graphout:rgb->hex green) (graphout:rgb->hex blue)))
       (table-set! g 'svg-opacity alpha)
     )
     (else (graph:hook g o 'RGBCOLOR red green blue alpha))
   )))

;; close a path
(define (graphout:closepath g)
  (graph:log 3 "graphout:closepath " g)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_ClosePath page)
        )
      )
      ((GRAPH_SVG) 
        (let ((svg-path (table-ref g 'svg-path "")))
          (table-set! g 'svg-path (string-append svg-path "Z"))
        )
      )
     (else (graph:hook g o 'CLOSEPATH))
    )))

;; draw path outline
(define (graphout:stroke g)
  (graph:log 3 "graphout:stroke " g)
  (let ((o (table-ref g 'output)))
    (case o
       ((GRAPH_PDF)
         (let ((page (table-ref g 'hpage)))
           (HPDF_Page_Stroke page)
         )
       )
       ((GRAPH_SVG)
          (let ((svg (table-ref g 'svg))
                (stroke-color (table-ref g 'svg-color "#000000"))
                (stroke-width (graphout:svgflo (table-ref g 'svg-stroke-width 1)))
                (stroke-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
                (path (table-ref g 'svg-path)))
            (table-set! g 'svg (append svg (list `(path (@ (d ,path) (fill "none") (stroke ,stroke-color) 
              (stroke-opacity ,stroke-opacity) (stroke-width ,stroke-width)) ""))))
            (table-set! g 'svg-path "")
          )
       )
       (else (graph:hook g o 'STROKE))
    )))

;; fill path
(define (graphout:fill g)
  (graph:log 3 "graphout:fill " g)
  (let ((o (table-ref g 'output)))
    (case o
       ((GRAPH_PDF)
         (let ((page (table-ref g 'hpage)))
           (HPDF_Page_Fill page)
         )
       )
       ((GRAPH_SVG)
          (let ((svg (table-ref g 'svg))
                (fill-color  (table-ref g 'svg-color "#000000"))
                (fill-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
                (path (table-ref g 'svg-path)))
            (table-set! g 'svg (append svg (list `(path (@ (d ,path) 
              (fill ,fill-color) (fill-opacity ,fill-opacity) (stroke "none")
              ) ""))))
            (table-set! g 'svg-path "")
          )
       )
       (else (graph:hook g o 'STROKE))
    )))

;; closepath, then stroke
(define (graphout:closepathstroke g)
 (graph:log 3 "graphout:closepathstroke " g)
  (graphout:closepath g)
  (graphout:stroke g))

;; closepath, then fill
(define (graphout:closepathfill g)
 (graph:log 3 "graphout:closepathfill " g)
  (graphout:closepath g)
  (graphout:fill g))

;; save current state
(define (graphout:gsave g)
  (graph:log 3 "graphout:gsave " g)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_GSave page)
        )
      )
      (else (graph:hook g o 'GSAVE))
    )))

;; restore current state
(define (graphout:grestore  g)
  (graph:log 3 "graphout:grestore " g)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_GRestore page)
        )
      )
      (else (graph:hook g o 'GRESTORE))
    )))

;; move pen in device coords
(define (graphout:devmove g x y)
  (graph:log 3 "graphout:devmove " g " " x " " y)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo x) (flo y))
        )
      )
      ((GRAPH_SVG)
        (let ((svg-path (table-ref g 'svg-path ""))
              (h (table-ref g 'devymax)))
          (table-set! g 'svg-path (string-append svg-path "M " (graphout:svgflo x) " " (graphout:svgflo (- h y)) " "))
        )
      )
      (else (graph:hook g o 'DEVMOVE x y))
    )
    (table-set! g 'devxpen x)
    (table-set! g 'devypen y)
  ))

;; move the pen position in user coordinates
(define (graphout:moveto g x y)
  (graphout:devmove g (graphout:curx->devx g x) (graphout:cury->devy g y)))

;; draw in device coords
(define (graphout:devline g x y)
  (graph:log 3 "graphout:devline " g " " x " " y)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_LineTo page (flo x) (flo y))
        )
      )
      ((GRAPH_SVG)
        (let ((svg-path (table-ref g 'svg-path ""))
              (h (table-ref g 'devymax)))
          (table-set! g 'svg-path (string-append svg-path "L " (graphout:svgflo x) " " (graphout:svgflo (- h y)) " "))
         )
      )
      (else (graph:hook g o 'DEVLINE x y))
    )
    (table-set! g 'devxpen x)
    (table-set! g 'devypen y)
  ))

;; draw in user coordinates
(define (graphout:lineto g x y)
  (graphout:devline g (graphout:curx->devx g x) (graphout:cury->devy g y)))

(define (graphout:line g x1 y1 x2 y2)
  (graph:log 3 "graphout:line " g " " x1 " " y1 " " x2 " " y2)
  (let ((o (table-ref g 'output))
        (dx1 (graphout:curx->devx g x1))
        (dy1 (graphout:cury->devy g y1))
        (dx2 (graphout:curx->devx g x2))
        (dy2 (graphout:cury->devy g y2)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_Stroke page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
              (h (table-ref g 'devymax))
              (sx1 (graphout:svgflo dx1))
              (sy1 (graphout:svgflo (- h dy1)))
              (sx2 (graphout:svgflo dx2))
              (sy2 (graphout:svgflo (- h dy2)))
              (stroke-color (table-ref g 'svg-color "#000000"))
              (stroke-width (graphout:svgflo (table-ref g 'svg-stroke-width 1)))
              (stroke-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.))))
          (table-set! g 'svg (append svg (list
            `(line (@ (x1 ,sx1) (y1 ,sy1) (x2 ,sx2) (y2 ,sy2)
                      (style ,(string-append "stroke: " stroke-color ";"
                        "stroke-opacity: " stroke-opacity ";"
                         "stroke-width: " stroke-width ";"))) ""))))
        )
      )
      (else (graph:hook g o 'LINE x1 y1 x2 y2))
    )
  ))

(define (graphout:box g x1 y1 x2 y2)
  (graph:log 3 "graphout:box " g " " x1 " " y1 " " x2 " " y2)
  (let ((o (table-ref g 'output))
        (dx1 (graphout:curx->devx g x1))
        (dy1 (graphout:cury->devy g y1))
        (dx2 (graphout:curx->devx g x2))
        (dy2 (graphout:cury->devy g y2)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_LineTo page (flo dx1) (flo dy2))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (graphout:svgflo (min dx1 dx2)))
               (sy (graphout:svgflo (min (- h dy1) (- h dy2))))
               (sw (graphout:svgflo (abs (- dx1 dx2))))
               (sh (graphout:svgflo (abs (- dy1 dy2))))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (graphout:svgflo (table-ref g 'svg-stroke-width 1)))
               (stroke-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(rect (@ (x ,sx) (y ,sy) (width ,sw) (height ,sh)
                                  (style ,(string-append
                                    "stroke: " stroke-color ";"
                                    "stroke-width: " stroke-width ";"
                                    "stroke-opacity: " stroke-opacity ";"
                                    "fill: none;"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'BOX x1 y1 x2 y2))
    )
  ))

(define (graphout:solidbox g x1 y1 x2 y2)
  (let ((o (table-ref g 'output))
        (dx1 (graphout:curx->devx g x1))
        (dy1 (graphout:cury->devy g y1))
        (dx2 (graphout:curx->devx g x2))
        (dy2 (graphout:cury->devy g y2)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_LineTo page (flo dx1) (flo dy2))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (graphout:svgflo (min dx1 dx2)))
               (sy (graphout:svgflo (min (- h dy1) (- h dy2))))
               (sw (graphout:svgflo (abs (- dx1 dx2))))
               (sh (graphout:svgflo (abs (- dy1 dy2))))
               (fill-color (table-ref g 'svg-color "#000000"))
               (fill-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(rect (@ (x ,sx) (y ,sy) (width ,sw) (height ,sh)
                                  (style ,(string-append
                                    "stroke: none;"
                                    "fill: " fill-color ";"
                                    "fill-opacity: " fill-opacity ";"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'SOLIDBOX x1 y1 x2 y2))
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
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_LineTo page (flo dx3) (flo dy3))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx1 (graphout:svgflo dx1))
               (sy1 (graphout:svgflo (- h dy1)))
               (sx2 (graphout:svgflo dx2))
               (sy2 (graphout:svgflo (- h dy2)))
               (sx3 (graphout:svgflo dx3))
               (sy3 (graphout:svgflo (- h dy3)))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (graphout:svgflo (table-ref g 'svg-stroke-width 1)))
               (stroke-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(polygon (@ (points ,(string-append
                           sx1 "," sy1 " " sx2 "," sy2 " " sx3 "," sy3))
                           (style ,(string-append
                             "stroke: " stroke-color ";"
                             "stroke-width: " stroke-width ";"
                             "stroke-opacity: " stroke-opacity ";"
                             "fill: none;"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'TRIANGLE x1 y1 x2 y2 x3 y3))
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
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx1) (flo dy1))
          (HPDF_Page_LineTo page (flo dx2) (flo dy2))
          (HPDF_Page_LineTo page (flo dx3) (flo dy3))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx1 (graphout:svgflo dx1))
               (sy1 (graphout:svgflo (- h dy1)))
               (sx2 (graphout:svgflo dx2))
               (sy2 (graphout:svgflo (- h dy2)))
               (sx3 (graphout:svgflo dx3))
               (sy3 (graphout:svgflo (- h dy3)))
               (fill-color (table-ref g 'svg-color "#000000"))
               (fill-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(polygon (@ (points ,(string-append
                           sx1 "," sy1 " " sx2 "," sy2 " " sx3 "," sy3))
                           (style ,(string-append
                             "stroke: none;"
                             "fill: " fill-color ";"
                             "fill-opacity: " fill-opacity ";"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'SOLIDTRIANGLE x1 y1 x2 y2 x3 y3))
    )
  ))

(define (graphout:circle g x y devradius)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_Circle page (flo dx) (flo dy) (flo devradius))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (graphout:svgflo dx))
               (sy (graphout:svgflo (- h dy)))
               (sr (graphout:svgflo devradius))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (graphout:svgflo (table-ref g 'svg-stroke-width 1)))
               (stroke-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(circle (@ (cx ,sx) (cy ,sy) (r ,sr)
                           (stroke ,stroke-color) (stroke-width ,stroke-width) (stroke-opacity ,stroke-opacity) (fill "none")) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'CIRCLE x y devradius))
    )
  ))

(define (graphout:solidcircle g x y devradius)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_Circle page (flo dx) (flo dy) (flo devradius))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((GRAPH_SVG)
         (let* ((svg (table-ref g 'svg))
                (h (table-ref g 'devymax))
                (sx (graphout:svgflo dx))
                (sy (graphout:svgflo (- h dy)))
                (sr (graphout:svgflo devradius))
                (fill-color (table-ref g 'svg-color "#000000"))
                (fill-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
                (element `(circle (@ (cx ,sx) (cy ,sy) (r ,sr)
                           (stroke "none") (fill ,fill-color) (fill-opacity ,fill-opacity)) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'SOLIDCIRCLE x y devradius))
    )
  ))

;; MARKER CODE
;; markers are special because the size is specified in points.

(define (graphout:markercircle g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_Circle page (flo dx) (flo dy) (/ dw 2.0))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (graphout:svgflo dx))
               (sy (graphout:svgflo (- h dy)))
               (sr (graphout:svgflo dw))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (graphout:svgflo (table-ref g 'svg-stroke-width 1)))
               (stroke-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(circle (@ (cx ,sx) (cy ,sy) (r ,sr)
                           (stroke ,stroke-color) (stroke-width ,stroke-width) (stroke-opacity ,stroke-opacity) (fill "none")) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'MARKERCIRCLE x y dw))
    )
  ))

(define (graphout:markersolidcircle g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_Circle page (flo dx) (flo dy) (/ dw 2.0))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((GRAPH_SVG)
         (let* ((svg (table-ref g 'svg))
                (h (table-ref g 'devymax))
                (sx (graphout:svgflo dx))
                (sy (graphout:svgflo (- h dy)))
                (sr (graphout:svgflo dw))
                (fill-color (table-ref g 'svg-color "#000000"))
                (fill-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
                (element `(circle (@ (cx ,sx) (cy ,sy) (r ,sr)
                           (stroke "none") (fill ,fill-color) (fill-opacity ,fill-opacity)) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'MARKERCIRCLE x y dw))
    )
  ))

(define (graphout:markerbox g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (+ dx (/ dw 2.)) (+ dy (/ dw 2.)))
          (HPDF_Page_LineTo page (+ dx (/ dw 2.)) (- dy (/ dw 2.)))
          (HPDF_Page_LineTo page (- dx (/ dw 2.)) (- dy (/ dw 2.)))
          (HPDF_Page_LineTo page (- dx (/ dw 2.)) (+ dy (/ dw 2.)))
          (HPDF_Page_ClosePathStroke page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (graphout:svgflo dx))
               (sy (graphout:svgflo (- h dy)))
               (sw (graphout:svgflo dw))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (graphout:svgflo (table-ref g 'svg-stroke-width 1)))
               (stroke-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(rect (@ (x ,sx) (y ,sy) (width ,sw) (height ,sw)
                                  (style ,(string-append
                                    "stroke: " stroke-color ";"
                                    "stroke-width: " stroke-width ";"
                                    "stroke-opacity: " stroke-opacity ";"
                                    "fill: none;"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'MARKERBOX x y dw))
    )
  ))

(define (graphout:markersolidbox g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (+ dx (/ dw 2.)) (+ dy (/ dw 2.)))
          (HPDF_Page_LineTo page (+ dx (/ dw 2.)) (- dy (/ dw 2.)))
          (HPDF_Page_LineTo page (- dx (/ dw 2.)) (- dy (/ dw 2.)))
          (HPDF_Page_LineTo page (- dx (/ dw 2.)) (+ dy (/ dw 2.)))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
     ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx (graphout:svgflo dx))
               (sy (graphout:svgflo (- h dy)))
               (sw (graphout:svgflo dw))
               (fill-color (table-ref g 'svg-color "#000000"))
               (fill-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (stroke-width (table-ref g 'svg-stroke-width 1))
               (element `(rect (@ (x ,sx) (y ,sy) (width ,sw) (height ,sw)
                                  (style ,(string-append
                                    "stroke: none;"
                                    "fill: " fill-color ";"
                                    "fill-opacity: " fill-opacity ";"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'MARKERSOLIDBOX x y dw))
    )
  ))

(define (graphout:markertriangle g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx)  (+ dy (* dw 0.667)))
          (HPDF_Page_LineTo page (+ dx (/ dw 1.7321)) (- dy (* dw 0.333)))
          (HPDF_Page_LineTo page (- dx (/ dw 1.7321)) (- dy (* dw 0.333)))
          (HPDF_Page_ClosePathStroke page)
        )
      )
     ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx1 (graphout:svgflo dx))
               (sy1 (graphout:svgflo (- h (+ dy (* dw 0.667)))))
               (sx2 (graphout:svgflo (+ dx (/ dw 1.7321))))
               (sy2 (graphout:svgflo (- h (- dy (* dw 0.333)))))
               (sx3 (graphout:svgflo (- dx (/ dw 1.7321))))
               (sy3 (graphout:svgflo (- h (- dy (* dw 0.333)))))
               (stroke-color (table-ref g 'svg-color "#000000"))
               (stroke-width (graphout:svgflo (table-ref g 'svg-stroke-width 1)))
               (stroke-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(polygon (@ (points ,(string-append
                           sx1 "," sy1 " " sx2 "," sy2 " " sx3 "," sy3))
                           (style ,(string-append
                             "stroke: " stroke-color ";"
                             "stroke-width: " stroke-width ";"
                             "stroke-opacity: " stroke-opacity ";"
                             "fill: none;"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'MARKERTRIANGLE x y dw))
    )
  ))

(define (graphout:markersolidtriangle g x y dw)
  (let ((o (table-ref g 'output))
        (dx (graphout:curx->devx g x))
        (dy (graphout:cury->devy g y)))
    (case o
      ((GRAPH_PDF)
        (let ((page (table-ref g 'hpage)))
          (HPDF_Page_MoveTo page (flo dx)  (+ dy (* dw 0.667)))
          (HPDF_Page_LineTo page (+ dx (/ dw 1.7321)) (- dy (* dw 0.333)))
          (HPDF_Page_LineTo page (- dx (/ dw 1.7321)) (- dy (* dw 0.333)))
          (HPDF_Page_ClosePath page)
          (HPDF_Page_Fill page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (h (table-ref g 'devymax))
               (sx1 (graphout:svgflo dx))
               (sy1 (graphout:svgflo (- h (+ dy (* dw 0.667)))))
               (sx2 (graphout:svgflo (+ dx (/ dw 1.7321))))
               (sy2 (graphout:svgflo (- h (- dy (* dw 0.333)))))
               (sx3 (graphout:svgflo (- dx (/ dw 1.7321))))
               (sy3 (graphout:svgflo (- h (- dy (* dw 0.333)))))
               (fill-color (table-ref g 'svg-color "#000000"))
               (fill-opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(polygon (@ (points ,(string-append
                           sx1 "," sy1 " " sx2 "," sy2 " " sx3 "," sy3))
                           (style ,(string-append
                             "stroke: none;"
                             "fill: " fill-color ";"
                             "fill-opacity: " fill-opacity ";"))) "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'MARKERSOLIDTRIANGLE x y dw))
    )
  ))

;; eof
