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

;; font stuff

;; set font
(define (graphout:font g fname fsize)
  (let ((o (table-ref g 'output)))
    (table-set! g 'fontnum fsize)
    (table-set! g 'fsizex fsize)
    (case o
      ((GRAPH_PDF)
        (let* ((pdf (table-ref g 'hpdf))
               (page (table-ref g 'hpage))
               (font (HPDF_GetFont pdf fname "WinAnsiEncoding")))
          (table-set! g 'hfont font)
          (table-set! g 'hfontsize fsize)
          (HPDF_Page_SetFontAndSize page font (flo fsize))
        )
      )
      ((GRAPH_SVG)
        (table-set! g 'svg-font fname)
        (table-set! g 'svg-fontsize fsize)
      )
    )
  ))

;; -------- HORIZONTAL TEXT

(define (graphout:htextleft g x y text)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let* ((page (table-ref g 'hpage))
               (font (table-ref g 'hfont))
               (fontsize (table-ref g 'hfontsize))
               (ta (flo (/ (* (HPDF_Font_GetAscent font) fontsize) 1000.)))
               (td (flo (/ (* (HPDF_Font_GetDescent font) fontsize) 1000.))))
          (HPDF_Page_BeginText page)
          (HPDF_Page_TextOut page (graphout:curx->devx g x) (- (graphout:cury->devy g y) (/ (+ ta td) 2.)) text)
          (HPDF_Page_EndText page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (svg-font (table-ref g 'svg-font "Helvetica"))
               (svg-fontsize (number->string (table-ref g 'svg-fontsize 12)))
               (svg-fontcolor (table-ref g 'svg-color "#000000"))
               (sx (graphout:svgflo (graphout:curx->devx g x)))
               (h (table-ref g 'devymax))
               (sy (graphout:svgflo (- h (graphout:cury->devy g y))))
               (opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(text (@ (x ,sx) (y ,sy) (fill ,svg-fontcolor) 
                         (font-family ,svg-font)(dy "0.3em")
                         (font-size ,svg-fontsize) 
                         (opacity ,opacity)
                         (text-anchor "start")) ,text "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'HTEXTLEFT x y text))
    )
  ))

(define (graphout:htextright g x y text)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let* ((page (table-ref g 'hpage))
               (font (table-ref g 'hfont))
               (fontsize (table-ref g 'hfontsize))
               (w (HPDF_Page_TextWidth page text))
               (ta (flo (/ (* (HPDF_Font_GetAscent font) fontsize) 1000.)))
               (td (flo (/ (* (HPDF_Font_GetDescent font) fontsize) 1000.))))
          (HPDF_Page_BeginText page)
          (HPDF_Page_TextOut page (- (graphout:curx->devx g x) w) (- (graphout:cury->devy g y) (/ (+ ta td) 2.)) text)
          (HPDF_Page_EndText page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (svg-font (table-ref g 'svg-font "Helvetica"))
               (svg-fontsize (number->string (table-ref g 'svg-fontsize 12)))
               (svg-fontcolor (table-ref g 'svg-color "#000000"))
               (sx (graphout:svgflo (graphout:curx->devx g x)))
               (h (table-ref g 'devymax))
               (sy (graphout:svgflo (- h (graphout:cury->devy g y))))
               (opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(text (@ (x ,sx) (y ,sy) (fill ,svg-fontcolor)
                         (font-family ,svg-font) (dy "0.3em")
                         (font-size ,svg-fontsize)
                         (opacity ,opacity)
                         (text-anchor "end")) ,text "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'HTEXTRIGHT x y text))
    )
  ))

;; this is the general code for centered horizontal text
;; pos = 1. y centered
;; pos = 0  y bottom
;; pos = 2  y top
(define (graphout:htextcenter g x y text . xtra)
  (let ((o (table-ref g 'output))
        (yofs  (if (> (length xtra) 0) (car xtra) 1.)))
    (case o
      ((GRAPH_PDF)
        (let* ((page (table-ref g 'hpage))
               (font (table-ref g 'hfont))
               (fontsize (table-ref g 'hfontsize))
               (w (HPDF_Page_TextWidth page text))
               (ta (flo (/ (* (HPDF_Font_GetAscent font) fontsize) 1000.)))
               (td (flo (/ (* (HPDF_Font_GetDescent font) fontsize) 1000.))))
          (HPDF_Page_BeginText page)
          (HPDF_Page_TextOut page (- (graphout:curx->devx g x) (/ w 2.0)) (- (graphout:cury->devy g y) (* yofs (/ (+ ta td) 2.))) text )
          (HPDF_Page_EndText page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (svg-font (table-ref g 'svg-font "Helvetica"))
               (svg-fontsize (number->string (table-ref g 'svg-fontsize 12)))
               (svg-fontcolor (table-ref g 'svg-color "#000000"))
               (sx (graphout:svgflo (graphout:curx->devx g x)))
               (h (table-ref g 'devymax))
               (sy (graphout:svgflo (- h (graphout:cury->devy g y))))
               (opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(text (@ (x ,sx) (y ,sy) (fill ,svg-fontcolor)
                         (font-family ,svg-font)(dy "0.3em")
                         (font-size ,svg-fontsize)
                         (opacity ,opacity)
                         (text-anchor "middle")) ,text "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'HTEXTCENTER x y text))
    )
  ))

;; -------- VERTICAL TEXT

;; draw a bottom aligned vertical text
(define (graphout:vtextbottom g x y text)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let* ((page (table-ref g 'hpage))
               (font (table-ref g 'hfont))
               (fontsize (table-ref g 'hfontsize))
               (ta (flo (/ (* (HPDF_Font_GetAscent font) fontsize) 1000.)))
               (td (flo (/ (* (HPDF_Font_GetDescent font) fontsize) 1000.))))
          (HPDF_Page_GSave page)
          (HPDF_Page_Concat page 1.0 0.0 0.0 1.0 (- (graphout:curx->devx g x) (/ (+ ta td) -2.)) (graphout:cury->devy g y))
          (HPDF_Page_Concat page 0.0 1.0 -1.0 0.0  0.0 0.0)
          (HPDF_Page_BeginText page)
          (HPDF_Page_TextOut page  0.0 0.0 text)
          (HPDF_Page_EndText page)
          (HPDF_Page_GRestore page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (svg-font (table-ref g 'svg-font "Helvetica"))
               (svg-fontsize (number->string (table-ref g 'svg-fontsize 12)))
               (svg-fontcolor (table-ref g 'svg-color "#000000"))
               (sx (graphout:svgflo (graphout:curx->devx g x)))
               (h (table-ref g 'devymax))
               (sy (graphout:svgflo (- h (fix (graphout:cury->devy g y)))))
               (opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(text (@ (x ,sx) (y ,sy) (fill ,svg-fontcolor)
                         (transform ,(string-append  "rotate(-90," sx "," sy ")"))
                         (font-family ,svg-font) (dy "0.3em")
                         (font-size ,svg-fontsize)
                         (opacity ,opacity)
                         (text-anchor "start")) ,text "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'VTEXTBOTTOM x y text))
    )
  ))

;; top aligned vertical text
(define (graphout:vtexttop g x y text)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let* ((page (table-ref g 'hpage))
               (font (table-ref g 'hfont))
               (fontsize (table-ref g 'hfontsize))
               (w (HPDF_Page_TextWidth page text))
               (ta (flo (/ (* (HPDF_Font_GetAscent font) fontsize) 1000.)))
               (td (flo (/ (* (HPDF_Font_GetDescent font) fontsize) 1000.))))
          (HPDF_Page_GSave page)
          (HPDF_Page_Concat page 1.0 0.0 0.0 1.0 (- (graphout:curx->devx g x) (/ (+ ta td) -2.)) (graphout:cury->devy g y))
          (HPDF_Page_Concat page 0.0 1.0 -1.0 0.0  0.0 0.0)
          (HPDF_Page_BeginText page)
          (HPDF_Page_TextOut page  (- w) 0.0 text)
          (HPDF_Page_EndText page)
          (HPDF_Page_GRestore page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (svg-font (table-ref g 'svg-font "Helvetica"))
               (svg-fontsize (number->string (table-ref g 'svg-fontsize 12)))
               (svg-fontcolor (table-ref g 'svg-color "#000000"))
               (sx (graphout:svgflo (graphout:curx->devx g x)))
               (h (table-ref g 'devymax))
               (sy (graphout:svgflo (- h (fix (graphout:cury->devy g y)))))
               (opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(text (@ (x ,sx) (y ,sy) (fill ,svg-fontcolor)
                         (transform ,(string-append  "rotate(-90," sx "," sy ")"))
                         (font-family ,svg-font) (dy "0.3em")
                         (font-size ,svg-fontsize)
                         (opacity ,opacity)
                         (text-anchor "end")) ,text "")))
          (table-set! g 'svg (append svg (list element)))
        )
      )
      (else (graph:hook g o 'VTEXTTOP x y text))
    )
  ))

;; draw a centered vertical text 
;; phantom is for a special case needed for ylabel
(define (graphout:vtextcenter g x y text . phantom) 
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
        (let* ((page (table-ref g 'hpage))
               (font (table-ref g 'hfont))
               (fontsize (table-ref g 'hfontsize))
               (w (HPDF_Page_TextWidth page text))
               (pw (if (> (length phantom) 0) (HPDF_Page_TextWidth page (car phantom)) 0.0))
               (ta (flo (/ (* (HPDF_Font_GetAscent font) fontsize) 1000.)))
               (td (flo (/ (* (HPDF_Font_GetDescent font) fontsize) 1000.))))
          (HPDF_Page_GSave page)
          (HPDF_Page_Concat page 1.0 0.0 0.0 1.0 (- (graphout:curx->devx g x) pw (/ (+ ta td) -2.)) (graphout:cury->devy g y))
          (HPDF_Page_Concat page 0.0 1.0 -1.0 0.0  0.0 0.0)
          (HPDF_Page_BeginText page)
          (HPDF_Page_TextOut page  (- (/ w 2.)) 0.0 text)
          (HPDF_Page_EndText page)
          (HPDF_Page_GRestore page)
        )
      )
      ((GRAPH_SVG)
        (let* ((svg (table-ref g 'svg))
               (svg-font (table-ref g 'svg-font "Helvetica"))
               (svg-fontsize (number->string (table-ref g 'svg-fontsize 12)))
               (svg-fontcolor (table-ref g 'svg-color "#000000"))
               (sx (graphout:svgflo (graphout:curx->devx g x)))
               (h (table-ref g 'devymax))
               (sy (graphout:svgflo (- h (fix (graphout:cury->devy g y)))))
               (opacity (graphout:svgflo (table-ref g 'svg-opacity 1.)))
               (element `(text (@ (x ,sx) (y ,sy) (fill ,svg-fontcolor)
                         (transform ,(string-append "rotate(-90," sx "," sy ")"))
                         (font-family ,svg-font) (dy "0.3em")
                         (font-size ,svg-fontsize)
                         (opacity ,opacity)
                         (text-anchor "middle")) ,text "")))
          (table-set! g 'svg (append svg (list element)))
        )
      ) 
      (else (graph:hook g o 'VTEXTCENTER x y text))
    )
  ))

;; eof
