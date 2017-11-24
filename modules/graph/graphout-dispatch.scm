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

;; this is the main dispatch for all devices
(define (graphout:dispatch g)
  (graph:log 3 "graphout:dispatch " g)
  (for-each (lambda (cmd)
    (if cmd
      (let ((cmdname (car cmd))(cmdargs (cdr cmd)))
        (cond 
          ((eq? cmdname '@axisenable) (apply graphout:axisenable (append (list g) cmdargs)))
          ((eq? cmdname '@aorigin) (apply graphout:aorigin (append (list g) cmdargs)))
          ((eq? cmdname '@box) (apply graphout:box (append (list g) cmdargs)))
          ((eq? cmdname '@circle) (apply graphout:circle (append (list g) cmdargs)))
          ((eq? cmdname '@closepathstroke) (apply graphout:closepathstroke (append (list g) cmdargs)))
          ((eq? cmdname '@closepathfill) (apply graphout:closepathfill (append (list g) cmdargs)))
          ((eq? cmdname '@color) (apply graphout:color (append (list g) cmdargs)))
          ((eq? cmdname '@dash) (apply graphout:dash (append (list g) cmdargs)))
          ((eq? cmdname '@font) (apply graphout:font (append (list g) cmdargs)))
          ((eq? cmdname '@frame) (apply graphout:frame (append (list g) cmdargs)))
          ((eq? cmdname '@htextcenter) (apply graphout:htextcenter (append (list g) cmdargs)))
          ((eq? cmdname '@htextleft) (apply graphout:htextleft (append (list g) cmdargs)))
          ((eq? cmdname '@htextright) (apply graphout:htextright (append (list g) cmdargs)))
          ((eq? cmdname '@line) (apply graphout:line (append (list g) cmdargs)))
          ((eq? cmdname '@linearstyle) (apply graphout:linearstyle (append (list g) cmdargs)))
          ((eq? cmdname '@lineto) (apply graphout:lineto (append (list g) cmdargs)))
          ((eq? cmdname '@linewidth) (apply graphout:linewidth (append (list g) cmdargs)))
          ((eq? cmdname '@logstyle) (apply graphout:logstyle (append (list g) cmdargs)))
          ((eq? cmdname '@marker) (apply graphout:marker (append (list g) cmdargs)))
          ((eq? cmdname '@mesh) (apply graphout:mesh (append (list g) cmdargs)))
          ((eq? cmdname '@moveto)  (apply graphout:moveto (append (list g) cmdargs)))
          ((eq? cmdname '@rgbcolor) (apply graphout:rgbcolor (append (list g) cmdargs)))
          ((eq? cmdname '@rlineto) (apply graphout:rlineto (append (list g) cmdargs)))
          ((eq? cmdname '@rmoveto)  (apply graphout:rmoveto (append (list g) cmdargs)))
          ((eq? cmdname '@rorigin) (apply graphout:rorigin (append (list g) cmdargs)))
          ((eq? cmdname '@setcoord) (apply graphout:setcoord (append (list g) cmdargs)))
          ((eq? cmdname '@solidbox) (apply graphout:solidbox (append (list g) cmdargs)))
          ((eq? cmdname '@solidcircle) (apply graphout:solidcircle (append (list g) cmdargs)))
          ((eq? cmdname '@solidtriangle) (apply graphout:solidtriangle (append (list g) cmdargs)))
          ((eq? cmdname '@stroke) (apply graphout:stroke (append (list g) cmdargs)))
          ((eq? cmdname '@fill) (apply graphout:fill (append (list g) cmdargs)))
          ((eq? cmdname '@tictime) (apply graphout:tictime (append (list g) cmdargs)))
          ((eq? cmdname '@triangle) (apply graphout:triangle (append (list g) cmdargs)))
          ((eq? cmdname '@vtextbottom) (apply graphout:vtextbottom (append (list g) cmdargs)))
          ((eq? cmdname '@vtextcenter) (apply graphout:vtextcenter (append (list g) cmdargs)))
          ((eq? cmdname '@vtexttop) (apply graphout:vtexttop (append (list g) cmdargs)))
          ((eq? cmdname '@xaxis) (apply graphout:xaxis (append (list g) cmdargs)))
          ((eq? cmdname '@xlabel) (apply graphout:xlabel (append (list g) cmdargs)))
          ((eq? cmdname '@xlinear) (apply graphout:xlinear (append (list g) cmdargs)))
          ((eq? cmdname '@xlog) (apply graphout:xlog (append (list g) cmdargs)))
          ((eq? cmdname '@yaxis) (apply graphout:yaxis (append (list g) cmdargs)))
          ((eq? cmdname '@ylabel) (apply graphout:ylabel (append (list g) cmdargs)))
          ((eq? cmdname '@ylinear) (apply graphout:ylinear (append (list g) cmdargs)))
          ((eq? cmdname '@ylog) (apply graphout:ylog (append (list g) cmdargs)))
          ((eq? cmdname '@newpage) (apply graphout:newpage (append (list g) cmdargs)))
          (else  (log-error (car cmd) " is not implemented\n")
        ))
    ))) (vector->list (table-ref g 'commands))
  ))

;; newpage for multi-page pdf output

(define (graphout:newpage g)
  (graph:log 3 "graphout:newpage " g)
  (let ((o (table-ref g 'output)))
    (case o
      ((GRAPH_PDF)
         (let* ((pdf (table-ref g 'hpdf))
                (w (table-ref g 'devxmax))
                (h (table-ref g 'devymax))
                (fnt (table-ref g 'hfont #f))
                (fntsize (table-ref g 'hfontsize #f))
                (page (HPDF_AddPage pdf)))
           (HPDF_Page_SetWidth page (flo w))
           (HPDF_Page_SetHeight page (flo h))
           (if (and fnt fntsize) 
             (HPDF_Page_SetFontAndSize page fnt (flo fntsize)))
           (table-set! g 'hpage page)
        ))
    )))

;; output pdf graph
(define (graphout:pdf g filename)
  (table-set! g 'output 'GRAPH_PDF)
  (let* ((w (table-ref g 'devxmax))
         (h (table-ref g 'devymax))
         (pdf (HPDF_New))
         (page (HPDF_AddPage pdf)))
    (HPDF_SetCompressionMode pdf HPDF_COMP_ALL)
    (HPDF_Page_SetWidth page (flo w))
    (HPDF_Page_SetHeight page (flo h))
    (table-set! g 'hpdf pdf)
    (table-set! g 'hpage page)
    (graphout:dispatch g)
    (HPDF_SaveToFile pdf filename)
    (HPDF_Free pdf)
  ))

(define (graphout:svg g filename)
  (table-set! g 'output 'GRAPH_SVG)
  (let* ((w (table-ref g 'devxmax))
         (h (table-ref g 'devymax))
         (gradients (table-ref g 'svggradients #f))
         (svg (if filename
           `(svg (@ (width ,w) (height ,h) (xmlns "http://www.w3.org/2000/svg")))
           `(svg (@ (version "1.1")
                    (viewBox ,(string-append "0 0 " (number->string (fix w)) " " (number->string (fix h))))
                    (xmlns "http://www.w3.org/2000/svg") 
              ))
          )))
    (table-set! g 'svg (append svg (if gradients (list (append '(defs) gradients)) '(""))))
    (graphout:dispatch g)
    (if filename
      (with-output-to-file filename (lambda ()
        (sxml->xml (table-ref g 'svg))))
      (table-ref g 'svg))
  ))

;; eof
