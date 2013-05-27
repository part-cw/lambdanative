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
  (let loop ((cmds (vector->list (table-ref g 'commands))))
    (if (and (> (length cmds) 0) (car cmds))
      (let ((cmdname (car (car cmds)))(cmdargs (cdr (car cmds))))
        (cond 
          ((eq? cmdname '@axisenable) (apply graphout:axisenable (append (list g) cmdargs)))
          ((eq? cmdname '@aorigin) (apply graphout:aorigin (append (list g) cmdargs)))
          ((eq? cmdname '@box) (apply graphout:box (append (list g) cmdargs)))
          ((eq? cmdname '@circle) (apply graphout:circle (append (list g) cmdargs)))
          ((eq? cmdname '@closepathstroke) (apply graphout:closepathstroke (append (list g) cmdargs)))
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
          (else  (log-error (car (car cmds)) " is not implemented\n")
        ))
      (loop (cdr cmds))
    ))
  ))

;; output pdf graph
(define (graphout:pdf g . filename)
  (table-set! g 'output GRAPH_PDF)
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
;; eof