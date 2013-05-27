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

;; handle the yaxis (linear)

;; setup the y axis
(define (graphout:ylinear g physysize ymin ymax physyoffset ytix numsepin)
  (let ((phys2dev (table-ref g 'phys2dev))
        (yma (+ ymax (* 0.001 (- ymax ymin)))))
    (table-set! g 'logy #f)
    (table-set! g 'physysize physysize)
    (table-set! g 'physyoffset physyoffset)
    (table-set! g 'axisymax yma)
    (table-set! g 'axisymin ymin)
    (table-set! g 'yticks ytix)  
    (table-set! g 'axisy2dev (/ (* physysize phys2dev) (- yma ymin)))
    (table-set! g 'ynumsep (if (< numsepin 1) 1 (if (> numsepin 99) 99 numsepin)))
    (table-set! g 'coord GRAPH_AXIS)
  ))

;;(define (graphout:yaxis g physysize ymin ymax physyoffset ytix numsepin)
(define (graphout:ylinaxis g)
  (let* ((phys2dev (table-ref g 'phys2dev))
         (linnumdist (table-ref g 'linnumdist))
         (lintnposition (table-ref g 'lintnposition))
         (precision (table-ref g 'precision))
         (linnumoff (table-ref g 'linnumoff))
         (linticlen (table-ref g 'linticlen))
         (fontnum (table-ref g 'fontnum))
         (devxorigo (table-ref g 'devxorigo))
         (numsep (table-ref g 'ynumsep))
         (physysize (table-ref g 'physysize))
         (physyoffset (table-ref g 'physyoffset))
         (ytix (table-ref g 'yticks))
         (ymin (table-ref g 'axisymin))
         (ymax (table-ref g 'axisymax))
         (ysiz (* physysize phys2dev))
         (yma ymax)
         (ymi ymin)
         (yrang (- yma ymi))
         (axisy2dev (/ ysiz yrang))
         (numdist (fix (+ linnumdist 1)))
         (tl (if (or (= lintnposition 3) (= lintnposition 4)) 0 (- linticlen)))
         (tr (if (or (= lintnposition 1) (= lintnposition 6)) 0 linticlen)))
  (table-set! g 'xstring "")
  (if (table-ref g 'axisenable) (begin
    (table-set! g 'coord GRAPH_PHYS)
    (graphout:moveto g physyoffset 0)
    (graphout:rlineto g 0 physysize)
    (table-set! g 'coord GRAPH_AXIS)
    (let loop ((y ymin))
      (if (< y yma) (begin
        (graphout:devmove g (+ (* physyoffset phys2dev) tr devxorigo) (graphout:axisy->devy g y))
        (graphout:devline g (+ (* physyoffset phys2dev) tl devxorigo) (graphout:axisy->devy g y))
        (loop (+ y ytix))
      ))
    )
    (graphout:stroke g)))
    (if (table-ref g 'axisnumberenable)
      (let ((sigdec (graphaux:findsigdec ymi (* yrang 1.01) numsep ytix precision linnumoff)))
        (let loop ((y (* linnumoff ytix)))
          (if (<= y (* yrang 1.01)) 
            (let ((line (graphaux:formaxnum (+ y ymi) sigdec precision)))
              (if (and (> lintnposition 0) (< lintnposition 4)) (begin
                (graphout:htextright g 
                  (graphout:devx->axisx g (+ (* physyoffset phys2dev) tl (- numdist) devxorigo))
                  (+ y ymin) line)
                (table-set! g 'leastx (+ (* physyoffset phys2dev) tl (- numdist))))
                (begin
                  (graphout:htextleft g 
                    (graphout:devx->axisx g (+ (* physyoffset phys2dev) tr numdist devxorigo)) 
                    (+ y ymin) line)
                  (table-set! g 'leastx (+ (* physyoffset phys2dev) tr numdist))
                )
              )
              (if (> (string-length line)
                (string-length (table-ref g 'xstring)))
                (table-set! g 'xstring line)
              )
              (loop (+ y (* numsep ytix)))
            ))
        ))
    )
  ))

;; eof