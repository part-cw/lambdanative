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

;; handle the xaxis (linear)

;; setup the x axis
(define (graphout:xlinear g physxsize xmin xmax physxoffset xtix numsepin)
  (let ((phys2dev (table-ref g 'phys2dev))
        (xma (+ xmax (* 0.001 (- xmax xmin)))))
  (table-set! g 'logx #f) 	  
  (table-set! g 'physxsize physxsize)
  (table-set! g 'physxoffset physxoffset)
  (table-set! g 'axisxmax xma)
  (table-set! g 'axisxmin xmin)
  (table-set! g 'xticks xtix) 
  (table-set! g 'axisx2dev (/ (* physxsize phys2dev) (- xma xmin)))
  (table-set! g 'xnumsep (if (< numsepin 1) 1 (if (> numsepin 99) 99 numsepin)))
  (table-set! g 'coord GRAPH_AXIS)
))

;; switch rendering of tic labels to time format t
(define (graphout:tictime g t)
  (if (string? t)
    (table-set! g 'tictime t)
  )
)

;; draw the x axis
(define (graphout:xlinaxis g)
  (let* ((phys2dev (table-ref g 'phys2dev))
         (devyorigo (table-ref g 'devyorigo))
         (fsizex (table-ref g 'fsizex))
         (linnumdist (table-ref g 'linnumdist))
         (lintnposition (table-ref g 'lintnposition))
         (precision (table-ref g 'precision))
         (linnumoff (table-ref g 'linnumoff))
         (linticlen (table-ref g 'linticlen))
         (fontnum (table-ref g 'fontnum))
         (numsep (table-ref g 'xnumsep))
         (physxsize (table-ref g 'physxsize))
         (physxoffset (table-ref g 'physxoffset))
         (tictime (table-ref g 'tictime #f))
         (xtix (table-ref g 'xticks))
         (xmin (table-ref g 'axisxmin))
         (xmax (table-ref g 'axisxmax))
         (xsiz (* physxsize phys2dev))
         (xma xmax)
         (xmi xmin)
         (xrang (- xma xmi))
         (xticks xtix)
         (axisx2dev (/ xsiz xrang))
         (numdist (fix (+ (* linnumdist fsizex 0.1) 1)))
         (tb (if (or (= lintnposition 3) (= lintnposition 4)) 0 (- linticlen)))
         (tt (if (or (= lintnposition 1) (= lintnposition 6)) 0 linticlen)))
  (if (table-ref g 'axisenable) (begin
     (table-set! g 'coord GRAPH_PHYS)
     (graphout:moveto g 0 physxoffset)
     (graphout:rlineto g physxsize 0)
     (table-set! g 'coord GRAPH_AXIS)
     (let loop ((x xmin))
       (if (< x xma) (begin
         (graphout:devmove g (graphout:axisx->devx g x) (+ (* physxoffset phys2dev) tt devyorigo))
         (graphout:devline g (graphout:axisx->devx g x) (+ (* physxoffset phys2dev) tb devyorigo))
         (loop (+ x xtix))
       ))
     )
     (graphout:stroke g)))
     (if (table-ref g 'axisnumberenable) 
       (let ((sigdec (graphaux:findsigdec xmi 
             (* xrang 1.01) numsep xticks precision linnumoff)))
         (let loop ((x (* linnumoff xticks)))
            (if (<= x (* xrang 1.01)) 
               (let ((line (if tictime (localseconds->string x tictime) (graphaux:formaxnum (+ x xmi) sigdec precision))))
                  (if (and (> lintnposition 0) (< lintnposition 4))
                     (graphout:htextcenter g (+ x xmin)
                       (graphout:devy->axisy g (+ (* physxoffset phys2dev) tb (- numdist) devyorigo)) 
                       line 2.)
                     (graphout:htextcenter g (+ x xmin)
                       (graphout:devy->axisy g (+ (* physxoffset phys2dev) tt numdist devyorigo)) 
                       line 0.)
                  )
                  (loop (+ x (* numsep xtix)))
                ))
         )))
     (table-set! g 'leasty (- (+ (* physxoffset phys2dev) tb) numdist (* 1.4 fontnum)))
  ))
;; eof