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

(define (graphout:linxmesh g)
  (let ((xmin (table-ref g 'axisxmin))
        (xmax (table-ref g 'axisxmax))
        (xticks (table-ref g 'xticks))
        (ymin (table-ref g 'axisymin))
        (ymax (table-ref g 'axisymax)))
   (let loop ((x xmin))
     (if (< x xmax) (begin
       (graphout:moveto g x ymin)
       (graphout:lineto g x ymax)
       (loop (+ x xticks))
     )))
  ))

(define (graphout:linymesh g)
  (let ((xmin (table-ref g 'axisxmin))
        (xmax (table-ref g 'axisxmax))
        (yticks (table-ref g 'yticks))
        (ymin (table-ref g 'axisymin))
        (ymax (table-ref g 'axisymax)))
   (let loop ((y ymin))
     (if (< y ymax) (begin
       (graphout:moveto g xmin y)
       (graphout:lineto g xmax y)
       (loop (+ y yticks))
     )))
  ))

(define (graphout:lwxmset g min)
  (let ((xma (table-ref g 'axisxmax))
        (yma (table-ref g 'axisymax))
        (ymi (table-ref g 'axisymin)))
    (let loop ((x min))
       (if (<= x xma) (begin
         (graphout:moveto g x ymi) 
         (graphout:lineto g x yma)
         (loop (* x 10.))
       )))
  ))

(define (graphout:lwxminmset g min)
  (let ((yma (table-ref g 'axisymax))
        (ymi (table-ref g 'axisymin)))
    (graphout:moveto g min ymi)
    (graphout:lineto g min yma)
  ))

(define (graphout:lwlxmesh g)
  (let* ((xmi (table-ref g 'axisxmin))
        (xma (table-ref g 'axisxmax))
        (yma (table-ref g 'axisymax))
        (ymi (table-ref g 'axisymin))
        (ldecx (table-ref g 'ldecx))
        (udecx (table-ref g 'udecx))
        (xtix (table-ref g 'xtix))
        (intmin xmi)
        (intdec (/ ldecx 10.)))
    (if (and (< xtix 2048) (>= xtix 0)) (begin
       (if (> (bitwise-and GRAPH:MINBIT xtix) 0) (graphout:lwxminmset g xmi))
       (if (> (bitwise-and GRAPH:MAXBIT xtix) 0) (graphout:lwxmset g xma))
       (if (not (= intmin intdec)) (graphout:lwxmset g ldecx))
       (let loop ((oldmask 1)(i 1))
         (if (< i 10) 
           (let ((mask (arithmetic-shift oldmask 1)))
             (if (> (bitwise-and mask xtix) 0)
               (graphout:lwxmset g (graphaux:wcxbottom i xmi ldecx))
             )
             (loop mask (+ i 1))
           )
         )
       ))
       (begin
         (let loop ((x xmi))
           (if (<= x ldecx) (begin
             (graphout:moveto g x ymi) 
             (graphout:lineto g x yma)
             (loop (+ x (/ ldecx 10.)))
           ))
         )
         (let loop ((x udecx))
           (if (<= x xma) (begin
             (graphout:moveto g x ymi) 
             (graphout:lineto g x yma)
             (loop (+ x udecx))
           ))
         )
         (if (> udecx ldecx)
           (let loop ((c (* 10. ldecx)))
             (if (<= c udecx) (begin
               (let loop2 ((x (/ c 10.)))
                 (if (<= x c) (begin
                   (graphout:moveto g x ymi) 
                   (graphout:lineto g x yma)
                   (loop2 (+ x (/ c 10.)))
                 ))
               )
               (loop (* c 10.)))
             ))
         )))
  ))

(define (graphout:lwymset g min)
  (let ((yma (table-ref g 'axisymax))
        (xma (table-ref g 'axisxmax))
        (xmi (table-ref g 'axisxmin)))
    (let loop ((y min))
       (if (<= y yma) (begin
         (graphout:moveto g xmi y) (graphout:lineto g xma y)
         (loop (* y 10.)))))))

(define (graphout:lwyminmset g min)
  (let ((xma (table-ref g 'axisxmax))
        (xmi (table-ref g 'axisxmin)))
    (graphout:moveto g xmi min)
    (graphout:lineto g xma min)
  ))

(define (graphout:lwlymesh g)
  (let* ((xmi (table-ref g 'axisxmin))
        (xma (table-ref g 'axisxmax))
        (yma (table-ref g 'axisymax))
        (ymi (table-ref g 'axisymin))
        (ldecy (table-ref g 'ldecy))
        (udecy (table-ref g 'udecy))
        (ytix (table-ref g 'ytix))
        (intmin ymi)
        (intdec (/ ldecy 10.)))
    (if (and (< ytix 2048) (>= ytix 0)) (begin
       (if (> (bitwise-and GRAPH:MINBIT ytix) 0) (graphout:lwyminmset g ymi))
       (if (> (bitwise-and GRAPH:MAXBIT ytix) 0) (graphout:lwymset g yma))
       (if (not (= intmin intdec)) (graphout:lwymset g ldecy))
       (let loop ((oldmask 1)(i 1))
         (if (< i 10)
           (let ((mask (arithmetic-shift oldmask 1)))
             (if (> (bitwise-and mask ytix) 0)
               (graphout:lwymset g (graphaux:wcybottom i ymi ldecy))
             )
             (loop mask (+ i 1))
           ))
        ))
        (begin
          (let loop ((y ymi))
            (if (<= y ldecy) (begin
              (graphout:moveto g xmi y)
              (graphout:lineto g xma y)
              (loop (+ y (/ ldecy 10.)))
            ))
          )
          (let loop ((y udecy))
            (if (<= y yma) (begin
              (graphout:moveto g xmi y) 
              (graphout:lineto g xma y)
              (loop (+ y udecy))
            ))
          )
          (if (> udecy ldecy)
            (let loop ((c (* 10. ldecy)))
              (if (<= c udecy) (begin
                (let loop2 ((y (/ c 10.)))
                  (if (<= y c) (begin
                    (graphout:moveto g xmi y)
                    (graphout:lineto g xma y)
                    (loop2 (+ y (/ c 10.)))
                  ))
                )
                (loop (* c 10.))
              ))
            ))))
  ))

(define (graphout:mesh g)
  (let ((logx (table-ref g 'logx))
        (logy (table-ref g 'logy)))
    (if logx
      (graphout:lwlxmesh g)
      (graphout:linxmesh g)
    )
    (if logy
      (graphout:lwlymesh g)
      (graphout:linymesh g)
    )
    (graphout:stroke g)
  ))

;; eof