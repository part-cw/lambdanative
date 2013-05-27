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

;; log y axis
(define (graphout:lwlynum g start numdist os tr tl)
  (let ((yma (table-ref g 'axisymax))
        (logtnposition (table-ref g 'logtnposition)))
    (let loop ((y0 start))
      (if (<= y0 yma)
        (let* ((y (graphaux:wctrunc y0 GRAPH:EVEN))
               (line (number->string y)))
          (if (and (> logtnposition 0) (< logtnposition 4))
            (graphout:htextright g (graphout:devx->axisx g (+ os tl (- numdist))) y line)
            (graphout:htextleft g (graphout:devx->axisx g (+ os tr numdist)) y line))
          (if (> (string-length line) (string-length (table-ref g 'xstring)))
            (table-set! g 'xstring line))
          (loop (* y 10.))
        )))
  ))

(define (graphout:lwlymin g start numdist os tr tl)
  (let* ((yma (table-ref g 'axisymax))
         (logtnposition (table-ref g 'logtnposition))
         (y (graphaux:wctrunc start GRAPH:EVEN))
         (line (number->string y)))
    (if (and (> logtnposition 0) (< logtnposition 4))
      (graphout:htextright g (graphout:devx->axisx g (+ os tl (- numdist))) y line)
      (graphout:htextleft g (graphout:devx->axisx g (+ os tr numdist)) y line))
      (if (> (string-length line) (string-length (table-ref g 'xstring)))
        (table-set! g 'xstring line)
      )
  ))

(define (graphout:lwyset g min right left)
  (let ((yma (table-ref g 'axisymax)))
    (let loop ((y min))
      (if (<= y yma) (begin
         (graphout:devmove g right (graphout:axisy->devy g y))
         (graphout:devline g left (graphout:axisy->devy g y))
         (graphout:stroke g)
         (loop (* y 10.))
      ))
    )
  ))

(define (graphout:lwallytix g left right)
  (let ((ymi (table-ref g 'axisymin))
        (yma (table-ref g 'axisymin))
        (ldecy (table-ref g 'ldecy))
        (udecy (table-ref g 'udecy)))
    (let loop ((y ymi))
      (if (<= y ldecy) (begin
         (graphout:devmove g right (graphout:axisy->devy g y))
         (graphout:devline g left (graphout:axisy->devy g y))
         (graphout:stroke g)
         (loop (+ y (/ ldecy 10.))))))
    (let loop ((y udecy))
      (if (<= y yma) (begin
         (graphout:devmove g right (graphout:axisy->devy g y))
         (graphout:devline g left (graphout:axisy->devy g y))
         (graphout:stroke g)
         (loop (+ y udecy)))))
    (if (> udecy ldecy)
       (let loop ((c (* 10. ldecy)))
          (if (<= c udecy) (begin
             (let loop2 ((y (/ c 10.)))
                (if (<= y c) (begin
                   (graphout:devmove g right (graphout:axisy->devy g y))
                   (graphout:devline g left (graphout:axisy->devy g y))
                   (graphout:stroke g) 
                   (loop2 (+ y (/ c 10.))))))
             (loop (* c 10.))
          ))
        )
     )
  ))

(define (graphout:lwylinit g os tt tb numdist tt1 tb1)
  (let* ((logticsel (table-ref g 'logticsel))
         (lognumsel (table-ref g 'lognumsel))
         (ytix logticsel)
         (ldecy (table-ref g 'ldecy))
         (ymi (table-ref g 'axisymin))
         (yma (table-ref g 'axisymax))
         (intmin (fix ymi))
         (intdec (fix (/ ldecy 10.))))
  (table-set! g 'xstring "")
  (if (table-ref g 'axisenable) (begin
    (if (and (< logticsel 2048) (>= logticsel 0)) (begin
      (if (> (bitwise-and GRAPH:MINBIT logticsel) 0) (graphout:lwyset g ymi (+ os tt) (+ os tb)))
      (if (> (bitwise-and GRAPH:MAXBIT logticsel) 0) (graphout:lwyset g yma (+ os tt) (+ os tb)))
      (if (not (= intmin intdec)) (graphout:lwyset g ldecy (+ os tt) (+ os tb)))
      (let loop ((oldmask 1)(i 1))
        (if (< i 10) 
          (let ((mask (arithmetic-shift oldmask 1)))
            (if (> (bitwise-and mask logticsel) 0)
              (if (= i 1)
                (graphout:lwyset g (graphaux:wcybottom 1 ymi ldecy) (+ os tt1) (+ os tb1))
                (graphout:lwyset g (graphaux:wcybottom i ymi ldecy) (+ os tt) (+ os tb))
              )
            )
            (loop mask (+ i 1))
          ))
       ))
       (graphout:lwallytix g (+ os tb) (+ os tt))
    )))

    (if (table-ref g 'axisnumberenable) (begin
      (if (and (< lognumsel 2048) (>= lognumsel 0)) (begin
        (if (> (bitwise-and GRAPH:MINBIT lognumsel) 0) (graphout:lwlymin g ymi numdist os tt tb))
        (if (> (bitwise-and GRAPH:MAXBIT lognumsel) 0) (graphout:lwlynum g yma numdist os tt tb))
        (let loop ((oldmask 1)(i 1))
          (if (< i 10) 
            (let ((mask (arithmetic-shift oldmask 1)))
              (if (> (bitwise-and mask lognumsel) 0)
                (graphout:lwlynum g (graphaux:wcybottom i ymi ldecy) numdist os tt tb)
              )
              (loop mask (+ i 1))
            ))
        ))
      (begin 
        (graphout:lwlymin g ymi numdist os tt tb)
        (graphout:lwlynum g yma numdist os tt tb)
        (graphout:lwlynum g (graphaux:wcybottom 1 ymi ldecy) numdist os tt tb)
      ))
    ))
  ))

;; setup the log axis
(define (graphout:ylog g physysize ymin ymax physyoffset)
  (let* ((phys2dev (table-ref g 'phys2dev))
        (yma (graphaux:wctrunc (* ymax 0.99) GRAPH:UP))
        (ymi (graphaux:wctrunc (* ymin 1.01) GRAPH:DOWN))
        (ldecy (expt 10. (ceiling (- (log10 ymi) 0.0001))))
        (udecy (expt 10. (floor (+ (log10 yma) 0.0001))))
        (yrang (log10 (/ yma ymi)))
        (axisy2dev (/ (* physysize phys2dev) yrang)))
    (table-set! g 'logy #t)
    (table-set! g 'physysize physysize)
    (table-set! g 'physyoffset physyoffset)
    (table-set! g 'axisymax yma)
    (table-set! g 'axisymin ymi)
    (table-set! g 'axisy2dev axisy2dev)
    (table-set! g 'ldecy ldecy)
    (table-set! g 'udecy udecy)
    (table-set! g 'ytix (table-ref g 'logticsel))
    (table-set! g 'coord GRAPH_AXIS)
  ))

;; draw the axis
(define (graphout:ylogaxis g)
  (let* ((phys2dev (table-ref g 'phys2dev)) 
         (devxorigo (table-ref g 'devxorigo))
         (lognumdist (table-ref g 'lognumdist))
         (logtnposition (table-ref g 'logtnposition))
         (logticlen (table-ref g 'logticlen))
         (logticlen10 (table-ref g 'logticlen10))
         (fsizex (table-ref g 'fsizex))
         (fontnum (table-ref g 'fontnum))
         (physysize (table-ref g 'physysize))
         (physyoffset (table-ref g 'physyoffset))
         (yma (table-ref g 'axisymax))
         (ymi (table-ref g 'axisymin))
         (ldecy (table-ref g 'ldecy))
         (udecy (table-ref g 'udecy))
         (yrang (log10 (/ yma ymi)))
         (axisy2dev (/ (* physysize phys2dev) yrang))
         (numdist (+ (* lognumdist fsizex 0.1) 1))
         (tl (if (or (= logtnposition 3) (= logtnposition 4)) 0 (- logticlen)))
         (tl1 (if (or (= logtnposition 3) (= logtnposition 4)) 0 (- logticlen10)))
         (tr (if (or (= logtnposition 1) (= logtnposition 6)) 0 logticlen))
         (tr1 (if (or (= logtnposition 1) (= logtnposition 6)) 0 logticlen10)))
    (table-set! g 'ispolar #f) (table-set! g 'logy #t)
    (table-set! g 'physysize physysize)
    (table-set! g 'axisymax yma) (table-set! g 'axisymin ymi)
    (table-set! g 'ldecy ldecy)  (table-set! g 'udecy udecy)  
    (table-set! g 'axisy2dev axisy2dev)
    (table-set! g 'leastx (- (+ (* physyoffset phys2dev) tl) numdist))
    (if (table-ref g 'axisenable) (begin
      (table-set! g 'coord GRAPH_PHYS)
      (graphout:moveto g physyoffset 0)
      (graphout:rlineto g 0 physysize)
      (graphout:stroke g)
      (table-set! g 'coord GRAPH_AXIS)
      (graphout:lwylinit g (+ (* physyoffset phys2dev) devxorigo) tr tl numdist tr1 tl1)
    ))
  ))

;; eof