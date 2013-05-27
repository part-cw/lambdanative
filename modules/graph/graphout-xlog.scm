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

;; log x axis

(define (graphout:lwlxnum g start numdist os tt tb)
  (let ((xma (table-ref g 'axisxmax))
        (logtnposition (table-ref g 'logtnposition)))
    (let loop ((x0 start))
      (if (<= x0 xma)
        (let* ((x (graphaux:wctrunc x0 GRAPH:EVEN))
              (line (number->string x)))
          (if (and (> logtnposition 0) (< logtnposition 4))
            (graphout:htextcenter g x (graphout:devy->axisy g (+ os tb (- numdist))) line 2.)
            (graphout:htextcenter g x (graphout:devy->axisy g (+ os tt numdist)) line .0)
          )
          (loop (* x 10.))
        ))
    )
  ))

(define (graphout:lwlxmin g start numdist os tt tb)
  (let* ((xma (table-ref g 'axisxmax))
        (logtnposition (table-ref g 'logtnposition))
        (x (graphaux:wctrunc start GRAPH:EVEN))
        (line (number->string x)))
    (if (and (> logtnposition 0) (< logtnposition 4))
      (graphout:htextcenter g x (graphout:devy->axisy g (+ os tb (- numdist))) line 2.)
      (graphout:htextcenter g x (graphout:devy->axisy g (+ os tt numdist)) line 0.)
    )
  ))

(define (graphout:lwxset g min top bot)
  (let ((xma (table-ref g 'axisxmax)))
    (let loop ((x min))
      (if (<= x xma) (begin
         (graphout:devmove g (graphout:axisx->devx g x) bot)
         (graphout:devline g (graphout:axisx->devx g x) top)
         (graphout:stroke g)
         (loop (* x 10.))
      ))
    )
  ))

(define (graphout:lwallxtix g top bot)
  (let ((xmi (table-ref g 'axisxmin))
        (xma (table-ref g 'axisxmin))
        (ldecx (table-ref g 'ldecx))
        (udecx (table-ref g 'udecx)))
    (let loop ((x xmi))
      (if (<= x ldecx) (begin
         (graphout:devmove g (graphout:axisx->devx g x) bot)
         (graphout:devline g (graphout:axisx->devx g x) top)
         (graphout:stroke g)
         (loop (+ x (/ ldecx 10.))))))
    (let loop ((x udecx))
      (if (<= x xma) (begin
         (graphout:devmove g (graphout:axisx->devx g x) bot)
         (graphout:devline g (graphout:axisx->devx g x) top)
         (graphout:stroke g)
         (loop (+ x udecx)))))
    (if (> udecx ldecx)
       (let loop ((c (* 10. ldecx)))
          (if (<= c  udecx) (begin
             (let loop2 ((x (/ c 10.)))
                (if (<= x c) (begin
                   (graphout:devmove g (graphout:axisx->devx g x) bot)
                   (graphout:devline g (graphout:axisx->devx g x) top)
                   (graphout:stroke g) 
                   (loop2 (+ x (/ c 10.)))
                ))
             )
             (loop (* c 10.))
          ))
        ))
  ))

(define (graphout:lwxlinit g os tt tb numdist tt1 tb1)
  (let* ((logticsel (table-ref g 'logticsel))
         (lognumsel (table-ref g 'lognumsel))
         (xtix logticsel)
         (ldecx (table-ref g 'ldecx))
         (xmi (table-ref g 'axisxmin))
         (xma (table-ref g 'axisxmax))
         (intmin (fix xmi))
         (intdec (fix (/ ldecx 10.))))
  (if (table-ref g 'axisenable) (begin
    (if (and (< logticsel 2048) (>= logticsel 0)) (begin
       (if (> (bitwise-and GRAPH:MINBIT logticsel) 0) (graphout:lwxset g xmi (+ os tt) (+ os tb)))
       (if (> (bitwise-and GRAPH:MAXBIT logticsel) 0) (graphout:lwxset g xma (+ os tt) (+ os tb)))
       (if (not (= intmin intdec)) (graphout:lwxset g ldecx (+ os tt) (+ os tb)))
       (let loop ((oldmask 1)(i 1))
         (if (< i 10) 
           (let ((mask (arithmetic-shift oldmask 1)))
             (if (> (bitwise-and mask logticsel) 0)
               (if (= i 1)
                 (graphout:lwxset g (graphaux:wcxbottom 1 xmi ldecx) (+ os tt1) (+ os tb1))
                 (graphout:lwxset g (graphaux:wcxbottom i xmi ldecx) (+ os tt) (+ os tb))))
                 (loop mask (+ i 1))))))
     (graphout:lwallxtix g (+ os tb) (+ os tt)))))
    (if (table-ref g 'axisnumberenable) (begin
      (if (and (< lognumsel 2048) (>= lognumsel 0)) (begin
        (if (> (bitwise-and GRAPH:MINBIT lognumsel) 0) (graphout:lwlxmin g xmi numdist os tt tb))
        (if (> (bitwise-and GRAPH:MAXBIT lognumsel) 0) (graphout:lwlxnum g xma numdist os tt tb))
       (let loop ((oldmask 1)(i 1))
         (if (< i 10) 
           (let ((mask (arithmetic-shift oldmask 1)))
             (if (> (bitwise-and mask lognumsel) 0)
             (graphout:lwlxnum g (graphaux:wcxbottom i xmi ldecx) numdist os tt tb))
             (loop mask (+ i 1))))))
      (begin 
         (graphout:lwlxmin g xmi numdist os tt tb)
         (graphout:lwlxnum g xma numdist os tt tb)
         (graphout:lwlxnum g (graphaux:wcxbottom 1 xmi ldecx) numdist os tt tb)))))
))

;; setup the log axis
(define (graphout:xlog g physxsize xmin xmax physxoffset)
  (let* ((phys2dev (table-ref g 'phys2dev))
        (xma (graphaux:wctrunc (* xmax 0.99) GRAPH:UP))
        (xmi (graphaux:wctrunc (* xmin 1.01) GRAPH:DOWN))
        (ldecx (expt 10. (ceiling (- (log10 xmi) 0.0001))))
        (udecx (expt 10. (floor (+ (log10 xma) 0.0001))))
        (xrang (log10 (/ xma xmi)))
        (axisx2dev (/ (* physxsize phys2dev) xrang)))
    (table-set! g 'logx #t)
    (table-set! g 'physxsize physxsize)
    (table-set! g 'physxoffset physxoffset)
    (table-set! g 'axisxmax xma)
    (table-set! g 'axisxmin xmi)
    (table-set! g 'axisx2dev axisx2dev)
    (table-set! g 'ldecx ldecx)
    (table-set! g 'udecx udecx)  
    (table-set! g 'xtix (table-ref g 'logticsel))
    (table-set! g 'coord GRAPH_AXIS)
  ))

;; draw the log axis
(define (graphout:xlogaxis g)
  (let* ((phys2dev (table-ref g 'phys2dev)) 
         (devyorigo (table-ref g 'devyorigo))
         (lognumdist (table-ref g 'lognumdist))
         (logtnposition (table-ref g 'logtnposition))
         (logticlen (table-ref g 'logticlen))
         (logticlen10 (table-ref g 'logticlen10))
         (fsizex (table-ref g 'fsizex))
         (fontnum (table-ref g 'fontnum))
         (physxsize (table-ref g 'physxsize))
         (physxoffset (table-ref g 'physxoffset))
         (xma (table-ref g 'axisxmax))
         (xmi (table-ref g 'axisxmin))
         (ldecx (table-ref g 'ldecx))
         (udecx (table-ref g 'udecx))
         (xrang (log10 (/ xma xmi)))
         (axisx2dev (/ (* physxsize phys2dev) xrang))
         (numdist (+ (* lognumdist fsizex 0.1) 1))
         (tb (if (or (= logtnposition 3) (= logtnposition 4)) 0 (- logticlen)))
         (tb1 (if (or (= logtnposition 3) (= logtnposition 4)) 0 (- logticlen10)))
         (tt (if (or (= logtnposition 1) (= logtnposition 6)) 0 logticlen))
         (tt1 (if (or (= logtnposition 1) (= logtnposition 6)) 0 logticlen10)))
    (table-set! g 'leasty (- (+ (* physxoffset phys2dev) tb) numdist (* 1.4 fontnum)))
    (if (table-ref g 'axisenable) (begin
      (table-set! g 'coord GRAPH_PHYS)
      (graphout:moveto g 0 physxoffset)
      (graphout:rlineto g physxsize 0)
      (graphout:stroke g)
      (table-set! g 'coord GRAPH_AXIS)
      (graphout:lwxlinit g (+ (* physxoffset phys2dev) devyorigo) tt tb numdist tt1 tb1)
    ))
  ))

;; eof