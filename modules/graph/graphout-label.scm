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

;; draw an X label
(define (graphout:xlabel g text)
  (let ((physxsize (table-ref g 'physxsize))
        (phys2dev  (table-ref g 'phys2dev))
        (leasty    (table-ref g 'leasty))
        (devxorigo (table-ref g 'devxorigo))
        (devyorigo (table-ref g 'devyorigo))
        (fontnum   (table-ref g 'fontnum))
        (coord     (table-ref g 'coord)))
    (graphout:setcoord g GRAPH_AXIS)
    (graphout:htextcenter g 
      (graphout:devx->axisx g (+ (/ (* physxsize phys2dev) 2.0) devxorigo))
      (graphout:devy->axisy g (+ (- leasty (* 0.4 fontnum)) devyorigo))
      text 1.
    )
    (graphout:setcoord g coord)
  ))

;; draw a Y label
(define (graphout:ylabel g text)
  (let ((physysize (table-ref g 'physysize))
        (phys2dev  (table-ref g 'phys2dev))
        (leastx    (table-ref g 'leastx))
        (devxorigo (table-ref g 'devxorigo))
        (devyorigo (table-ref g 'devyorigo))
        (tndirection (table-ref g 'tndirection))
        (xstring   (table-ref g 'xstring))
        (fontnum   (table-ref g 'fontnum))
        (coord     (table-ref g 'coord))
        (o   (table-ref g 'output)))
    (graphout:setcoord g GRAPH_AXIS)
    (graphout:vtextcenter g
      (graphout:devx->axisx g (+ (- leastx (* tndirection
         (if (or (eq? o 'GRAPH_SVG) (eq? o 'GRAPH_OGL)) 3. 0.4) fontnum)) devxorigo))
      (graphout:devy->axisy g (+ (/ (* physysize phys2dev) 2.0) devyorigo))
      text (string-append xstring "X")
    )  ;; xstring is a phantom spacer
    (graphout:setcoord g coord)
  ))

;;eof
