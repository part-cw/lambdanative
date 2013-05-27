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

;; convert from physical/axis coordinates to device coordinates (i.e. dpi)
;; 
;; to prevent confusion we prefix coordinates like this:
;; axis[] = axis coordinate
;; phys[] = physical (inch) coordinate
;; dev[] = device coordinate
;;
;; if coordinates=GRAPH_PHYS:
;; units of physical coordinates (inches) around device origo
;; devx = x * phys2dev + devxorigo
;;
;; if coordinates=GRAPH_AXIS:
;; units of axis around device origo
;; devx = (- x axisxmin) * axisx2dev + devxorigo
;;

;; X-Axis
(define (graphout:axisx->devx g x)
  (let ((axisxmin (table-ref g 'axisxmin))
        (axisx2dev (table-ref g 'axisx2dev))
        (devxorigo (table-ref g 'devxorigo))
        (logx (table-ref g 'logx)))
    (if logx 
      (+ (* axisx2dev (- (log10 x) (log10 axisxmin))) devxorigo)
      (+ (* axisx2dev (- x axisxmin)) devxorigo)
    )))

(define (graphout:physx->devx g x)
  (let ((phys2dev (table-ref g 'phys2dev))
        (devxorigo (table-ref g 'devxorigo)))
    (+ (* phys2dev x) devxorigo)))

(define (graphout:curx->devx g x)
  (if (= (table-ref g 'coord) GRAPH_AXIS)
     (graphout:axisx->devx g x)
     (graphout:physx->devx g x)))
   
(define (graphout:devx->axisx g x)
  (let ((axisxmin (table-ref g 'axisxmin))
        (axisx2dev (table-ref g 'axisx2dev))
        (devxorigo (table-ref g 'devxorigo))  
        (logx (table-ref g 'logx)))
    (if logx 
      (expt 10. (+ (/ (- x devxorigo) axisx2dev) (log10 axisxmin)))
      (+ (/ (- x devxorigo) axisx2dev) axisxmin)
    )))

(define (graphout:devx->physx g x)
  (let ((phys2dev (table-ref g 'phys2dev))
        (devxorigo (table-ref g 'devxorigo)))
    (/ (- x devxorigo) phys2dev)))
  
(define (graphout:devx->curx g x)
  (if (= (table-ref g 'coord) GRAPH_AXIS)
     (graphout:devx->axisx g x)
     (graphout:devx->physx g x)))

;; Y-Axis
 
(define (graphout:axisy->devy g y)
  (let ((axisymin (table-ref g 'axisymin))
        (axisy2dev (table-ref g 'axisy2dev))
        (devyorigo (table-ref g 'devyorigo))
        (logy (table-ref g 'logy)))
    (if logy 
      (+ (* axisy2dev (- (log10 y) (log10 axisymin))) devyorigo)
      (+ (* axisy2dev (- y axisymin)) devyorigo)
    )))

(define (graphout:physy->devy g y)
  (let ((phys2dev (table-ref g 'phys2dev))
        (devyorigo (table-ref g 'devyorigo)))
    (+ (* phys2dev y) devyorigo)))

(define (graphout:cury->devy g y)
  (if (= (table-ref g 'coord) GRAPH_AXIS)
     (graphout:axisy->devy g y)
     (graphout:physy->devy g y)))
   
(define (graphout:devy->axisy g y)
  (let ((axisymin (table-ref g 'axisymin))
        (axisy2dev (table-ref g 'axisy2dev))
        (devyorigo (table-ref g 'devyorigo))  
        (logy (table-ref g 'logy)))
    (if logy 
      (expt 10. (+ (/ (- y devyorigo) axisy2dev) (log10 axisymin)))
      (+ (/ (- y devyorigo) axisy2dev) axisymin)
    )))

(define (graphout:devy->physy g y)
  (let ((phys2dev (table-ref g 'phys2dev))
        (devyorigo (table-ref g 'devyorigo)))
    (/ (- y devyorigo) phys2dev)))
  
(define (graphout:devy->cury g y)
  (if (= (table-ref g 'coord) GRAPH_AXIS)
     (graphout:devy->axisy g y)
     (graphout:devy->physy g y)))

;; eof
