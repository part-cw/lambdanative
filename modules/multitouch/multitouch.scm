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
;; basic multitouch handling
;; at the moment this only keeps track of two concurrent touches

(define multitouch:touchid0 0)
(define multitouch:touchid1 0)
(define multitouch:touchid2 0)

(define multitouch:touch1 '(0 0))
(define multitouch:touch2 '(0 0))

;; return true if multitouch is occurring
(define (multitouch?) (not (= multitouch:touchid1 multitouch:touchid2)))

;; multitouch event handler
(define (multitouch t x y) 
  ;; new event id
  (if (= t EVENT_MULTITOUCH) (begin (set! multitouch:present #t) (set! multitouch:touchid0 x)))
  ;; keep track of two event ids
  (if (= t EVENT_BUTTON1DOWN)
    (if (and (= multitouch:touchid1 0) (not (= multitouch:touchid2 multitouch:touchid0))) 
      (set! multitouch:touchid1 multitouch:touchid0)
        (if (and (= multitouch:touchid2 0) (not (= multitouch:touchid1 multitouch:touchid0))) 
          (set! multitouch:touchid2 multitouch:touchid0))))
  ;; clear current event id
  (if (= t EVENT_BUTTON1UP)
    (if (= multitouch:touchid1 multitouch:touchid0) (set! multitouch:touchid1 0)
      (if (= multitouch:touchid2 multitouch:touchid0) (set! multitouch:touchid2 0))))
  ;; insert touch coordinates for two ids
  (if (or (= t EVENT_MOTION) (= t EVENT_BUTTON1DOWN) (= t EVENT_BUTTON1UP))
    (if (= multitouch:touchid0 multitouch:touchid1) (set! multitouch:touch1 (list x y))
      (if (= multitouch:touchid0 multitouch:touchid2) (set! multitouch:touch2 (list x y))))))

;; return x coordinate of multi-event 0 or 1
(define (multitouch-x id) 
  (cond 
    ((= id 0) (car multitouch:touch1))
    ((= id 1) (car multitouch:touch2))
    (else #f)))

;; return y coordinate of multi-event 0 or 1
(define (multitouch-y id) 
  (cond 
    ((= id 0) (cadr multitouch:touch1))
    ((= id 1) (cadr multitouch:touch2))
    (else #f)))

;; x coordinate of current touch event
(define (multitouch-curx) 
  (if (= multitouch:touchid0 multitouch:touchid1) (car multitouch:touch1)
    (if (= multitouch:touchid0 multitouch:touchid2) (car multitouch:touch2))))

;; y coordinate of current touch event
(define (multitouch-cury) 
  (if (= multitouch:touchid0 multitouch:touchid1) (cadr multitouch:touch1)
    (if (= multitouch:touchid0 multitouch:touchid2) (cadr multitouch:touch2))))

;; eof
