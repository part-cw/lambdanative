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

(define multitouch:timeout 1.0)

(define multitouch:table (make-table))

(define multitouch:curid #f)
(define multitouch:prvid #f)

;; return true if multitouch is occurring
(define (multitouch?)
  (let* ((now (time->seconds (current-time)))
         (entry1 (if multitouch:curid (table-ref multitouch:table multitouch:curid #f) #f))
         (entry2 (if multitouch:prvid (table-ref multitouch:table multitouch:prvid #f) #f))
         (t1 (if entry1 (caddr entry1) 0))
         (t2 (if entry2 (caddr entry2) 0)))
    (and (< (- now t1) multitouch:timeout)
         (< (- now t2) multitouch:timeout))))

;; multitouch event handler
(define (multitouch t x y) 
  (let ((now (time->seconds (current-time))))
    (if (= t EVENT_MULTITOUCH) (begin
       (if (not (eq? x multitouch:curid)) (set! multitouch:prvid multitouch:curid))
       (set! multitouch:curid x)))
    (if (= t EVENT_BUTTON1UP) (set! multitouch:curid #f))
    (if (or (= t EVENT_MOTION) (= t EVENT_BUTTON1DOWN))
      (table-set! multitouch:table multitouch:curid (list x y now)))
   ))

;; return x coordinate of multi-event 0 or 1
(define (multitouch-x id) 
  (if (and multitouch:curid multitouch:prvid)
    (let ((id1 (min multitouch:curid multitouch:prvid))
          (id2 (max multitouch:curid multitouch:prvid)))
      (cond
        ((= id 0) (car (table-ref multitouch:table id1 '(#f #f #f))))
        ((= id 1) (car (table-ref multitouch:table id2 '(#f #f #f))))
        (else #f))) #f))

(define (multitouch-y id)
  (if (and multitouch:curid multitouch:prvid)
    (let ((id1 (min multitouch:curid multitouch:prvid))
          (id2 (max multitouch:curid multitouch:prvid)))
      (cond
        ((= id 0) (cadr (table-ref multitouch:table id1 '(#f #f #f))))
        ((= id 1) (cadr (table-ref multitouch:table id2 '(#f #f #f))))
        (else #f))) #f))

;; coordinates of current touch event
(define (multitouch-curx)
  (if multitouch:curid (car (table-ref multitouch:table multitouch:curid '(#f #f #f))) #f))

(define (multitouch-cury)
  (if multitouch:curid (cadr (table-ref multitouch:table multitouch:curid '(#f #f #f))) #f))

;; eof
