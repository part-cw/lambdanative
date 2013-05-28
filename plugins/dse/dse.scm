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

;; Simple decision support engine

#|
(make-instance store "DSE" "dse" '("Rules" (
  ("Bradycardia"  ((< "HR" "HRmin")) 2)
  ("Hypertension" ((> "NIBPsys" "BPmax")) 2)
)))
|#

(define dse:rule-conclusion car)
(define dse:rule-condition cadr)
(define dse:rule-priority caddr)

(define (dse:check-weak-fact store fact)
  (if (and (list? fact) (> (length fact) 2))
    (let* ((op (car fact)
           (v1 (if (string? (cadr fact)) ((if (eq? op '=) store-ref store-timedref) store (cadr fact) #f) (cadr fact)))
           (v2 (if (string? (caddr fact)) (store-ref store (caddr fact) #f) (caddr fact)))
           (v3 (if (> (length fact) 3) (if (string? (cadddr fact)) (store-ref store (cadddr fact) #f) (cadddr fact)) #f)))
      (cond ((eq? op '<) (and (number? v1) (number? v2) (< v1 v2)))
             ((eq? op '>) (and (number? v1) (number? v2) (> v1 v2)))
             ((eq? op '=) (equal? v1 v2))
             ((eq? op '!=) (not (equal? v1 v2)))
             ((eq? op 'diff>) (and (number? v1) (number? v2) (number? v3) (> (- v2 v1) v3)))
             ((eq? op 'diff<) (and (number? v1) (number? v2) (number? v3) (< (- v2 v1) v3)))
             ((eq? op '>deviation_perc) (and (number? v1) (f32vector? v2) (number? v3) (> (* (median (f32vector->list v2)) (/ (+ 100 v3) 100)) v1)))
             ((eq? op '<deviation_perc) (and (number? v1) (f32vector? v2) (number? v3) (< (* (median (f32vector->list v2)) (/ (- 100 v3) 100)) v1)))                
             (else (log-error "dse: unknown rule operator " op) #f)
      )
    )
    (begin
      (log-error "dse: syntax error in rule " fact) 
      #f
    )
  ))

(define (dse:check-strong-fact store instance fact)
  (member fact (instance-refvar store instance "FactList")))

(define (dse:check-fact store instance fact)
  (if (list? fact)
    (dse:check-weak-fact store fact)
    (dse:check-strong-fact store instance fact)
  ))

(define (dse:trigger-alarm store instance fact)
  (let ((priority (dse:rule-priority (assoc fact (instance-refvar store instance "Rules")))))
    (if (> priority 0) (store-event-add store priority fact instance))
 ))

(define (dse:check-condition store instance rule)
  (define (check-iter condition)
    (if (null? condition)
      #t
      (let ((next-fact (car condition)))
        (if (dse:check-fact store instance next-fact)
          (check-iter (cdr condition))
          #f
        )
      )
    )
  )
  (check-iter (dse:rule-condition rule)))

(define (dse:adopt-conclusion store instance rule)
  (let ((fact (dse:rule-conclusion rule))
        (flist (instance-refvar store instance "FactList")))
     (instance-setvar! store instance "FactList" (append (list fact) flist))
  ))

;; ---- plugin hooks
(define (dse-init store instance)
  #t)

(define (dse-caseinit store instance)
  (instance-setvar! store instance "FactList" '())
  (instance-setvar! store instance "RepeatFactList" '())
)

(define (dse-run store instance)
  (define (dse:outerloop continue?)
    (if continue? 
      (dse:outerloop (dse:innerloop (instance-refvar store instance "Rules"))) 
      #t
    ))
  (define (dse:innerloop rbase)
    (if (null? rbase)
      #f
      (let ((rule (car rbase)))
        (if (not (dse:check-strong-fact store instance (dse:rule-conclusion rule)))
          (if (dse:check-condition store instance rule)
            (begin
              (dse:adopt-conclusion store instance rule) 
              (dse:outerloop #t)
            )
            (dse:innerloop (cdr rbase))
          )
          (dse:innerloop (cdr rbase))
        )
      )
    ))
  (dse:outerloop #t)

  ;; update repeating facts and trigger as needed
  ;; non-repeating stuff is dropped
  (let ((flist (instance-refvar store instance "FactList"))
        (rlist (instance-refvar store instance "RepeatFactList")))
    (let loop ((fs flist)(rs '()))
      (if (= (length fs) 0)
        (instance-setvar! store instance "RepeatFactList" rs)
        (let* ((entry (assoc (car fs) rlist))
               (oldv (if entry (cadr entry) 0))
               (newv (if (>= oldv 10)
                 (begin (dse:trigger-alarm store instance (car fs)) 0)
                 (+ oldv 1))
               ))
          (loop (cdr fs) (append rs (list (list (car fs) newv))))
        ))
    )
  )
  (instance-setvar! store instance "FactList" '())
  #t)

(define (dse-caseend store instance) #t)
(define (dse-end store instance) #t)

(plugin-register "dse" dse-init dse-caseinit dse-run dse-caseend dse-end 'dse)

;; eof