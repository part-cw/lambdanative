
;;(include "../../libs/gambit/myenv.sch")
;;(include "../../multi-parser/id/srfi-12.sch")


(define (exc:signal obj) (raise (list obj)))

(define gambit-error error)

(define (error msg . args)
  (abort (make-property-condition 'exn 'message (cons msg args))))


;  (define (with-exception-handler handler thunk)
;    (let ((old #f))
;     (dynamic-wind
;      (lambda ()
;        (set! old *current-exn-handler*)
;        (set! *current-exn-handler* handler))
;      thunk
;      (lambda ()
;        (set! *current-exn-handler* old)))))
 
;  (define (abort obj)
;     ((CURRENT-EXCEPTION-HANDLER) obj)
;     (ABORT (make-property-condition
;             'exn
;             'message
;             "Exception handler returned")))
 
;  (define (exc:signal exn)
;   ((CURRENT-EXCEPTION-HANDLER) exn))


;------------------------------------------------------------------------
; Exception conditions
; The following is an approximate implementation of conditions that
; uses lists, instead of a disjoint class of values
; The code below is basically the reference SRFI-12 implementation,
; with a few types fixed.

; A condition is represented as a pair where the first value of the
; pair is this function. A program could forge conditions, and they're
; not disjoint from Scheme pairs.
; Exception conditions are disjoint from any other Scheme values
; (or so should appear).
(define (condition? obj)
  (and (pair? obj)
       (eq? condition? (car obj))))


; Procedure: make-property-condition KIND-KEY PROP-KEY VALUE ...
; This procedure accepts any even number of arguments after kind-key,
; which are regarded as a sequence of alternating prop-key and value
; objects. Each prop-key is regarded as the name of a property, and
; each value is regarded as the value associated with the key that
; precedes it. Returns a kind-key condition that associates the given
; prop-keys with the given values.
(define (make-property-condition kind-key . prop-vals)
  (cons condition? (list (cons kind-key prop-vals))))


; Procedure: make-composite-condition CONDITION ...
; Returns a newly-allocated condition whose components correspond to
; the the given conditions. A predicate created by CONDITION-PREDICATE
; returns true for the new condition if and only if it returns true
; for one or more of its component conditions.
(define (make-composite-condition . conditions)
  (cons condition? (apply append (map cdr conditions))))
 

; Procedure: condition-predicate KIND-KEY 
; Returns a predicate that can be called with any object as its
; argument. Given a condition that was created by
; make-property-condition, the predicate returns #t if and only if
; kind-key is EQV? to the kind key that was passed to
; make-property-condition. Given a composite condition created with
; make-composite-condition, the predicate returns #t if and only if
; the predicate returns #t for at least one of its components.
(define (condition-predicate kind-key)
  (lambda (exn)
    (and (condition? exn) (assv kind-key (cdr exn)))))
 
; Procedure: condition-property-accessor KIND-KEY PROP-KEY
; Returns a procedure that can be called with any condition that satisfies
; (condition-predicate KIND-KEY). Given a condition that was created by
; make-property-condition and KIND-KEY, the procedure returns the value
; that is associated with prop-key. Given a composite condition created with
; make-composite-condition, the procedure returns the value that is
; associated with prop-key in one of the components that
; satisfies (condition-predicate KIND-KEY). 
; Otherwise, the result will be #f

(define (condition-property-accessor kind-key prop-key)
  (lambda (exn)
    (let* ((p ((condition-predicate kind-key) exn))
	   (prop-lst (and p (pair? p) (memq prop-key (cdr p)))))
      (and prop-lst (pair? (cdr prop-lst)) (cadr prop-lst)))))
