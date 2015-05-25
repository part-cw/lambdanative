; *************************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; *************************************************************************

(##namespace ("" sort))

(define gsort (lambda (predicate list) (sort list predicate)))

(define ??? 'unspecified-result)

(define list*
  (lambda args
    (letrec ((chase
	      (lambda (args)
		(cond ((null? args) '())
		      ((null? (cdr args)) (car args))
		      (else (cons (car args) (chase (cdr args))))))))
      (chase args))))

(define apply*
    (lambda (proc . args)
      (apply proc (apply list* args))))

(define position-of
    (lambda (x lst)
      (if (eq? x (car lst)) 0 (+ 1 (position-of x (cdr lst))))))

(define map-append
    (lambda (proc . lists)
      (apply append (apply map (cons proc lists)))))

(define last
    (lambda (l)
      (if (null? l)
	  #f
	  (if (null? (cdr l))
	      (car l)
	      (last (cdr l))))))

(define every
    (lambda (test . lists)
      (let scan ((tails lists))
	(if (member #t (map null? tails))             ;(any null? lists)
	    #t
	    (and (apply test (map car tails))
		 (scan (map cdr tails)))))))

(define remove
    (lambda (x list)
      (cond ((null? list) '())
	    ((eq? (car list) x) (cdr list))
	    (else (cons (car list) (remove x (cdr list)))))))

(define getl
    (lambda (initargs name . not-found)
      (letrec ((scan (lambda (tail)
		       (cond ((null? tail)
			      (if (pair? not-found)
				  (car not-found)
				  (error "GETL couldn't find" name)))
			     ((eq? (car tail) name) (cadr tail))
			     (else (scan (cddr tail)))))))
	(scan initargs))))

(define union
    (lambda lists
      (letrec ((clean (lambda (list result)
			(cond ((null? list) result)
			      ((memq (car list) result)
			       (clean (cdr list) result))
			      (else
			       (clean (cdr list) (cons (car list) result)))))))
	(clean (apply append lists) '()))))

(define collect-if
    (lambda (test? list)
      (cond ((null? list) '())
	    ((test? (car list)) (cons (car list) (collect-if test? (cdr list))))
	    (else (collect-if test? (cdr list))))))

;(define remove-unless
;    (lambda (test list)
;      (if (null? list)
;	  ()
;	  (let ((rest (remove-unless test (cdr list))))
;	    (if (test (car list))
;		(cons (car list) rest)
;		rest)))))

(define remove-duplicates
    (lambda (list)
      (let loop ((result-so-far '())
		 (remaining list))
	   (if (null? remaining)
	       result-so-far
	       (if (null? (memq (car remaining) result-so-far))
		   (loop (cons (car remaining) result-so-far)
			 (cdr remaining))
		   (loop result-so-far
			 (cdr remaining)))))))




;
; A simple topological sort.
;
; It's in this file so that both TinyClos and Objects can use it.
;
; This is a fairly modified version of code I originally got from Anurag
; Mendhekar <anurag@moose.cs.indiana.edu>.
;
;

(define compute-std-cpl
    (lambda (c get-direct-supers)
      (top-sort ((build-transitive-closure get-direct-supers) c)
		((build-constraints get-direct-supers) c)
		(std-tie-breaker get-direct-supers))))


(define top-sort
    (lambda (elements constraints tie-breaker)
      (let loop ((elements    elements)
		 (constraints constraints)
		 (result      '()))
	(if (null? elements)
	    result
	    (let ((can-go-in-now
		    (collect-if
		      (lambda (x)
			(every (lambda (constraint)
				 (or (not (eq? (cadr constraint) x))
				     (memq (car constraint) result)))
			       constraints))
		      elements)))
	      (if (null? can-go-in-now)
		  (error 'top-sort "Invalid constraints")
		  (let ((choice (if (null? (cdr can-go-in-now))
				    (car can-go-in-now)
				    (tie-breaker result
						 can-go-in-now))))
		    (loop
		      (collect-if (lambda (x) (not (eq? x choice)))
			          elements)
		      constraints
		      (append result (list choice))))))))))

(define std-tie-breaker
    (lambda (get-supers)
      (lambda (partial-cpl min-elts)
	(let loop ((pcpl (reverse partial-cpl)))
	     (let ((current-elt (car pcpl)))
	       (let ((ds-of-ce (get-supers current-elt)))
		 (let ((common (collect-if (lambda (x)
					     (memq x ds-of-ce))
					   min-elts)))
		   (if (null? common)
		       (if (null? (cdr pcpl))
			   (error 'std-tie-breaker "Nothing valid")
			   (loop (cdr pcpl)))
		       (car common)))))))))


(define build-transitive-closure
    (lambda (get-follow-ons)
      (lambda (x)
	(let track ((result '())
		    (pending (list x)))
	     (if (null? pending)
		 result
		 (let ((next (car pending)))
		   (if (memq next result)
		       (track result (cdr pending))
		       (track (cons next result)
			      (append (get-follow-ons next)
				      (cdr pending))))))))))

(define build-constraints
  (lambda (get-follow-ons)
    (lambda (x)
      (let loop ((elements ((build-transitive-closure get-follow-ons) x))
		 (this-one '())
		 (result '()))
	   (if (or (null? this-one) (null? (cdr this-one)))
	       (if (null? elements)
		   result
		   (loop (cdr elements)
			 (cons (car elements)
			       (get-follow-ons (car elements)))
			 result))
	       (loop elements
		     (cdr this-one)
		     (cons (list (car this-one) (cadr this-one))
			   result)))))))

;; eof
