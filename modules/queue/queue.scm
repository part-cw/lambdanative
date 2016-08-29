(define-type qnode val (prev unprintable:) (next unprintable:))
(define-type q first last count)

(define (make-q*)
  (make-q #f #f 0))

(define (q-push! q el)
  (let ((first (q-first q)))
	(cond 
	 ((eq? #f first)
	  (let ((new-node (make-qnode el #f #f)))
		(q-first-set! q new-node)
		(q-last-set! q new-node)))
	 (else 
	  (let ((new-node (make-qnode el #f first)))
		(qnode-prev-set! first new-node)
		(q-first-set! q new-node))))
	(q-count-set! q (+ 1 (q-count q)))
	q))

(define (q-pop! q #!optional (or-value #f))
  (let ((last (q-last q)))
	(cond
	 ((eq? #f last) or-value)
	 (else
	  (let ((element (qnode-val last))
			(prev (qnode-prev last)))
		(cond 
		 ((eq? prev #f)
		  (q-first-set! q #f)
		  (q-last-set! q #f)
		  (q-count-set! q 0))
		 (else 
		  (q-last-set! q prev)
		  (q-count-set! q (- (q-count q) 1))))
		element)))))

(define (q-push-limited! q el max)
  (let ((count (q-count q)))
	(if (< count max)
		(q-push! q el)
		#f)))

(define (q-for-each q f)
  (do ((el (q-first q) (qnode-next el)))
	  ((eq? el #f) done:)
	(f (qnode-val el))))

(define (q->list q)
  (let ((lst '()))
	(q-for-each q (lambda (el)
					(set! lst (cons el lst))))
	(reverse lst)))

(define (list->q l)
  (define (loop l q)
    (if (eq? l '())
	q
	(loop (cdr l)
	      (q-push! (car l)))))
  (loop l (make-q*)))

(define (display-q q)
  (display "<queue (")
  (display (q-count q))
  (display "): ")
  (q-for-each q (lambda (el)
				  (display el) (display " ")))
  (display ">")
  (newline))



