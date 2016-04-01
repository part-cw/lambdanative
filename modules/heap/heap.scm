;; API
;;

;; Create a heap for a given comparison function PRED
;; and optionally provide a guess at the final size of the heap
(define (make-heap* pred #!key (size-guess 20))
  (make-heap (make-vector (max 1 size-guess) heap-null-value) 0 pred))

;; Returns #t when the heap is empty.
(define (heap-empty? heap)
  (fx= 0 (heap-size heap)))

;; test for the heap null value
(define (heap-null-value? v)
  (eq? v heap-null-value))

;; Add an element to the heap
(define (heap-add! heap el)
  (cond 
   ((heap-empty? heap)
	(heap-expand-by-one heap el))
   (#t 
	(let ((n (heap-size heap))
		  (pred (heap-pred heap))
		  (data #f))
	  (heap-expand-by-one heap el)
	  (set! data (heap-data heap))
	  (let loop 
		  ((n n))
		(let* ((root? (heap-root? n))
			   (val (vector-ref data n))
			   (pn (heap-parent n))
			   (parent-val (if root? #f (vector-ref data pn)))) 
		  (cond 
		   ((heap-root? n) heap)
		   ((pred val parent-val)
			(vector-set! data pn val)
			(vector-set! data n parent-val)
			(loop pn))
		   (#t heap))))))))

;; Add element el only if el is true under `if`
(define (heap-add-if! heap el)
  (if el (heap-add! heap el))
  heap)

;; Take the top of the heap off.
;; Returns a heap-null-value when the heap is empty.
;; to extract the values sorted by pred, pop the heap
;; until it is empty.
(define (heap-pop! heap)
  (if (heap-empty? heap)
	  heap-null-value
	  (let* ((return-value (heap-ref heap 0))
			 (data (heap-data heap))
			 (new-size (fx- (heap-size heap) 1))
			 (last-val (vector-ref data new-size)))
		   (heap-size-set! heap new-size)
		   (vector-set! data new-size heap-null-value)
		   (vector-set! data 0 last-val)
		   (heap-adjust! heap 0)
		   return-value)))

;; Return the object at the head of the heap without popping.
(define (heap-peek heap)
  (if (heap-empty? heap)
	  heap-null-value
	  (heap-ref heap 0)))

;; Implementation 

;; friends don't let friends let nil occupy all types heap-null-value
;; is a value which signals the absence of a value in a heap 
(define heap-null-value (gensym 'heap-null-value))


;; the heap type
(define-type heap data size pred)

(define (heap-empty-fast! heap)
  (heap-size-set! heap 0)
  (let* ((d (heap-data heap))
		 (n (vector-length d))) 
	(do ((i 0 (fx+ i 1)))
		((fx= i n) heap)
	  (vector-set! d i heap-null-value))))

;; double the current maximum size of the heap
(define (heap-double-max-size heap)
  (let* ((old-data (heap-data heap))
		 (n-items (heap-size heap))
		 (new-data (make-vector (fx* 2 (vector-length old-data)) heap-null-value)))
	(heap-data-set! heap
	 new-data)
	(let loop 
		((i 0))
	  (cond 
	   ((fx< i n-items)
		(vector-set! new-data i (vector-ref old-data i))
		(loop (fx+ i 1)))
	   (#t new-data)))
	heap))

;; expand the current heap size by one, placing val in the last node
;; double the size of the heap if needed.
(define (heap-expand-by-one heap val)
  (let* ((max-size (vector-length (heap-data heap)))
		 (current-size (heap-size heap))
		 (new-size (fx+ 1 current-size)))
	(if (> new-size max-size)
		(begin 
		  (heap-double-max-size heap)
		  (heap-expand-by-one heap val))
		(let ((old-size (heap-size heap)))
		  (heap-size-set! heap (fx+ 1 old-size))
		  (vector-set! (heap-data heap) old-size val)))))

;; give index of the left child of i
(define (heap-left i)
  (fx+ 1 (fx* 2 i)))

;; give the index of the right child of i
(define (heap-right i)
  (fx+ 2 (fx* 2 i)))

;; give the index of i's parent
(define (heap-parent i)
  (cond 
   ((odd? i) (quotient (fx- i 1) 2))
   ((even? i) (quotient (fx- i 2) 2))))

;; Return #t when i is the root of the heap
(define (heap-root? i)
  (fx= i 0))

;; Give the node value at N of the heap HEAP
(define (heap-ref heap n)
  (let ((data (heap-data heap))) 
	(if (>= n (heap-size heap))
		heap-null-value
		(vector-ref data n))))

;; give the value to the left of the node n
(define (heap-ref-left heap n)
  (heap-ref heap (heap-left n)))

;; give the value to the right of the node n
(define (heap-ref-right heap n)
  (heap-ref heap (heap-right n)))

;; #t when the n is a leaf of the heap tree
(define (heap-leaf? heap n)
  (and    
   (heap-null-value? (heap-ref heap (heap-left n)))
   (heap-null-value? (heap-ref heap (heap-right n)))))

;; Given a position in the heap, give the position of the smaller of
;; its children if either are smaller than the value at n.
;; give false if the node is a leaf or if both children are larger.
(define (heap-smaller-child-or-false heap n)
  (let ((val (heap-ref heap n))
		(pred (heap-pred heap))
		(left-val (heap-ref-left heap n))
		(right-val (heap-ref-right heap n))
		(smaller-child #f))
	(if (and (not (heap-null-value? left-val)) 
			 (not (pred val left-val)))
		(begin 
		  (set! smaller-child (heap-left n))
		  (set! val left-val)))
	(if (and (not (heap-null-value? right-val))
			 (not (pred val right-val)))
		(set! smaller-child (heap-right n)))
	smaller-child))

;; Adjust the heap starting at n to ensure the heap's invariants hold
(define (heap-adjust! heap n)
  (cond 
   ((heap-leaf? heap n) heap)
   (#t 
	(let ((m-or-false (heap-smaller-child-or-false heap n)))
	  (cond
	   (m-or-false
		(let* ((m m-or-false)
			   (val (heap-ref heap n))
			   (swap-val (heap-ref heap m))
			   (data (heap-data heap)))
		  (vector-set! data n swap-val)
		  (vector-set! data m val)
		  (heap-adjust! heap m)))
	   (#t heap))))))

;; Pop all the elements in the heap out into a list.
(define (heap-empty->list! heap)
  (let loop ((output '()))
	(let ((val (heap-pop! heap)))
	  (if (heap-null-value? val)
		  (reverse output)
		  (loop (cons val output))))))

;; (define (heap-process->empty! f heap)
;;   (let loop ((val (heap-pop! heap)))
;; 	(if (not (heap-null-value? val))
;; 		(begin (f val)
;; 			   (loop (heap-pop! heap)))
;; 		#t)))

(define (heap-process->empty! f heap)
  (let ((n (heap-size heap))) 
	(do ((i 0 (fx+ i 1)))
		((fx= i n) #t)
	  (f (heap-pop! heap)))))




