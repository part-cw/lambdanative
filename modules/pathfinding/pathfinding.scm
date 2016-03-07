(define very-large-integer 1000000)
(define-type grid-node x y walkable? g h (previous unprintable:))
(define (grid-node-reset gn)
  (grid-node-walkable?-set! gn #t)
  (grid-node-g-set! gn very-large-integer)
  (grid-node-h-set! gn very-large-integer)
  (grid-node-previous-set! #f)
  gn)
(define (grid-node-f grid-node)
  (fx+ (grid-node-g grid-node)
	 (grid-node-g grid-node)))

(define-type grid w h (data unprintable:))
(define (grid-index grid x y)
  (let ((w (grid-w grid)))
	(fx+ x (fx* y w))))

(define (grid-ref grid x y)
  (vector-ref (grid-data grid) (grid-index grid x y)))

(define (grid-ref-or grid x y orval)
  (if (grid-on? grid x y)
	  (grid-ref grid x y)
	  orval))

(define (grid-on? grid x y)
  (and (>= x 0)
	   (>= y 0)
	   (fx< x (grid-w grid))
	   (fx< y (grid-h grid))))

(define (grid-ref-walkable-only-or grid x y)
  (if (or (not (grid-on? grid x y))
		  (and (grid-on? grid x y) 
			   (not (grid-walkable-at? grid x y))))
	  #f
	  (grid-ref grid x y)))

(define (grid-walkable-at? grid x y)
  (grid-node-walkable? (grid-ref grid x y)))

(define (grid-set-one! grid i j v)
  (vector-set! (grid-data grid) 
			   (grid-index grid i j) 
			   v))

(define (grid-for-each-pair fun lst)
  (let* ((count (length lst)))
	(if (not (fx= 0 (modulo count 2)))
		(error "for-each-pair requires a list whose length is divisible by two.")
		(let loop ((pairs lst)
				   (remaining count))
		  (if (fx= 0 remaining) #t
			  (begin 
				(fun (car pairs)
					 (cadr pairs))
				(loop (cddr pairs)
					  (fx- remaining 2))))))))

(define (grid-for-each-triple fun lst)
  (let* ((count (length lst)))
	(if (not (fx= 0 (fxmodulo count 3)))
		(error "for-each-triple requires a list whose length is divisible by three.")
		(let loop ((triples lst)
				   (remaining count))
		  (if (fx= 0 remaining) #t
			  (begin 
				(fun (car triples)
					 (cadr triples)
					 (caddr triples))
				(loop (cdddr triples)
					  (fx- remaining 3))))))))

(define (grid-set! grid #!rest triples)
  (grid-for-each-triple (lambda (i j v)
					 (grid-set-one! grid i j v))
				   triples))

(define (grid-forbid-single! grid i j)
  (grid-node-walkable?-set! (grid-ref grid i j) #f))

(define (grid-forbid! grid #!rest pairs)
  (grid-for-each-pair (lambda (i j)
						(grid-forbid-single! grid i j))
					  pairs))

(define (grid-allow-single! grid i j)
  (grid-node-walkable?-set! (grid-ref grid i j) #t))

(define (grid-allow! grid #!rest pairs)
  (grid-for-each-pair (lambda (i j)
						(grid-allow-single! grid i j))
					  pairs))


(define (make-grid* w h #!optional (init (lambda (i j)
										   (make-grid-node 
											i j #t
											very-large-integer very-large-integer
											#f))))
  (let* ((vec (make-vector (fx* w h)))
		 (grid (make-grid w h vec)))
	(let loop-x 
		((x 0))
	  (if (>= x w)
		  grid
		  (begin
			(let loop-y ((y 0))
			  (if (>= y h)
				  grid
				  (begin 
					(grid-set! grid x y (init x y))
					(loop-y (fx+ y 1)))))
			(loop-x (fx+ x 1)))))))

(define (grid-for-each-node f grid)
  (let* ((data (grid-data grid))
		 (n (vector-length data))) 
	(do ((i 0 (fx+ i 1)))
		((fx= i n) grid)
	  (f (vector-ref data i)))))
;; (define (grid-for-each-node f grid)
;;   (let ((w (grid-w grid))
;; 		(h (grid-w grid)))
;; 	(let loop-y
;; 		((y 0))
;; 	  (if (< y h)
;; 		  (begin 
;; 			(let loop-x
;; 				((x 0))
;; 			  (if (< x w)
;; 				  (begin 
;; 					(f (grid-ref grid x y))
;; 					(loop-x (+ x 1)))
;; 				  grid))
;; 			(loop-y (+ y 1)))
;; 		  grid))))

(define (display-grid grid)
  (let ((w (grid-w grid))
		(h (grid-w grid))) 
	(let yloop ((y 0))
	  (if (fx< y h) 
		  (begin 
			(let xloop ((x 0))
			 (if (fx< x w)
				 (begin (display 
						 (if (grid-walkable-at? grid x y) "O" "X"))
						(xloop (fx+ x 1)))
				 (newline)))
			(yloop (fx+ y 1)))
		  grid))))

(define (node< n1 n2)
  (fx< (grid-node-f n1)
	 (grid-node-f n2)))

(define (manhattan-distance x0 y0 xf yf)
  (fx+ (abs (fx- x0 xf))
	 (abs (fx- y0 yf))))

(define (a*-coords-eq? x1 y1 x2 y2)
  (and (fx= x1 x2)
	   (fx= y1 y2)))

;; (define (grid-walkable-neighbors grid node)
;;   (let ((x (grid-node-x node))
;; 		(y (grid-node-y node)))
;; 	(let loop ((pairs '(-1 0 1 0 0 -1 0 1))
;; 			   (nodes '()))
;; 	  (if (eq? '() pairs)
;; 		  nodes
;; 		  (let ((node (grid-ref-walkable-only-or 
;; 					   grid
;; 					   (+ x (car pairs))
;; 					   (+ y (cadr pairs)))))
;; 			(if node 
;; 				(loop (cddr pairs)
;; 					  (cons node nodes))
;; 				(loop (cddr pairs)
;; 					  nodes)))))))

(define (grid-walkable-neighbors grid node)
  (let* ((x (grid-node-x node))
		 (y (grid-node-y node))
		 (x+1 (fx+ x 1))
		 (x-1 (fx- x 1))
		 (y+1 (fx+ y 1))
		 (y-1 (fx- y 1))
		 (output (list))
		 (node #f))
	(set! node (grid-ref-walkable-only-or 
				grid
				x y+1))
	(if node (set! output (cons node output)))

	(set! node (grid-ref-walkable-only-or 
				grid
				x y-1))
	(if node (set! output (cons node output)))

	(set! node (grid-ref-walkable-only-or 
				grid
				x+1 y))
	(if node (set! output (cons node output)))

	(set! node (grid-ref-walkable-only-or 
				grid
				x-1 y))
	(if node (set! output (cons node output)))
	output))

(define open-set (make-heap* node< size-guess: (fx* 18 18)))
(define (a**! xs ys xf yf grid)
  (heap-empty-fast! open-set)
  (if (not (grid-walkable-at? grid xs ys))
	  (error (list "Grid is marked as unwalkable at start position: " xs ys)))
  (if (not (grid-walkable-at? grid xf yf))
	  (error (list "Grid is marked as unwalkable at end position: " xf yf)))
  (heap-add! open-set (grid-ref grid xs ys))
  (let ((start-node (grid-ref grid xs ys)))
	(grid-node-g-set! start-node 0))
  (let while ((current (heap-pop! open-set)))
	(cond 
	 ((heap-null-value? current)
	  #f)
	 ((a*-coords-eq? xf yf (grid-node-x current) (grid-node-y current))
	  (grid-node-reconstruct-path current))
	 (#t 
	  (let loop ((neighbors (grid-walkable-neighbors grid current)))
		(if (eq? neighbors '())
			#t
			(let* 
				((neighbor (car neighbors))
				 (tentative-g (fx+ 
							   (grid-node-g current)
							   (manhattan-distance (grid-node-x current)
												   (grid-node-y current)
												   (grid-node-x neighbor)
												   (grid-node-y neighbor))))
				 (neighbors (cdr neighbors)))			  
			  (if (fx< tentative-g (grid-node-g neighbor))
				  (begin 
					(heap-add! open-set neighbor)
					(grid-node-previous-set! neighbor current)
					(grid-node-g-set! neighbor tentative-g)
					(grid-node-h-set! neighbor (manhattan-distance 
												(grid-node-x neighbor)
												(grid-node-y neighbor)
												xf yf)))
				  #t)
			  (loop neighbors))))
	  (while (heap-pop! open-set))))))

(define (a*! xs ys xf yf grid)
  (heap-empty-fast! open-set)
  (if (not (grid-walkable-at? grid xs ys))
	  (error (list "Grid is marked as unwalkable at start position: " xs ys)))
  (if (not (grid-walkable-at? grid xf yf))
	  (error (list "Grid is marked as unwalkable at end position: " xf yf)))
  (heap-add! open-set (grid-ref grid xs ys))
  (let ((start-node (grid-ref grid xs ys)))
	(grid-node-g-set! start-node 0))
  (let ((neighbor #f)
		(tentative-g #f)) 
	(do ((current (heap-pop! open-set) (heap-pop! open-set)))
		((or (heap-null-value? current)
			 (a*-coords-eq? xf yf (grid-node-x current) (grid-node-y current))) 
		 (if (heap-null-value? current) #f (grid-node-reconstruct-path current)))
	  (do ((neighbors (grid-walkable-neighbors grid current) (cdr neighbors)))
		  ((eq? '() neighbors) #t)
		(set! neighbor (car neighbors))
		(set! tentative-g (fx+ 
						   (grid-node-g current)
						   (manhattan-distance (grid-node-x current)
											   (grid-node-y current)
											   (grid-node-x neighbor)
											   (grid-node-y neighbor))))
		(if (fx< tentative-g (grid-node-g neighbor))
			(begin 
			  (heap-add! open-set neighbor)
			  (grid-node-previous-set! neighbor current)
			  (grid-node-g-set! neighbor tentative-g)
			  (grid-node-h-set! neighbor (manhattan-distance 
										  (grid-node-x neighbor)
										  (grid-node-y neighbor)
										  xf yf)))
			#t)
		))))



(define (grid-node-reconstruct-path node)
  (let loop ((path (list (list (grid-node-x node)
							   (grid-node-y node))))
			 (node node))
	(let ((previous (grid-node-previous node)))
	  (cond 
	   (previous (loop (cons (list (grid-node-x previous)
								   (grid-node-y previous)) path)
					   previous))
	   (#t path)))))

(define (grid-reset-scores! grid)
  (grid-for-each-node 
   (lambda (node)
	 (grid-node-g-set! node very-large-integer)
	 (grid-node-h-set! node very-large-integer)
	 (grid-node-previous-set! node #f))
   grid))

(define (grid-reset! grid)
  (grid-for-each-node 
   (lambda (node)
	 (grid-node-g-set! node very-large-integer)
	 (grid-node-h-set! node very-large-integer)
	 (grid-node-walkable?-set! node #t)
	 (grid-node-previous-set! node #f))
   grid))

(define (display-grid-and-path grid path)
  (define (point-in-path? x y path)
	(cond ((eq? path '())
		   #f)
		  ((and (fx= (car (car path)) x)
				(fx= (cadr (car path)) y))
		   #t)
		  (#t 
		   (point-in-path? x y (cdr path)))))
  (if path
	  (let ((w (grid-w grid))
			(h (grid-w grid))) 
		(let yloop ((y 0))
		  (if (fx< y h) 
			  (begin 
				(let xloop ((x 0))
				  (if (fx< x w)
					  (begin (display 
							  (cond 
							   ((not (grid-walkable-at? grid x y)) "X")
							   ((point-in-path? x y path)
								"+")
							   (#t ".")))
							 (xloop (fx+ x 1)))
					  (newline)))
				(yloop (fx+ y 1)))
			  grid)))
	  (display-grid grid)))

(define (grid-north x y)
  (list x (fx- y 1)))

(define (grid-south x y)
  (list x (fx+ y 1)))

(define (grid-east x y)
  (list (fx+ x 1) y))

(define (grid-west x y)
  (list (fx- x 1) y))

(define (grid-forbid-run! grid sx sy dir len)
  (let loop ((node (grid-ref-or grid sx sy #f))
			 (len len))
	(cond 
	 ((and node
		   (fx> len 0))
	  (let ((x (grid-node-x node))
			(y (grid-node-y node))) 
		(grid-forbid-single! 
			   grid 
			   x
			   y)
	   (loop (let ((new-pos (dir x y)))
			   (grid-ref-or grid (car new-pos) (cadr new-pos) #f))
			 (fx- len 1))))
	 (#t 'done))))

(define (grid-filter-nodes f grid)
  (let ((output '()))
	(grid-for-each-node 
	 (lambda (node)
	   (if (f node)
		   (set! output (cons node output))
		   #f))
	 grid)
	(reverse output)))

(define (grid-always-true node)
  #t)

;; Note - if a heap is passed in, make sure it is empty.
(define (grid-nodes-ordered-by f grid #!key 
							   (filter grid-always-true) 
							   (heap (make-heap* f size-guess: (vector-length (grid-data grid)))))
  (grid-for-each-node 
   (lambda (node)
	 (if (filter node)
		 (heap-add! node)
		 #t)))
  (heap-empty->list! heap))
