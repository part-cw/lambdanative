;; This software is released under the terms of the MIT License.  See
;; the LICENSE file in this repository.

;; for debugging - display, newline and flush
(define (dbg #!rest args)
  (display args)
  (newline)
  (force-output))

;; a table to hold optional component names
(define *component-name-map* (make-table test: equal?))
(define (name-component component-id #!optional (name #f))
  (table-set! 
   *component-name-map*
   component-id
   (if name 
       name
       (string-append "unnamed-component-" 
		      (number->string component-id)))))
(define (get-component-name component-id)
  (table-ref *component-name-map* component-id "unnamed-component"))

;; ****************************************
;; BEGIN API
;; 
;; Scran can enter inconsistent states if 
;; non-api functions are used.
;;
;; ****************************************

;; create a component.  The constructor should take the entity itself,
;; plus the values to initialize the component, and return the component.
(define (component! constructor #!optional (name #f))
  (set! components (addv components constructor))
  (let ((component-id (fx- (vector-length components) 1)))
    (name-component component-id name)
    component-id))

;; create a system.

;; local-components is the list of components which constitute
;; membership in this system
;; enter is called on entities entering the system, after they 
;; have entered
;; exit is called on entities exiting the system, before they have
;; exited.
;; pre is called before every, during system-execute
;; post is called after every, during system-execute
;; every is called on each component of the system during system-execute

(define (system! local-components #!key 
				 (first #f)
				 (last #f)
				 (enter #f)
				 (exit #f)
				 (pre #f)
				 (post #f)
				 (every #f))
  (set! systems (addv systems (make-system (components-list->vector local-components) 
										   local-components
										   first 
										   last 
										   enter 
										   exit 
										   pre 
										   post 
										   every)))
  (fx- (vector-length systems) 1))


;; create an entity and optionally add components to it.  
;; each argument is a list whose first element is a component id and
;; whose subsequent elements are passed to that component constructor.
(define (entity! #!rest components)
  (let ((ent (entity-raw!)))
	(let loop ((components components))
	  (cond
	   ((eq? (list) components)
		ent)
	   (#t 
		(apply add-component! ent (car components))
		(loop (cdr components)))))))

;; Add a component c to entity e, with arguments to construct the component args
;; If this addition causes the system membership of e to change, 
;; this function adds it to the implied system or systems,
;; calling entry functions as needed.
(define (add-component! e c #!rest args)
  (let ((anti-systems (list-anti-systems e)))
	(apply add-component-raw! e c args)
	(for-each 
	 (lambda (anti-system)
	   (if (entity-in-system? e anti-system)
		   (begin 
			 (add-to-system-raw! e anti-system))
		   #f))
	 anti-systems)))

;; Remove a component c from entity e
;; When such a remove implies a removal from associated systems,
;; also remove the entity from those systems, calling exit functions 
;; as needed.
(define (remove-component! e c)
  (if (not (nullcomp? (vector-ref (entity-components e) c)))
	  (let ((entity-systems (list-systems e)))
		(for-each 
		 (lambda (si)
		   (let* ((system (vector-ref systems si)))
			 (if (system-uses-component? system c)
				 (remove-from-system-raw! e si)
				 #f)))
		 entity-systems)
		(nullify-entity-component! e c))))

;; There is no independent tracking of entities in scran so deleting
;; an entity amounts to nothing more than removing all of its components
;; the user must throw away any references so the GC can reclaim the object.
;; Alternatively, the entity can return to a pool and be reused
(define (delete! entity)
  (if entity
	  (for-each (lambda (c)
				  (remove-component! entity c))
				(get-component-list entity)))
  #f)

;; Set the entity component c to value v
;; it is an error to call this on a nullcomp component.
(define (entity-component-set! e c v)
  (let ((current-value (entity-component-raw e c)))
	(if (not (nullcomp? current-value))
		(entity-component-set-raw! e c v)
		(error "Can't set empty component."))))

;; Fetch the entity component at c or return o, if it is nullcomp
(define (entity-component-or e c o)
  (let ((r (entity-component-raw e c)))
	(if (nullcomp? r) o r)))

;; a utility function which returns the value v, dropping e
;; eg: (define simple-value-component (component! comp-id))
(define (comp-id e v)
  v)

;; #t when this entity has the component c
(define (entity-has-component? e c)
  (not (eq? nullcomp (entity-component-raw e c))))

;; #t when entity doesn't have the component c
(define (entity-lacks-component? e c)
  (eq? nullcomp (entity-component-raw e c)))

;; alias for entity-has-component?
(define (entity-component? e c)
  (not (nullcomp? (entity-component-raw e c))))

;; use the function f to transform the entity-component c
;; f takes the old c value and returns the new one
(define (transform-entity-component e c f)
  (let ((cval (entity-component-raw e c)))
	(if (not (nullcomp? cval))
		(entity-component-set-raw! e c (f cval))
		(error "Tried to transform a component which an entity does not have."))))

;; if entity has component c, call body on it and return the result.
(define (maybe-do-with-component e c body)
  (let ((cval (entity-component-raw e c)))
	(if (not (nullcomp? cval))
		(body cval)
		cval)))

;; call body on the value stored for entity at component c, even if it
;; is nullcomp
(define (do-with-component e c body)
  (body (entity-component e c)))

;; as `do-with-component` except that if the entity does not have such
;; a component, it is created with `comp-args` before the body is
;; executed.
(define (do-with-component/add e c body #!rest comp-args)
  (let ((c (entity-component-raw e c)))
	(if (nullcomp? c)
		(apply add-component! e c comp-args)
		#t)
	(do-with-component e c body)))

;; as with `do-with-component` except if the component doesn't exist,
;; use orval instead as the argument to body.
(define (do-with-component-or e c orval body)
  (body (entity-component-or e c orval)))

;; get the entity-component at c, error if it is nullcomp
(define (entity-component e c)
  (let ((cval (entity-component-raw e c)))
	(if (not (nullcomp? cval))
		cval
		(error (string-append "Tried to get an entity component which did not exist. "
				      (get-component-name e))))))

;; Returns #t when entity e BELONGS in system S, regardless of whether
;; it is actually in the linked list for that system.
;; When used from the API, it is impossible for an entity to belong in
;; a system and not be in it.
(define (entity-in-system? e s)
  (let* ((ecs (entity-components e)) 
		 (scs (system-components (vector-ref systems s)))
		 (max-n (vector-length ecs)))
	(letrec ((loop (lambda (i)
					 (if (fx< i max-n)
						 (let ((sc (vector-ref scs i)))
						   (cond 
							((not sc) (loop (fx+ i 1)))
							((nullcomp? (vector-ref ecs i)) #f)
							(#t (loop (fx+ i 1)))))
						 #t))))
	  (loop 0))))

;; Returns #t when the list of components would belong in system s.
(define (components-in-system? components-list s)
  (let* ((ecs (components-list->vector components-list)) 
		 (scs (system-components (vector-ref systems s)))
		 (max-n (vector-length ecs)))
	(letrec ((loop (lambda (i)
					 (if (fx< i max-n)
						 (let ((sc (vector-ref scs i)))
						   (cond 
							((not sc) (loop (fx+ i 1)))
							((nullcomp? (vector-ref ecs i)) #f)
							(#t (loop (fx+ i 1)))))
						 #t))))
	  (loop 0))))

;; List the systems to which the entity belongs.
(define (list-systems entity)
  (let ((n (vector-length systems)))
	(letrec ((loop 
			  (lambda (i o) 
				(if (fx< i n)
					(loop (fx+ i 1)
						  (if (entity-in-system? entity i)
							  (cons i o)
							  o))
					o))))
	  (loop 0 (list)))))

;; List the systems to which the entity does not belong.
(define (list-anti-systems entity)
  (let ((n (vector-length systems)))
	(letrec ((loop 
			  (lambda (i o) 
				(if (fx< i n)
					(loop (fx+ i 1)
						  (if (not (entity-in-system? entity i))
							  (cons i o)
							  o))
					o))))
	  (loop 0 (list)))))

;; Call the function f on e and the components specified by comps
(define (call-with-components f e comps)
  (apply f e (component-values e comps)))

;; Call the function f on the entity e and the components implied by system s.
(define (call-with-system-components f e s)
  (let ((components-list (system-components-order (vector-ref systems s))))
	(call-with-components f e components-list)))


(define (entity-has-been-deleted-from-system? e s)
  (and (eq? #f (get-system-link-next e s))
	   (eq? #f (get-system-link-prev e s))
	   (not (eq? (system-first (vector-ref systems s)) e))))
;; apply f for side effects to each entity in s
;; use `call-with-system-components` so that f is a function which expects
;; the entity itself and then one argument for each
;; system component, in the order defined by the system
;; declaration.
(define (system-for-each-entity f s)
  (let* ((si s)
		 (s (vector-ref systems si)))
	(letrec ((get-next-node (lambda (previous-nodes)
							  (cond 
							   ((eq? previous-nodes '()) 
								(let ((first (system-first s)))
								  (if first (get-system-link-next first si) #f)))
							   ((not (entity-has-been-deleted-from-system? (car previous-nodes) si))
								;; previous node still in list
								(get-system-link-next (car previous-nodes) si))
							   (#t (get-next-node (cdr previous-nodes)))))))
	  (let loop ((node (system-first s))
				 (previous-nodes '()))
   (cond
	(node (call-with-system-components f node si) 
		  (if (entity-has-been-deleted-from-system? node si) 
			  (loop (get-next-node previous-nodes) previous-nodes)
			  (loop (get-system-link-next node si) (cons node previous-nodes))))
	(#t #t))))))

;; A sigil value to terminate system-for-each-entity*
(define scran-stop-value (list 0))

;; Exactly as system-for-each-entity except that if 
;; f returns scran-stop-value, the iteration is aborted.
(define (system-for-each-entity* f s)
  (let* ((si s)
		 (s (vector-ref systems si)))
	(let loop ((node (system-first s)))
	  (cond
	   (node (let ((rval (call-with-system-components f node si)))
			   (if (not (eq? rval scran-stop-value)) 
				   (loop (get-system-link-next node si))
				   #t)))
	   (#t #t)))))

;; as in `system-for-each-entity` but the results are collected
;; into a list.
(define (system-map-entities f s)
  (let* ((si s)
		 (s (vector-ref systems si)))
	(let loop ((node (system-first s))
			   (out (list)))
	  (cond
	   (node (loop (get-system-link-next node si) (cons (call-with-system-components f node si) out)))
	   (#t (reverse out))))))

;; as in `system-for-each-entity` but the results are collected.  If
;; predicate is false, no result is calculated or returned for that entity.
(define (system-filter-map-entities predicate transform s)
  (let* ((si s)
		 (s (vector-ref systems si)))
	(let loop ((node (system-first s))
			   (out (list)))
	  (cond
	   (node  
	    (if (call-with-system-components predicate node si)
		(loop (get-system-link-next node si) (cons (call-with-system-components transform node si) out))
		(loop (get-system-link-next node si) out)))
	   (#t (reverse out))))))

;; Apply f to the entity and appropriate components and collect just the entity if the result is true.
(define (system-filter-entities f s)
  (let ((output (list))) 
	(system-for-each-entity 
	 (lambda (#!rest args)
	   (if (apply f rest)
		   (set! output (cons (car args) output))))
	 s)
	(reverse output)))

;; reduce the entities in system s with reducer f, starting with
;; initial value init.
;; f takes the accumlator, then the entity, then the
;; component values of system s
(define (system-reduce-entities f s init)
  (let ((ac init))
	(system-for-each-entity (lambda (#!rest rest)
							  (set! ac (apply f ac rest))) s)
	ac))

;; Return the count of entities in system s
(define (system-count-entities s)
  (system-reduce-entities (lambda (ac #!rest rest)
							(fx+ ac 1))
						  s
						  0))

;; perform the default behavior of system s,
;; which is 
;; 1. execute the `pre` function, if it exists.
;; 2. execute the `every` function on each entity, if it exists.
;; 3. execute the `post` function if it exists.
(define (system-execute s)
  (let* ((si s)
		 (s (vector-ref systems si))
		 (pre (system-pre s))
		 (post (system-post s))
		 (every (system-every s)))
	(if pre (pre) #f)
	(if every (system-for-each-entity every si) #f)
	(if post (post) #f)
	#t))

;; return the entity ids of the entities in system s
;; this is purely informative
(define (system-entity-ids s)
  (system-map-entities (lambda (e #!rest rest) (entity-id e)) s))


;; Count all the entities tracked by scran.
(define (total-entity-count)
  (let ((n (vector-length systems)))
	(let loop ((total 0)
		  (i 0))
	  (if (fx= i n)
		  total
		  (loop (+ total (system-count-entities i))
				(+ i 1))))))

;; This removes all entities from all systems.  Assuming that entities
;; are not captured by other contexts, this results in the garbage
;; collection of all entities.
(define (reset-all-systems!)
  (let ((n (vector-length systems))) 
	(let outer-loop ((count (total-entity-count)))
	  (cond 
	   ((fx= count 0)
		#t)
	   (else 
		(let loop ((i 0))
		  (cond 
		   ((fx= i n)
			#t)
		   (else
			;; Save the trouble of all the backtracking logic implied in deleting during traversal.
			(let ((entities (system-map-entities (lambda (e #!rest ignore) e) i)))
			  (map delete! entities)
			  (loop (fx+ i 1))))))
		(outer-loop (total-entity-count)))))))

;; ************************************************
;;
;; BEGIN SUPPORT CODE
;;
;; Code in this section is implementation oriented
;;
;; ************************************************

;; foldl over elements of vector v
;; (a b -> b) [a] b -> b
(define (vector-foldl f v init)
  (let ((n (vector-length v)))
	(let loop ((i 0)
			   (ac init))
	  (if (fx< i n)
		  (loop (fx+ i 1)
				(f (vector-ref v i) ac))
		  ac))))

;; global vector holding our system definitions
(define systems (make-vector 0 #f))

;; global vector holding our component definitions
(define components (make-vector 0 #f))

;; reset the entire universe.  Old entities will have unspecified
;; behavior after a reset.
(define (reset!)
  (set! systems (make-vector 0 #f))
  (set! components (make-vector 0 #f)))

;; Append VAL to VEC, resulting in a new VEC
(define (addv vec val)
  (vector-append vec (vector val)))

;; A link in a doubly linked list
;; prev and next point to entities 
(define-type link (prev unprintable:) (next unprintable:))
(define-type system components components-order first last enter exit pre post every)

;; return #t when system `system` used component `component`
;; NOTE: system here is the structure itself, not the integer ID
;; of the system.  
;; NOTE: component is the integer ID of the component.
;; this is not part of the API
(define (system-uses-component? system component)
  (not (not (vector-ref (system-components system) component))))

;; #t when the system is empty
;; NOTE: this uses the system struct itself, rather than 
;; the system id.
(define (system-empty? s)
  (eq? #f (system-last s)))

;; Add an entity to the end of the system represented by S
;; NOTE: s is the system ID, not the system struct
;; NOTE: entity is added whether it makes sense to add it to this system or not
;; NOTE: this is not an API function.
(define (system-add-to-end s e)
  (let* ((si s)
		(s (vector-ref systems si))) 
	(cond 
	 ((system-empty? s)	  
	  (system-first-set! s e)
	  (system-last-set! s e)
	  (set-system-link-prev! e si #f)
	  (set-system-link-next! e si #f))
	 (#t 
	  (let* ((last (system-last s)))
		(system-last-set! s e)
		(set-system-link-next! last si e)
		(set-system-link-next! e si #f)
		(set-system-link-prev! e si last))))))

;; return the link associated with system id S in entity E
;; Scran works by maintaining a linked list of entities for 
;; system scran is managing.  That is, each entity has an array 
;; of links, one for each system.
;;
;; This is a memory/speed trade.
(define (get-system-link e s)
  (let* ((links (entity-system-links e))
		 (res (vector-ref links s)))
	(cond 
	 (res res)
	 ((not res)
	  (let ((lnk (make-link #f #f)))
		(vector-set! links s lnk)
		lnk)))))

;; Get the previous entity in the system s.
(define (get-system-link-prev e s)
  (link-prev (get-system-link e s)))

;; Get the next entity in the system s
(define (get-system-link-next e s)
  (link-next (get-system-link e s)))

;; set the previous entity in the system s to prev
(define (set-system-link-prev! e s prev)
  (link-prev-set! (get-system-link e s) prev))

;; set the next entity in the system s to next
(define (set-system-link-next! e s next)
  (link-next-set! (get-system-link e s) next))

;; remove the entity e from the system s
(define (system-remove s e)
  (let* ((si s)
		 (s (vector-ref systems si))
		 (first (system-first s))
		 (last (system-last s)))
	(cond 
	 ((and (eq? e first)
		   (eq? e last))
	  (system-first-set! s #f)
	  (system-last-set! s #f)
	  (set-system-link-prev! e si #f)
	  (set-system-link-next! e si #f))
	 ((eq? e first)
	  (system-first-set! s (get-system-link-next e si))
	  (set-system-link-prev! (system-first s) si #f)
	  (set-system-link-prev! e si #f)
	  (set-system-link-next! e si #f))
	 ((eq? e last)
	  (system-last-set! s (get-system-link-prev e si))
	  (set-system-link-next! (get-system-link-prev e si) si #f)
	  (set-system-link-prev! e si #f)
	  (set-system-link-next! e si #f))
	 (#t 
	  (set-system-link-next! (get-system-link-prev e si) si (get-system-link-next e si))
	  (set-system-link-prev! (get-system-link-next e si) si (get-system-link-prev e si))
	  (set-system-link-prev! e si #f)
	  (set-system-link-next! e si #f)))))


;; find the constructor for component component
(define (get-component-constructor component)
  (vector-ref components component))

;; create a component which takes no values
;; useful for tagging things.
(define (tag!)
  (component! #t))

;; returns #t when entity e has tag #t
(define (entity-has-tag? e tag)
  (not (eq? nullcomp (entity-component-raw e tag))))

;; alias for above
(define tagged? entity-has-tag?)

;; #t when entity e does not have the tag tag
(define (not-tagged? e tag)
  (nullcomp? (entity-component-raw e tag)))

;; given a list of components, convert to a vector 
;; with length equal to the number of defined components,
;; with #t where the component is in components.
(define (components-list->vector cs)
  (let ((v (make-vector (vector-length components) #f)))
	(for-each (lambda (i)
				(vector-set! v i #t))
			  cs)
	v))

;; Get the list of components which an entity has
;; as a list of component ids.
(define (get-component-list entity)
  (let* ((ecs (entity-components entity))
		 (n (vector-length ecs))) 
	(let loop ((i 0)
			   (out (list)))
	  (cond 
	   ((fx< i n)
		(loop (fx+ i 1) 
			  (if (not (nullcomp? (vector-ref ecs i)))
				  (cons i out)
				  out)))
	   (#t (reverse out))))))


;; Nullcomp is a unique value indicating that an entity's component is null
(define nullcomp '(0))
;; test for nullcomp
(define (nullcomp? x)
  (eq? nullcomp x))

;; a counter for entity ids
(define entity-id-counter -1)

;; The entity type.
;; Should basically be opaque to users of the library.
(define-type entity id components (system-links unprintable:))

;; get the entity component c, no checks
;; returns nullcomp if the component isn't 
;; used by this entity.
(define (entity-component-raw e c)
  (vector-ref (entity-components e) c))

;; Set the entity component at c to v, regardless of its value
(define (entity-component-set-raw! e c v)
  (vector-set! (entity-components e) c v))

;; Set the entity component at c to nullcomp, regardless of its value.
;; NOTE: not part of the API, use can cause weird behavior.
(define (nullify-entity-component! e c)
  (entity-component-set-raw! e c nullcomp))

;; create an empty entity.
(define (entity-raw!)
  (make-entity 
   (begin (set! entity-id-counter (fx+ 1 entity-id-counter))
		  entity-id-counter)
   (make-vector (vector-length components) nullcomp)
   (make-vector (vector-length systems) #f)))


;; build a component value.
(define (construct-component entity component args)
  (let ((cc (get-component-constructor component)))
	(if (procedure? cc)
		(apply cc entity args)
		cc)))

;; add a component without worrying about system membership
(define (add-component-raw! entity component #!rest args)
  (vector-set! (entity-components entity) component 
			   (construct-component entity component args)))

;; remove a component without worrying about system membership
(define (remove-component-raw! entity component)
  (vector-set! (entity-components entity) component nullcomp))

;; List the component values of e specified by component-list
(define (component-values e component-list)
  (let ((comps (entity-components e))) 
	(map (lambda (i) (vector-ref comps i)) component-list)))

;; Add an entity to the system s, regardless of whether it belongs there.
(define (add-to-system-raw! e s)
  (let* ((si s)
		 (s (vector-ref systems si))
		 (enter (system-enter s)))
	(system-add-to-end si e)
	(if (not (not enter))
		(call-with-system-components enter e si)
		#f)))

;; Remove the entity e from system s, regardless of whether it belongs there.
(define (remove-from-system-raw! e s)
  (let* ((si s)
		 (s (vector-ref systems si))
		 (exit (system-exit s)))
	(if (not (not exit))
		(call-with-system-components exit e si)
		#f)
	(system-remove si e)))

