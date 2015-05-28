; "record.scm" record data types
; Written by David Carlton, carlton@husc.harvard.edu.
; Re-Written by Aubrey Jaffer, agj @ alum.mit.edu, 1996, 1997
;
; This code is in the public domain.

; Implements `record' data structures for Scheme.  Using only the
; opacity of procedures, makes record datatypes and
; record-type-descriptors disjoint from R4RS types and each other, and
; prevents forgery and corruption (modification without using
; RECORD-MODIFIER) of records.

;;2001-07-24  Aubrey Jaffer  <agj@alum.mit.edu>
;;  changed identifiers containing VECTOR to VECT or VCT.

;;(require 'common-list-functions)
;;(require 'rev4-optional-procedures)

(define vector? vector?)
(define vector-ref vector-ref)
(define vector-set! vector-set!)
(define vector-fill! vector-fill!)
(define vector->list vector->list)
(define display display)
(define write write)
;@
(define record-modifier #f)
(define record-accessor #f)
(define record-constructor #f)
(define record-predicate #f)
(define make-record-type #f)

(let (;; protect CL functions against redefinition.
      (has-duplicates? has-duplicates?)
      (notevery notevery)
      (position position)

      ;; Need to close these to keep magic-cookie hidden.
      (make-vect make-vector)
      (vect vector)

      ;; We have to wrap these to keep magic-cookie hidden.
      (vect? vector?)
      (vect-ref vector-ref)
      (vect->list vector->list)
      (disp display)
      (wri write)

      ;; Need to wrap these to protect record data from being corrupted.
      (vect-set! vector-set!)
      (vect-fill! vector-fill!)

      (nvt "of non-vector type")
      )
  (letrec
      (;; Tag to identify rtd's.  (A record is identified by the rtd
       ;; that begins it.)
       (magic-cookie (cons 'rtd '()))
       (rtd? (lambda (object)
	       (and (vect? object)
		    (not (= (vector-length object) 0))
		    (eq? (rtd-tag object) magic-cookie))))
       (rec? (lambda (obj)
	       (and (vect? obj)
		    (>= (vector-length obj) 1)
		    (or (eq? magic-cookie (rec-rtd obj))
			(rtd? (rec-rtd obj))))))

       (vec:error
	(lambda (proc-name msg obj)
	  (log-error proc-name msg
		      (cond ((rtd? obj) 'rtd)
			    ((rec? obj) (rtd-name (rec-rtd obj)))
			    (else obj)))))

       ;; Internal accessor functions.  No error checking.
       (rtd-tag (lambda (x) (vect-ref x 0)))
       (rtd-name (lambda (rtd) (if (vect? rtd) (vect-ref rtd 1) "rtd")))
       (rtd-fields (lambda (rtd) (vect-ref rtd 3)))
       ;; rtd-vfields is padded out to the length of the vector, which is 1
       ;; more than the number of fields
       (rtd-vfields (lambda (rtd) (cons #f (rtd-fields rtd))))
       ;; rtd-length is the length of the vector.
       (rtd-length (lambda (rtd) (vect-ref rtd 4)))

       (rec-rtd (lambda (x) (vect-ref x 0)))
       (rec-disp-str
	(lambda (x)
	  (let ((name (rtd-name (rec-rtd x))))
	    (string-append
	     "#<" (if (symbol? name) (symbol->string name) name) ">"))))

       (make-rec-type
	(lambda (type-name field-names)
	  (if (not (or (symbol? type-name) (string? type-name)))
	      (log-error 'make-record-type "non-string type-name argument."
			  type-name))
	  (if (or (and (list? field-names) (has-duplicates? field-names))
		  (notevery symbol? field-names))
	      (log-error 'make-record-type "illegal field-names argument."
			  field-names))
	  (let* ((augmented-length (+ 1 (length field-names)))
		 (rtd (vect magic-cookie
			    type-name
			    '()
			    field-names
			    augmented-length
			    #f
			    #f)))
	    (vect-set! rtd 5
		       (lambda (x)
			 (and (vect? x)
			      (= (vector-length x) augmented-length)
			      (eq? (rec-rtd x) rtd))))
	    (vect-set! rtd 6
		       (lambda (x)
			 (and (vect? x)
			      (>= (vector-length x) augmented-length)
			      (eq? (rec-rtd x) rtd)
			      #t)))
	    rtd)))

       (rec-predicate
	(lambda (rtd)
	  (if (not (rtd? rtd))
	      (log-error 'record-predicate "invalid argument." rtd))
	  (vect-ref rtd 5)))

       (rec-constructor
	(lambda (rtd . field-names)
	  (if (not (rtd? rtd))
	      (log-error 'record-constructor "illegal rtd argument." rtd))
	  (if (or (null? field-names)
		  (equal? field-names (rtd-fields rtd)))
	      (let ((rec-length (- (rtd-length rtd) 1)))
		(lambda elts
		  (if (= (length elts) rec-length) #t
		      (log-error 'record-constructor
				  (rtd-name rtd)
				  "wrong number of arguments."))
		  (apply vect rtd elts)))
	      (let ((rec-vfields (rtd-vfields rtd))
		    (corrected-rec-length (rtd-length rtd))
		    (field-names (car field-names)))
		(if (or (and (list? field-names) (has-duplicates? field-names))
			(notevery (lambda (x) (memq x rec-vfields))
					  field-names))
		    (log-error
		     'record-constructor "invalid field-names argument."
		     (cdr rec-vfields)))
		(let ((field-length (length field-names))
		      (offsets
		       (map (lambda (field) (position field rec-vfields))
			    field-names)))
		  (lambda elts
		    (if (= (length elts) field-length) #t
			(log-error 'record-constructor
				    (rtd-name rtd)
				    "wrong number of arguments."))
		    (let ((result (make-vect corrected-rec-length)))
		      (vect-set! result 0 rtd)
		      (for-each (lambda (offset elt)
				  (vect-set! result offset elt))
				offsets
				elts)
		      result)))))))

       (rec-accessor
	(lambda (rtd field-name)
	  (if (not (rtd? rtd))
	      (log-error 'record-accessor "invalid rtd argument." rtd))
	  (let ((index (position field-name (rtd-vfields rtd)))
		(augmented-length (rtd-length rtd)))
	    (if (not index)
		(log-error 'record-accessor "invalid field-name argument."
			    field-name))
	    (lambda (x)
	      (if (and (vect? x)
		       (>= (vector-length x) augmented-length)
		       (eq? rtd (rec-rtd x)))
		  #t
		  (log-error 'record-accessor "wrong record type." x "not" rtd))
	      (vect-ref x index)))))

       (rec-modifier
	(lambda (rtd field-name)
	  (if (not (rtd? rtd))
	      (log-error 'record-modifier "invalid rtd argument." rtd))
	  (let ((index (position field-name (rtd-vfields rtd)))
		(augmented-length (rtd-length rtd)))
	    (if (not index)
		(log-error 'record-modifier "invalid field-name argument."
			    field-name))
	    (lambda (x y)
	      (if (and (vect? x)
		       (>= (vector-length x) augmented-length)
		       (eq? rtd (rec-rtd x)))
		  #t
		  (log-error 'record-modifier "wrong record type." x "not" rtd))
	      (vect-set! x index y)))))
       )
    (set! vector? (lambda (obj) (and (vect? obj) (not (rec? obj)))))
    (set! vector-ref
	  (lambda (vct k)
	    (cond ((rec? vct)
		   (vec:error 'vector-ref nvt vct))
		  (else (vect-ref vct k)))))
    (set! vector->list
	  (lambda (vct)
	    (cond ((rec? vct)
		   (vec:error 'vector->list nvt vct))
		  (else (vect->list vct)))))
    (set! vector-set!
	  (lambda (vct k obj)
	    (cond ((rec? vct) (vec:error 'vector-set! nvt vct))
		  (else (vect-set! vct k obj)))))
    (set! vector-fill!
	  (lambda (vct fill)
	    (cond ((rec? vct)
		   (vec:error 'vector-fill! nvt vct))
		  (else (vect-fill! vct fill)))))
    (set! display
	  (lambda (obj . opt)
	    (apply disp (if (rec? obj) (rec-disp-str obj) obj) opt)))
    (set! write
	  (lambda (obj . opt)
	    (if (rec? obj)
		(apply disp (rec-disp-str obj) opt)
		(apply wri obj opt))))
    (set! record-modifier rec-modifier)
    (set! record-accessor rec-accessor)
    (set! record-constructor rec-constructor)
    (set! record-predicate rec-predicate)
    (set! make-record-type make-rec-type)
    ))
