;; implementation of sets based on binary search trees

(##namespace ("sets#"))
(##include "~~lib/gambit#.scm")
(##include "sets#.scm")

;; -----------------------
;; wrapper to handle comparison operator

(define element-less? <)

(define (set? s)
  (and (vector? s) (= (vector-length s) 2)
       (procedure? (vector-ref s 0))
       (vector? (vector-ref s 1))))

(define (set-empty? s)
  (set! element-less? (vector-ref s 0))
  (empty? (vector-ref s 1)))

(define (set-empty element-less)
  (vector element-less (empty-set)))

(define (set-adjoin e s)
  (set! element-less? (vector-ref s 0))
  (vector element-less? (adjoin e (vector-ref s 1))))

(define (set-remove e s)
  (set! element-less? (vector-ref s 0))
  (vector element-less? (remove e (vector-ref s 1))))

(define (set-singleton e element-less)
  (set! element-less? element-less)
  (vector element-less (singleton e)))

(define (set-element? e s)
  (set! element-less? (vector-ref s 0))
  (element? e (vector-ref s 1)))

(define (set-union s1 s2)
  (set! element-less? (vector-ref s1 0))
  (vector element-less? (union (vector-ref s1 1) (vector-ref s2 1))))

(define (set-intersection s1 s2)
  (set! element-less? (vector-ref s1 0))
  (vector element-less? (intersection (vector-ref s1 1) (vector-ref s2 1))))
 
(define (set-sum lst)
  (set! element-less? (vector-ref (car lst) 0))
  (vector element-less? (sum-sets (map (lambda (v) (vector-ref v 1)) lst))))

(define (set-difference s1 s2)
  (set! element-less? (vector-ref s1 0))
  (vector element-less? (set:difference (vector-ref s1 1) (vector-ref s2 1))))

(define (set->list s)
  (set! element-less? (vector-ref s 0))
  (set:>list (vector-ref s 1)))

(define (list->set s element-less)
  (set! element-less? element-less)
  (vector element-less (list:>set s)))
   
;; -----------------------
;;; This is sets.scm version 1.00 of 92/09/10.

;;; Sets implemented using bounded balanced binary trees.
;;; John D. Ramsdell --- The MITRE Corporation.

;;; The implementation is based on Binary search trees of Bounded
;;; Balance, similar to Nievergelt & Reingold, SIAM J. Computing
;;; 2(1), March 1973.  

;;; Copyright 1992 by The MITRE Corporation.
;;; Permission to use, copy, modify, and distribute this software and
;;; its documentation for any purpose and without fee is hereby granted,
;;; provided that the above copyright notice appear in all copies.  The
;;; MITRE Corporation makes no representations about the suitability of
;;; this software for any purpose.  It is provided "as is" without express
;;; or implied warranty.

;;; This is a translation of Shephen Adams' SML programs used to
;;; implement sets of integers.

;;; Copyright 1992 Stephen Adams.
;;;
;;; This software may be used freely provided that:
;;;   1. This copyright notice is attached to any copy, derived work,
;;;      or work including all or part of this software.
;;;   2. Any derived work must contain a prominent notice stating that
;;;      it has been altered from the original.

;;; Name(s): Stephen Adams.
;;; Department, Institution: Electronics & Computer Science,
;;;   University of Southampton
;;; Address:  Electronics & Computer Science
;;;           University of Southampton
;;;           Southampton  SO9 5NH
;;;	      Great Britian
;;; E-mail:   sra@ecs.soton.ac.uk

;;; The application must define element-less?.

;;; Exports
;;; (empty? SET) => BOOLEAN
;;; (empty-set) => SET
;;; (adjoin ELEMENT SET) => SET
;;; (remove ELEMENT SET) => SET
;;; (singleton ELEMENT) => SET
;;; (element? ELEMENT SET) => BOOLEAN
;;; (union SET SET) => SET
;;; (intersection SET SET) => SET
;;; (set-difference SET SET) => SET
;;; (sum-sets LIST-of-SETS) => SET.
;;; (set->list SET) => LIST-of-ELEMENTS.
;;; (list->set LIST-of-ELEMENTS) => SET.

(define (data tree) (vector-ref tree 0))
(define (size tree) (vector-ref tree 1))
(define (left tree) (vector-ref tree 2))
(define (right tree) (vector-ref tree 3))

(define the-empty-set (vector 'empty 0))
(define (mk-tree data left right)
  (vector data (+ 1 (size left) (size right)) left right))

;;; Tree balancing.

;;; Rotations.

;;;   a                 b
;;;  / \               / \
;;; x   b      ==>    a   z
;;;    / \           / \
;;;   y   z         x   y
(define (single-left d l r)
  (mk-tree
   (data r)
   (mk-tree d l (left r))
   (right r)))

;;;   a                   b
;;;  / \                /   \
;;; w   c              /     \
;;;    / \    ==>     a       c
;;;   b   z          / \     / \
;;;  / \            w   x   y   z
;;; x   y
(define (double-left d l r)
  (let ((lr (left r)))
    (mk-tree
     (data lr)
     (mk-tree d l (left lr))
     (mk-tree (data r) (right lr) (right r)))))

;;; (b (a x y) z)) ==> (a x (b y z))
(define (single-right d l r)
  (mk-tree
   (data l)
   (left l)
   (mk-tree d (right l) r)))

;;; (c (a w (b x y)) z) ==> (b (a w x) (c y z))
(define (double-right d l r)
  (let ((rl (right l)))
    (mk-tree
     (data rl)
     (mk-tree (data l) (left l) (left rl))
     (mk-tree d (right rl) r))))

(define (balance d l r)
  (let ((sl (size l))
	(sr (size r)))
    (cond ((< (+ sl sr) 2) (mk-tree d l r))
	  ((>= sr (* 3 sl))		; Right too big.
	   (if (< (size (left r)) (size (right r)))
	       (single-left d l r)
	       (double-left d l r)))
	  ((>= sl (* 3 sr))		; Left too big.
	   (if (< (size (right l)) (size (left l)))
	       (single-right d l r)
	       (double-right d l r)))
	  (else (mk-tree d l r)))))

;;; Exported
(define (empty? s) (zero? (size s)))

;;; Exported
; (singleton e) == (mk-tree e the-empty-set the-empty-set)
(define (singleton e)			
  (vector e 1 the-empty-set the-empty-set))

;;; Exported
(define (element? e s)
  (cond ((empty? s) #f)
	((element-less? e (data s))
	 (element? e (left s)))
	((element-less? (data s) e)
	 (element? e (right s)))
	(else #t)))

(define (insert e s)
  (cond ((empty? s) (singleton e))
	((element-less? e (data s))
	 (balance
	  (data s)
	  (insert e (left s))
	  (right s)))
	((element-less? (data s) e)
	 (balance
	  (data s)
	  (left s)
	  (insert e (right s))))
	(else s)))

(define (concat3 l d r)
  (cond ((empty? l) (insert d r))
	((empty? r) (insert d l))
	((< (* 3 (size l)) (size r))	; Right too big.
	 (balance
	  (data r)
	  (concat3 l d (left r))
	  (right r)))
	((< (* 3 (size r)) (size l))	; Left too big.
	 (balance
	  (data l)
	  (left l)
	  (concat3 (right l) d r)))
	(else (mk-tree d l r))))

(define (split< e s)
  (cond ((empty? s) s)
	((element-less? e (data s))
	 (split< e (left s)))
	((element-less? (data s) e)
	 (concat3 (left s) (data s) (split< e (right s))))
	(else (left s))))

(define (split> e s)
  (cond ((empty? s) s)
	((element-less? (data s) e)
	 (split> e (right s)))
	((element-less? e (data s))
	 (concat3 (split> e (left s)) (data s) (right s)))
	(else (right s))))

;;; Exported.
;;; This implementation assumes card(s0) is small.
(define (union s0 s1)
  (cond ((empty? s0) s1)
	((empty? s1) s0)
	(else
	 (let ((d (data s0)))
	   (concat3 (union (left s0) (split< d s1))
		    d
		    (union (right s0) (split> d s1)))))))

(define (delete e s)
  (cond ((empty? s) s)
	((element-less? e (data s))
	 (balance
	  (data s)
	  (delete e (left s))
	  (right s)))
	((element-less? (data s) e)
	 (balance
	  (data s)
	  (left s)
	  (delete e (right s))))
	(else (merge (left s) (right s)))))

(define (merge l r)
  (cond ((empty? l) r)
	((empty? r) l)
	(else
	 (balance (smallest r) l (delete-smallest r)))))

(define (smallest s)
  (if (empty? (left s))
      (data s)
      (smallest (left s))))

(define (delete-smallest s)
  (if (empty? (left s))
      (right s)
      (balance
       (data s)
       (delete-smallest (left s))
       (right s))))

;;; Exported.
;;; This implementation assumes card(s1) is small.
(define (set:difference s0 s1)
  (cond ((empty? s0) s0)
	((empty? s1) s0)
	(else
	 (let ((d (data s1)))
	   (concat (set:difference (split< d s0) (left s1))
		   (set:difference (split> d s0) (right s1)))))))

(define (concat l r)
  (cond ((empty? l) r)
	((empty? r) l)
	((< (* 3 (size l)) (size r))	; Right too big.
	 (balance
	  (data r)
	  (concat l (left r))
	  (right r)))
	((< (* 3 (size r)) (size l))	; Left too big.
	 (balance
	  (data l)
	  (left l)
	  (concat (right l) r)))
	(else
	 (balance (smallest r) l (delete-smallest r)))))

;;; Exported
(define (intersection s0 s1)
  (cond ((empty? s0) s0)
	((empty? s1) s1)
	(else
	 (let ((d (data s1)))
	   (let ((l (intersection (split< d s0) (left s1)))
		 (r (intersection (split> d s0) (right s1))))
	     (if (element? d s0)
		 (concat3 l d r)
		 (concat l r)))))))

;;; Exported.
(define (set:>list s)
  (let loop ((s s) (l '()))
    (if (empty? s)
	l
	(loop (left s)
	      (loop (right s) (cons (data s) l))))))

;;; Set functions defined in terms of the above ones.

;;; Exported.
(define (empty-set) the-empty-set)

;;; Exported.
(define (adjoin e s)
  (if (element? e s)
      s
      (insert e s)))

;;; Exported.
(define (list:>set l)
  (let loop ((l l) (s (empty-set)))
    (if (null? l)
	s
	(loop (cdr l) (adjoin (car l) s)))))

;;; Exported.
(define (sum-sets sets)
  (let loop ((sets sets) (sum (empty-set)))
    (if (null? sets)
	sum
	(loop (cdr sets) (union (car sets) sum)))))

;;; Exported.
(define (remove e s)
  (if (element? e s)
      (delete e s)
      s))
