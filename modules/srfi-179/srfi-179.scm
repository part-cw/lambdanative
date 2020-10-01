(define-macro (define-values names . body)
  (let ((vals (gensym 'vals)))
    `(begin
       ,@(map (lambda (name) `(define ,name #f)) names)
       (call-with-values (lambda () . ,body)
         (lambda ,vals
           . ,(map (lambda (name)
                     `(set! ,name (let ((,name (car ,vals))) (set! ,vals (cdr ,vals)) ,name)))
                   names))))))

(define-macro (this-module name exports)
  (let ((extern (lambda (e) (if (pair? e) (car e) e)))
        (intern (lambda (e) (if (pair? e) (cadr e) e)))
        (module-name-end (string->symbol (string-append "module-" (symbol->string name) "-end"))))
    (let ((exported (map extern exports))
          (internal (map intern exports)))
      (let ((params (map (lambda (n) (string->symbol (string-append "p-" (number->string n)))) (iota (length internal))))
            (end-code `(,name . ,internal)))
        (eval `(define-macro (,module-name-end) ',end-code))
        `(begin
           (define-macro (,module-name-end) ',end-code)
           ,@(map (lambda (name) `(define ,name #f)) exported)
           (define ,name
             (lambda ,params . ,(map (lambda (e i) `(set! ,e ,i)) exported params))))))))


(cond-expand
 (gambit-c
  ;; from srfi-1
  (define (##iota count #!optional (start 0) (step 1))
    (if (and (##eqv? step 1)
             (##fixnum? start)
             (##fx+? (##fx- count 1) start))

        (let loop ((i count) (result '()))
          (if (##fx> i 0)
              (let ((i (##fx- i 1)))
                (loop i (##cons (##fx+ start i) result)))
              result))

        (let loop ((i count) (result '()))
          (if (##fx> i 0)
              (let ((i (##fx- i 1)))
                (loop i (##cons (##+ start (##* step i)) result)))
              result))))
  (define iota ##iota)
  (define (take x i)
    (let loop ((probe x)
               (j i)
               (rev-result '()))
      (if (##fx> j 0)
          (loop (if (pair? probe) (##cdr probe) (error "take: short list" x i))
                (##fx- j 1)
                (##cons (##car probe) rev-result))
          (reverse rev-result))))

  (define (drop x i)
    (let loop ((probe x)
               (j i))
      (if (##fx> j 0)
          (loop (if (pair? probe) (##cdr probe) (error "drop: short list" x i))
                (##fx- j 1))
          probe)))
  (define (exact-integer? obj) (and (number? obj) (exact? obj)))
  ;;
  (define vector-copy! ##vector-copy)
  (define s8vector-copy! ##s8vector-copy)
  (define s16vector-copy! ##s16vector-copy)
  (define s32vector-copy! ##s32vector-copy)
  (define s64vector-copy! ##s64vector-copy)
  (define u8vector-copy! ##u8vector-copy)
  (define u16vector-copy! ##u16vector-copy)
  (define u32vector-copy! ##u32vector-copy)
  (define u64vector-copy! ##u64vector-copy)
  (define f32vector-copy! ##f32vector-copy)
  (define f64vector-copy! ##f64vector-copy)
  (define (c64vector-copy! to at from start end)
    (f32vector-copy! to (* 2 at) from (* 2 start) (* 2 end)))
  (define (c128vector-copy! to at from start end)
    (f64vector-copy! to (* 2 at) from (* 2 start) (* 2 end)))
  ;; from srfi-43
  (define (vectors-ref vectors i)
    (map (lambda (v) (vector-ref v i)) vectors))
  (define %smallest-length
    (letrec
        ((loop
          (lambda (vector-list length callee)
            (if (null? vector-list)
                length
                (loop (cdr vector-list)
                      (min (vector-length
                            (let ((e (car vector-list)))
                              (if (vector? e) e (error "not a vector" callee e))))
                           length)
                      callee)))))
      loop))
  (define %vector-map2+!
    (letrec ((loop (lambda (f target vectors i)
                     (if (zero? i)
                         target
                         (let ((j (- i 1)))
                           (vector-set! target j
                                        (apply f j (vectors-ref vectors j)))
                           (loop f target vectors j))))))
      (lambda (f target vectors len)
        (loop f target vectors len))))
  (define (vector-map f vec . vectors)
    (let ((len (%smallest-length vectors
                                 (vector-length vec)
                                 vector-map)))
      (%vector-map2+! f (make-vector len) (cons vec vectors)
                      len)))
  )
 ((or gambit r7rs)
  (begin
    (define (c64vector-copy! to at from start end)
      (f32vector-copy! to (* 2 at) from (* 2 start) (* 2 end)))
    (define (c128vector-copy! to at from start end)
      (f64vector-copy! to (* 2 at) from (* 2 start) (* 2 end)))))
 (else
  ;; Punt
  (begin
    (define vector-copy! #f)
    (define s8vector-copy! #f)
    (define s16vector-copy! #f)
    (define s32vector-copy! #f)
    (define s64vector-copy! #f)
    (define u8vector-copy! #f)
    (define u16vector-copy! #f)
    (define u32vector-copy! #f)
    (define u64vector-copy! #f)
    (define c64vector-copy! #f)
    (define c128vector-copy! #f))))

(cond-expand
 ;;(debug (include "generic-arrays.scm"))
 (else
  (this-module
   generic-arrays
   (
    c64vector-copy!
    c128vector-copy!
    ;; Miscellaneous Functions
    translation? permutation?

    ;; Intervals
    make-interval interval? interval-dimension interval-lower-bound interval-upper-bound interval-lower-bounds->list interval-upper-bounds->list
    interval-lower-bounds->vector interval-upper-bounds->vector interval= interval-volume interval-subset? interval-contains-multi-index?
    interval-projections interval-for-each interval-dilate interval-intersect interval-translate interval-permute interval-rotate interval-scale
    interval-cartesian-product

    ;; Storage Classes
    make-storage-class storage-class? storage-class-getter storage-class-setter storage-class-checker storage-class-maker storage-class-copier
    storage-class-length storage-class-default generic-storage-class s8-storage-class s16-storage-class s32-storage-class s64-storage-class
    u1-storage-class u8-storage-class u16-storage-class u32-storage-class u64-storage-class f8-storage-class f16-storage-class f32-storage-class
    f64-storage-class c64-storage-class c128-storage-class

    ;; Arrays
    make-array array? array-domain array-getter array-dimension mutable-array? array-setter specialized-array-default-safe?
    specialized-array-default-mutable? make-specialized-array specialized-array? array-storage-class array-indexer array-body array-safe?
    array-elements-in-order? specialized-array-share array-copy array-curry array-extract array-tile array-translate array-permute
    array-rotate array-reverse array-sample array-outer-product array-map array-for-each array-fold array-fold-right array-reduce array-any array-every
    array->list list->array array-assign! array-swap! specialized-array-reshape array-ref array-set!
    ))

  (let ()
    (include "generic-arrays.scm")
    (module-generic-arrays-end))
)) ;; cond-expand

(define-cond-expand-feature srfi-179 generic-arrays)

