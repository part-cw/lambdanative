#|
Copyright (C) 2020 Jörg F. Wittenberger. All Rights Reserved.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation
files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice (including the
next paragraph) shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
|#

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
 ;;(debug (include "generic-arrays-impl.scm"))
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
    (include "generic-arrays-impl.scm")
    (module-generic-arrays-end))
)) ;; cond-expand

(define-cond-expand-feature srfi-179 generic-arrays)
