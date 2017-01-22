#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2017, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#

;; u8vector-like operations on single data structure
;; this is used to speed up parsing and catch errors
(define u8data:vector #f)
(define u8data:length 0)

(define (u8data)
  (set! u8data:vector #f)
  (set! u8data:length 0)
  (cons 0 u8data:length))

(define (u8data:sane? data)
  (if (pair? data)
    (if (and (integer? (car data)) (integer? (cdr data)))
      (and (fx<= (car data) (cdr data)) (fx<= (cdr data) u8data:length))
      #f
    )
    #f
  ))

(define (u8vector->u8data v . l)
  (if (u8vector? v)
    (let* ((len0 (u8vector-length v))
           (len (if (fx= (length l) 1) (car l) len0)))
      (set! u8data:vector v)
      (set! u8data:length
        (if (fx> len len0)
          (begin
            (log-error "u8vector->u8data: invalid length")
            len0
          )
          len
        )
      )
      (cons 0 u8data:length)
    )
    #f
  ))

(define (subu8data data start end)
  (if (u8data:sane? data)
    (if (fx<= start end)
      (let* ((ofs (car data))
             (nstart (fx+ ofs start))
             (nend (fx+ ofs end)))
        (if (and (fx<= nend u8data:length) (fx<= nend (cdr data)))
          (cons nstart nend)
          (begin
            (log-warning "subu8data: invalid range: [" start " -> " end
              "] really ends " (cdr data) " (" u8data:length ")")
            (cons (min nstart u8data:length) (min nend   u8data:length))
          )
        )
      )
      (begin
        (log-error "subu8data: invalid range: [" start " -> " end "]")
        data
      )
    )
    (begin
      (log-error "subu8data: invalid data")
      data
    )
  ))

(define (u8data-skip data count)
  (subu8data data count (u8data-length data)))

(define (u8data-ref data idx)
  (if (u8data:sane? data)
    (let ((ofs (fx+ idx (car data))))
      (if (and (fx< ofs u8data:length) (fx< ofs (cdr data)))
        (u8vector-ref u8data:vector ofs)
        (begin
          (log-error "u8data-ref: invalid range: " ofs " in " (cdr data) " (" u8data:length ")")
          0
        )
      )
    )
    (begin
      (log-error "u8data-ref: invalid data")
      0
    )
  ))

(define (u8data-set! data idx val)
  (if (u8data:sane? data)
    (let ((ofs (fx+ idx (car data))))
      (if (and (fx< ofs u8data:length) (fx< ofs (cdr data)))
        (u8vector-set! u8data:vector ofs val)
        (log-error "u8data-set!: invalid range")
      )
    )
    (log-error "u8data-set!: invalid data")
  ))

(define (u8data-length data)
  (if (u8data:sane? data)
    (fx- (cdr data) (car data))
    (begin
      (log-error "u8data-length: invalid data")
      0
    )
  ))

(define (u8data:u8->s8 v)
  (if (fx<= v 127) v (fx- v 256)))
(define (u8data:u16->s16 v)
  (if (fx<= v 32767) v (fx- v 65536)))
(define (u8data:u24->s24 v)
  (if (fx<= v 8388607) v (fx- v 16777216)))
(define (u8data:u32->s32 v)
  (if (<= v 2147483647) v (- v 4294967296)))

(define (u8data-u8 buf)
  (u8data-ref buf 0))
(define (u8data-s8 buf)
  (u8data:u8->s8 (u8data-u8 buf)))
(define (u8data-u16 v)
  (bitwise-ior (arithmetic-shift (u8data-ref v 0) 8) (u8data-ref v 1)))
(define (u8data-s16 buf)
  (u8data:u16->s16 (u8data-u16 buf)))
(define (u8data-u32 v)
  (bitwise-ior (arithmetic-shift (u8data-ref v 0) 24)
               (arithmetic-shift (u8data-ref v 1) 16)
               (arithmetic-shift (u8data-ref v 2) 8)
   (u8data-ref v 3)))
(define (u8data-s32 buf)
  (u8data:u32->s32 (u8data-u32 buf)))

(define (u8data-le-u16 v)
  (bitwise-ior (arithmetic-shift (u8data-ref v 1) 8) (u8data-ref v 0)))
(define (u8data-le-s16 buf)
  (u8data:u16->s16 (u8data-le-u16 buf)))
(define (u8data-le-u32 v)
  (bitwise-ior (arithmetic-shift (u8data-ref v 3) 24)
               (arithmetic-shift (u8data-ref v 2) 16)
               (arithmetic-shift (u8data-ref v 1) 8)
    (u8data-ref v 0)))
(define (u8data-le-s32 buf)
  (u8data:u32->s32 (u8data-le-u32 buf)))

(define (u8data->u8vector data)
  (if (u8data:sane? data)
    (let ((ofs (car data))
          (len (fx- (cdr data) (car data))))
      (if (fx< ofs (- u8data:length len -1))
        (subu8vector u8data:vector ofs (+ ofs len))
        (begin
          (log-warning "u8data->u8vector: invalid range: " ofs " in " (cdr data) " (" u8data:length ")")
          #f
        )
      )
    )
    (begin
      (log-error "u8data->u8vector: invalid data")
      #f
    )
  ))

(define u8data:u8vector->f32
  (c-lambda (scheme-object) float
      "___result=*(float*)___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED));"))

(define u8data:u8vector->f64
  (c-lambda (scheme-object) double
      "___result=*(double*)___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED));"))

(define (u8data-f32 u)
  (if (pair? u)
    (let ((v (u8data->u8vector u)))
      (u8data:u8vector->f32 v))
    #f
  ))

(define (u8data-f64 u)
  (if (pair? u)
    (let ((v (u8data->u8vector u)))
      (u8data:u8vector->f64 v))
  ))

;; eof
