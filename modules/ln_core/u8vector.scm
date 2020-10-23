#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2020, University of British Columbia
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
;; misc u8vector manipulations

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void fill_vector(void *dst, char *src, int src_len) 
{
  memcpy(dst,src,src_len); 
}

end-of-c-declare
)

(define (string->u8vector str)
  (let* ((strlen (string-length str))
         (vec (make-u8vector strlen 0)))
  ((c-lambda (scheme-object char-string int) void 
      "fill_vector(___CAST(void *,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,___arg3);") vec str strlen)
  vec))

(define (u8vector->string vec)
  (list->string (map integer->char (u8vector->list (subu8vector vec 0 
    (let loop ((i 0))
      (if (or (fx= i (u8vector-length vec)) (fx= (u8vector-ref vec i) 0)) i 
      (loop (+ i 1)))
    ))))))

(define (u8vector->hexstring u8v)
  (if (u8vector? u8v)
    (apply string-append (map (lambda (x)
      (let* ((s (number->string x 16))
             (spad (if (= (string-length s) 1) "0" "")))
        (string-append spad s))) (u8vector->list u8v))) "??"))

(define (hexstring->u8vector str)
  (list->u8vector (map
     (lambda (s) (string->number s 16))
       (let loop ((s str)(res '()))
         (if (= (string-length s) 0) res
           (loop (substring s 2 (string-length s))
             (append res (list (substring s 0 2)))))))))

;;u8vectors that need unicode conversion
(define (u8vector->unicode-vector invec)
  (let* ((inveclen (u8vector-length invec))
         (outvec (make-vector inveclen))
         (outveclen 0)
         (armed? #f))
    (let loop ((i 0))
      (if (fx= i inveclen)
        (subvector outvec 0 outveclen)
        (begin
          (if armed?
            (case (u8vector-ref invec i)
              ((117)
                (let loop ((k 0) (val 0))
                  (if (fx= k 4)
                    (begin
                      (vector-set! outvec outveclen val)
                      (set! outveclen (fx+ outveclen 1))
                      (set! i (fx+ i 4))
                    )
                    (loop (fx+ k 1) (fx+ val (* (expt 16 (fx- 3 k))
                      (let ((v (u8vector-ref invec (fx+ k i 1))))
                        (if (fx>= v 97) (fx- v 87) (fx- v 48))))))
                  )))
               ((92)
                 (vector-set! outvec outveclen (u8vector-ref invec i))
                 (set! outveclen (fx+ outveclen 1))
                 (vector-set! outvec outveclen (u8vector-ref invec i))
                 (set! outveclen (fx+ outveclen 1))
               )
               (else
                 (vector-set! outvec outveclen 92)
                 (set! outveclen (fx+ outveclen 1))
                 (vector-set! outvec outveclen (u8vector-ref invec i))
                 (set! outveclen (fx+ outveclen 1))
               )
            )
            (if (not (fx= (u8vector-ref invec i) 92)) (begin
              (vector-set! outvec outveclen (u8vector-ref invec i))
              (set! outveclen (fx+ outveclen 1))
            ))
          )
          (set! armed? (fx= (u8vector-ref invec i) 92))
          (loop (fx+ i 1))
        )
      )
    )))

;;write a u8vector to a file
(define (u8vector->file u8v filename)
  (let ((file (open-output-file filename)))
    (write-subu8vector u8v 0 (u8vector-length u8v) file)
    (close-port file)))

(define (file->u8vector file)
  (if (file-exists? file) 
    (let* ((len (file-size file))
           (u8v (make-u8vector len))
           (fh (open-input-file file)))
      (read-subu8vector u8v 0 len fh)
      (close-port fh) 
   u8v) #f))

;; number conversions (big endian)
(define (u8vector->uXX v bytecount . o)
  (let ((ofs (if (fx= (length o) 1) (car o) 0)))
    (apply bitwise-ior (let loop ((n 0)(res '()))
      (if (fx= n bytecount) res 
        (loop (fx+ n 1) (append res (list
          (arithmetic-shift (u8vector-ref v (fx+ n ofs))
            (fx* (fx- bytecount n 1) 8))))))))))
(define (u8vector->u16 v . ofs) (apply u8vector->uXX (append (list v 2) ofs)))
(define (u8vector->u24 v . ofs) (apply u8vector->uXX (append (list v 3) ofs)))
(define (u8vector->u32 v . ofs) (apply u8vector->uXX (append (list v 4) ofs)))
(define (u8vector->u48 v . ofs) (apply u8vector->uXX (append (list v 6) ofs)))
(define (u8vector->u64 v . ofs) (apply u8vector->uXX (append (list v 8) ofs)))
(define (u8vector->u96 v . ofs) (apply u8vector->uXX (append (list v 12) ofs)))

(define (uXX->u8vector v bytecount)
  (let loop ((n 0)(res '#u8())(tmp v))
    (if (fx= n bytecount) res
      (loop (fx+ n 1) (u8vector-append
        (u8vector (bitwise-and tmp #xff)) res)
          (arithmetic-shift tmp -8)))))
(define (u16->u8vector v) (uXX->u8vector v 2))  
(define (u24->u8vector v) (uXX->u8vector v 3))  
(define (u32->u8vector v) (uXX->u8vector v 4))  
(define (u48->u8vector v) (uXX->u8vector v 6))  
(define (u64->u8vector v) (uXX->u8vector v 8))  
(define (u96->u8vector v) (uXX->u8vector v 12))  

;; change signedness
(define (uXX->sXX v bytecount)
  (let ((tmp (arithmetic-shift bytecount 3)))
    (if (< v (arithmetic-shift 1 (- tmp 1))) v (- v (arithmetic-shift 1 tmp)))))

(define (sXX->uXX v bytecount)
  (let ((tmp (arithmetic-shift bytecount 3)))
    (if (< v 0) (+ (arithmetic-shift 1 tmp) v) v)))

(define (u8vector->sXX v bytecount . o)
  (uXX->sXX (apply u8vector->uXX (append (list v bytecount) o)) bytecount))

(define (sXX->u8vector v bytecount)
  (uXX->u8vector (sXX->uXX v bytecount) bytecount))

(define (u8vector->s16 v . ofs) (apply u8vector->sXX (append (list v 2) ofs)))
(define (u8vector->s24 v . ofs) (apply u8vector->sXX (append (list v 3) ofs)))
(define (u8vector->s32 v . ofs) (apply u8vector->sXX (append (list v 4) ofs)))
(define (u8vector->s48 v . ofs) (apply u8vector->sXX (append (list v 6) ofs)))
(define (u8vector->s64 v . ofs) (apply u8vector->sXX (append (list v 8) ofs)))
(define (u8vector->s96 v . ofs) (apply u8vector->sXX (append (list v 12) ofs)))

(define (s16->u8vector v) (sXX->u8vector v 2))
(define (s24->u8vector v) (sXX->u8vector v 3))
(define (s32->u8vector v) (sXX->u8vector v 4))
(define (s48->u8vector v) (sXX->u8vector v 6))
(define (s64->u8vector v) (sXX->u8vector v 8))
(define (s96->u8vector v) (sXX->u8vector v 12))

(define (u8vector->integer u8v)
  (let loop ((i (- (u8vector-length u8v) 1))(e 1)(res 0))
    (if (< i 0) res
      (loop (- i 1) (* e 256) (+ res (* e (u8vector-ref u8v i)))))))

(define (integer->u8vector val)
  (let loop ((v val)(res (u8vector)))
    (if (= v 0) (if (= (u8vector-length res) 0) (u8vector 0) res)
      (loop (arithmetic-shift v -8)
        (u8vector-append (u8vector (bitwise-and v 255)) res)))))

;; eof
