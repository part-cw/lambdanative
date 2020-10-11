#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2013, University of British Columbia
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
;; type conversions used throughout to prevent FFI type errors

(define fix:fixnum-max-as-flonum (##fixnum->flonum ##max-fixnum))

(define (fix n)
  (declare (not safe))
  (cond
    ((##fixnum? n) n)
    ((##bignum? n) n)
    ((##flonum? n) (if (##fl< n fix:fixnum-max-as-flonum)
        (##flonum->fixnum n) (##flonum->exact-int n)))
    ((##ratnum? n) (##floor n))
    (else #f) ;; no complex numbers
  ))

(define (flo n)
  (declare (not safe))
  (cond
    ((##flonum? n) n)
    ((##fixnum? n) (##fixnum->flonum n))
    (else (##exact->inexact n))
  ))

;; Note that macro-fix and macro-flo are only visible within ln_core.scm,
;; but are much faster. fix and flo are available everywhere but costlier.
(define-macro (macro-fix n)
  `(cond
    ((##fixnum? ,n) ,n)
    ((##bignum? ,n) ,n)
    ((##flonum? ,n)
     (if (##fl< ,n fix:fixnum-max-as-flonum)
         (##flonum->fixnum ,n) (##flonum->exact-int ,n)))
    ((##ratnum? ,n) (##floor ,n))
    (else #f) ;; no complex numbers
    ))

(define-macro (macro-flo n)
  `(cond
    ((##flonum? ,n) ,n)
    ((##fixnum? ,n) (##fixnum->flonum ,n))
    (else (##exact->inexact ,n))))

;; eof
