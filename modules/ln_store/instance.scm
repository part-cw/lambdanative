#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2016, University of British Columbia
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

;; Storage for instance-related variables

(define (store:instance-set! store id val)
  (if (not val)
    (store:instance-clear! store id)
    (let ((t (store:instancetable store)))
      (if (table? t)
        (begin
          (store:grab!)
          (table-set! t id (cons val ##now))
          (store:release!)
          #t
        )
        #f)
    )))

(define (store:instance-ref store id . fback)
  (let ((fallback (if (= (length fback) 1) (car fback) #f))
        (t (store:instancetable store)))
    (if (table? t)
      (begin
        (store:grab!)
        (let ((res (table-ref t id #f)))
          (store:release!)
          (if (pair? res) (car res) fallback)
        )
      )
      #f
    )))

(define (store:instance-clear! store id)
  (let ((t (store:instancetable store)))
    (if (and (table? t) (table-ref t id #f))
      (begin
        (store:grab!)
        (table-set! t id)
        (store:release!)
        #t
      )
      #f
    )
  ))

(define (store:instance-timestamp store id)
  (let ((t (store:instancetable store)))
    (if (table? t)
      (begin
        (store:grab!)
        (let ((res (table-ref t id #f)))
          (store:release!)
          (if (pair? res) (cdr res) 0.)
        )
      )
      (begin
        (log-error "store:instance-timestamp: unknown store " store)
        0.
      ))))

;; eof
