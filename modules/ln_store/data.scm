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

;; thread-safe access to data stores
(define store:mutex (make-mutex))
(define (store:grab!) (mutex-lock! store:mutex))
(define (store:release!) (mutex-unlock! store:mutex))

;; time sensitive store disappear after this time:
(define store:timeout 30.)

(define (store-clear! store ids0)
  (let ((ids (if (list? ids0) ids0 (list ids0))))
    (store:clearlocal! store ids)
    (if (store-ref store "use_fifo_export" #f)
      (let loop2 ((is ids))
        (if (> (length is) 0)
          (begin
            (let loop3 ((fifos (store-ref store (string-append (car is) ":fifoexportlist") '())))
              (if (fx> (length fifos) 0)
                (begin
                  (fifo-write (car fifos) (list (car is)))
                  (loop3 (cdr fifos))
                )))
            (loop2 (cdr is))
          )
        )))
  ))

(define (store:clearlocal! store ids)
  (let loop0 ((is (if (list? ids) ids (list ids))))
    (if (fx> (length is) 0)
      (let ((id (car is)))
        (let ((t (store-datatable store))
              (ct (store-categorytable store)))
          (if (table? t) (begin
            (store:grab!)
            (table-set! t id)
            ;; clear a category entry if present
            (if ct 
              (let loop ((cats (table->list ct))) (begin
                (if (fx> (length cats) 0) 
                  (let ((cat (car cats)))
                    (if (member id (cdr cat))
                      (let ((newlist (let loop2 ((cs (cdr cat))(res '()))
                            (if (fx= (length cs) 0) res
                              (loop2 (cdr cs) (append res 
                                (if (equal? id (car cs)) '() (list (car cs)))))))))
                    (table-set! ct (car cat) newlist)
                  ))
                  (loop (cdr cats))))
            )))
            (store:release!)
          #t) #f)
        ) (loop0 (cdr is)))))
)

;M @deffn {procedure} store-clearexpired! store timeout idlist [thunk]
;M @strong{Description:}@*
;M Clear entries in the datastore if the timestamp differ from wall time by more than timeout.
;M @end deffn
(define (store-clearexpired! store timeout ids . thunk)
  (let ((cb (if (fx= (length thunk) 1) (car thunk) #f)))
    (let loop ((is (if (list? ids) ids (list ids))))
      (if (fx> (length is) 0)  
        (let* ((id (car is))
               (tstamp (store-timestamp store id)))
          (if (fl> (fl- ##now tstamp) (flo timeout)) (begin 
            (if (and cb (fl> tstamp 0.)) (cb store id))
            (store-clear! store id)))
          (loop (cdr is)))))))


(define (store:setlocal! store id val . category)
  (if (not val)
    (store-clear! store id)
    (let* ((t (store-datatable store))
           (c (if (fx= (length category) 1) (car category) #f))
           (ct (store-categorytable store))
           (cl (if ct (table-ref ct c '()) #f)))
      (if (table? t)
        (begin
          (store:grab!)
          (table-set! t id (cons val ##now))
          (if (and c (not (member id cl)))
            (table-set! ct c (append cl (list id)))
          )
          (store:release!)
          #t
        )
        #f)
    )))

(define (store-set! store id val . category)
  (apply
    (if (store-ref store "use_fifo_export" #f) store:setfifo! store:setlocal!)
    (append (list store id val) category)
  ))

(define (store-setnew! store id val . c)
  (let ((oldval (store-ref store id #f))
        (cat (if (fx= (length c) 1) (car c) #f)))
    (if (not (equal? oldval val))
      (store-set! store id val cat) #f)))



;; note that this doesn't clear the store, just the category field
(define (store-clearcat store category)
  (let ((ct (store-categorytable store)))
     (table-set! ct category '())))

(define (store-listcat store category)
  (let ((t (store-datatable store))
        (ct (store-categorytable store)))
    (let loop ((cl (table-ref ct category '()))(res '()))
      (if (fx= (length cl) 0)
        res
        (loop (cdr cl) (append res (list (list (car cl) (car (table-ref t (car cl) '(#f)))))))
      ))))


;;  store-ref uses the fallback ONLY when a parameter is not defined (not if the value is set to #f)
(define (store-ref store id . fback)
  (let ((fallback (if (= (length fback) 1) (car fback) #f))
        (t (store-datatable store)))
    (if (table? t)
      (begin
        (store:grab!)
        (let ((res (table-ref t id #f)))
          (store:release!)
          (if (pair? res) (car res) fallback)
        ))
      #f)))

;; lookup volatile store
(define (store-timedref store id . fback)
  (if (= (length fback) 1)
    (store-timedrefsec store id store:timeout (car fback))
    (store-timedrefsec store id store:timeout)
  )
)
;; lookup volatile store with user-specified timeout period
(define (store-timedrefsec store id tout . fback)
  (let ((fallback (if (= (length fback) 1) (car fback) #f))
        (t (store-datatable store)))
    (if (table? t)
      (begin
        (store:grab!)
        (let ((res (table-ref t id fallback)))
          (store:release!)
          (if (pair? res)
            (if (> (- ##now (cdr res)) tout) fallback (car res))
            res
          ))
      )
      #f)))

(define (store-timestamp store id)
  (let ((t (store-datatable store)))
    (if (table? t)
      (begin
        (store:grab!)
        (let ((res (table-ref t id #f)))
          (store:release!)
          (if (pair? res) (cdr res) 0.)
        )
      )
      (begin
        (log-error "store-timestamp: unknown store " store) 
        0.
      ))))

;; eof