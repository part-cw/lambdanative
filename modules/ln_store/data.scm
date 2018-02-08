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

;; hooks
(define (store-set-extern-handler! store proc)
  (store:setlocal! store "extern:set!" proc))
(define (store-clear-extern-handler! store proc)
  (store:setlocal! store "extern:clear!" proc))

;; thread-safe access to data stores
(define store:mutex (make-mutex))
(define (store:grab!) (mutex-lock! store:mutex))
(define (store:release!) (mutex-unlock! store:mutex))

;; time sensitive store disappear after this time:
(define store:timeout 30.)

(define (store-clear! store ids0)
  (let ((ids (if (list? ids0) ids0 (list ids0))))
    (if (store-ref store "use_fifo_export" #f)
      (for-each (lambda (id)
        (let ((fifos (store-ref store (string-append id ":fifoexportlist") '())))
          (for-each (lambda (fifo) (fifo:write fifo (list id))) fifos)
        )) ids)
    )
    (if (store-ref store "extern:clear!" #f)
      (for-each (lambda (id) (if (store-ref store id)((store-ref store "extern:clear!" #f) store id))) ids))
    (store:clearlocal! store ids)
  ))

(define (store:clearlocal! store ids)
  (let ((is (if (list? ids) ids (list ids))))
    (for-each (lambda (id)
      (let ((t (store:datatable store))
            (ct (store:categorytable store)))
        (if (and (table? t) (table-ref t id #f)) (begin
          (store:grab!)
          (table-set! t id)
          ;; clear a category entry if present
          (if ct
            (for-each (lambda (cat)
               (if (member id (cdr cat))
                  (let ((newlist (let loop2 ((cs (cdr cat))(res '()))
                    (if (fx= (length cs) 0) res
                       (loop2 (cdr cs) (append res
                         (if (equal? id (car cs)) '() (list (car cs)))))))))
                    (table-set! ct (car cat) newlist)
                  ))) (table->list ct))
          )
         (store:release!)
       #t) #f))) is)))

(define (store-clearexpired! store timeout ids . thunk)
  (let ((cb (if (fx= (length thunk) 1) (car thunk) #f))
        (is (if (list? ids) ids (list ids))))
    (for-each (lambda (id)
       (let ((tstamp (store-timestamp store id)))
         (if (fl> (fl- ##now tstamp) (flo timeout)) (begin
           (if (and cb (fl> tstamp 0.)) (cb store id))
           (store-clear! store id)))
       )) is)))

(define (store:setlocal! store id val . category)
  (if (not val)
    (store-clear! store id)
    (let* ((t (store:datatable store))
           (c (if (fx= (length category) 1) (car category) #f))
           (ct (store:categorytable store))
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
    (cond
      ((store-ref store "use_fifo_export" #f) store:setfifo!)
      (else store:setlocal!))
    (append (list store id val) category))
  (if (store-ref store "extern:set!" #f)
    (apply (store-ref store "extern:set!" #f) (append (list store id val) category)))
)

(define (store-setnew! store id val . c)
  (let ((oldval (store-ref store id #f))
        (cat (if (fx= (length c) 1) (car c) #f)))
    (if (not (equal? oldval val))
      (store-set! store id val cat) #f)))

;; note that this doesn't clear the store, just the category field
(define (store-clearcat! store category)
  (let ((ct (store:categorytable store)))
     (table-set! ct category '())))

(define (store-listcat store category)
  (let ((t (store:datatable store))
        (ct (store:categorytable store)))
    (map (lambda (v) (list v (car (table-ref t v '(#f))))) (table-ref ct category '()))
  ))

(define (store-getcat store)
  (let ((ct (store:categorytable store)))
    (map car (table->list ct))
  )
)

;;  store-ref uses the fallback ONLY when a parameter is not defined (not if the value is set to #f)
(define (store-ref store id . fback)
  (let ((fallback (if (= (length fback) 1) (car fback) #f))
        (t (store:datatable store)))
    (if (table? t)
      (begin
        (store:grab!)
        (let ((res (table-ref t id #f)))
          (store:release!)
          (if (pair? res) (car res) fallback)
        ))
      #f)))

(define (store-ref-clear store id . fback)
  (let ((fallback (if (= (length fback) 1) (car fback) #f))
        (t (store:datatable store)))
    (if (table? t)
      (begin
        (store:grab!)
        (let ((res (table-ref t id #f)))
          (table-set! t id)
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
        (t (store:datatable store)))
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
  (let ((t (store:datatable store)))
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
