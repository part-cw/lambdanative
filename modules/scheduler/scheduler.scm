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

;; task scheduler: Manages and runs the plugins
(include "plugin.scm")
(include "instance.scm")

(define scheduler:debuglevel 10)
(define (scheduler:log level . x) (if (>= scheduler:debuglevel level) (apply log-system (append (list "scheduler: ") x))))


(define (scheduler:directory name)
  (let* ((sysdir (system-directory))
         (localdir (string-append sysdir (system-pathseparator) name)))
    ;; android might not have an existing system directory.
    (if (not (file-exists? sysdir)) (create-directory sysdir))
    (if (file-exists? localdir)
      localdir
      (begin
        (create-directory localdir)
        (if (not (file-exists? localdir)) (log-error "scheduler: failed to create path: " localdir))
        localdir
      )
    )
  ))

(define scheduler:configpath (scheduler:directory "config"))
(define scheduler:inputpath (scheduler:directory "input"))
(define scheduler:outputpath (scheduler:directory "results"))
(define scheduler:logpath (scheduler:directory "log"))
(define scheduler:defaultoutputpath scheduler:outputpath)


;; ---- MAIN PROCESSING

;; this function runs all inputs 
;; returns false if one input returns false
(define (scheduler:doinputs)
  (let loop ((sl (store-list))(ret #t))
    (if (= (length sl) 0)
      ret
      (loop (cdr sl) (and ret
        (let loop2 ((is (instance:allinputs (car sl)))(ret2 #t))
          (if (= (length is) 0)
            ret2
            (loop2 (cdr is) (and ret2 (instance:run (car sl) (car is))))
          )
        )
      ))
    )
  ))

(define scheduler:pendingstart '())
(define scheduler:pendingend '())

(define (scheduler-startcase store name)
  (store-event-clear! store)
  (scheduler:log 1 "startcase " name)
  (store-set! store "CaseStartPending" name)
  (set! scheduler:pendingstart (append scheduler:pendingstart (list (list store name)))))

(define (scheduler-endcase store)
  (scheduler:log 1 "endcase")
  (store-set! store "CaseEndPending" #t)
  (set! scheduler:pendingend (append scheduler:pendingend (list store))))

;; run case init on all stores with new cases
(define (scheduler:initnewcases now nowstr)
  (for-each (lambda (p)
    (let ((s (car p)) (c (cadr p)))
      (scheduler:log 1 "starting case " c)
      (store-set! s "CaseID" c)
      (store-set! s "Start" now)
      (store-set! s "StartStr" nowstr)
      (for-each (lambda (i) (instance:caseinit s i)) (instance:all s))
    )
  ) scheduler:pendingstart)
  (set! scheduler:pendingstart '())
)

;; run case end on all stores with finished cases
(define (scheduler:endoldcases)
  (for-each (lambda (s)
    (for-each (lambda (i) (instance:caseend s i)) (instance:all s))
    (scheduler:log 1 "ending case " (store-ref s "CaseID" #f))
    (store-set! s "CaseID" #f)
  ) scheduler:pendingend)
  (set! scheduler:pendingend '())
)

;; set store  timestamps (with offset for better performance)
(define (scheduler:initdispatchtimes)
  (let ((timedelta (/ 5.0 (if (fx> (length (store-list)) 0) (length (store-list)) 1.))))
    (let loop ((sl (store-list))(n 0))
      (if (> (length sl) 0) (begin
        (store-set! (car sl) "DispatchStart" (flo (+ ##now (* timedelta n))))
        (store-set! (car sl) "DispatchCount" 0.)
        (loop (cdr sl) (fx+ n 1))
      ))
    )
  ))

(define scheduler:initflag #f)
(define (scheduler-initialized?) scheduler:initflag)

(define (scheduler-init . timefunc)
  (scheduler:log 2 "initializing all instances")
  (for-each (lambda (s) (for-each (lambda (i) (instance:init s i)) (instance:all s))) (store-list))

  (if (fx= (length timefunc) 1) (begin
    (set! current-time-seconds (car timefunc))
    ;; If this is not done and we are replaying file scheduler:initdispatchtimes prevents stores from ever dispatching.
    (set! ##now (current-time-seconds))
  ))
  (scheduler:log 2 "setting store dispatch timing")
  (scheduler:initdispatchtimes)
  (set! scheduler:initflag #t)
)

(define (scheduler-iterate . guiwaveproc)
  (for-each (lambda (s) (store-set! s "Now" ##now)) (store-list))
  (scheduler:endoldcases)
  (scheduler:initnewcases ##now (seconds->string (fix ##now) "%T"))
  (if (scheduler:doinputs)
    (begin
      (for-each (lambda (s)
        (if (fl>= (fl- ##now (store-ref s "DispatchStart" 0.)) (store-ref s "DispatchCount" 0.))  ;; run every 1 second
          (begin
            (store-set! s "DispatchCount" (fl+ (store-ref s "DispatchCount" 0.) 1.))
            (store-waveform-dispatch s)
            (store-raw-dispatch s)
            ;; provide waveform access to gui here
            (if (= (length guiwaveproc) 1) ((car guiwaveproc)))

            ;; run all algorithms, even if case not active
            (for-each (lambda (i) (instance:run s i)) (instance:allalgs s))
            (if (store-ref s "CaseID" #f) (begin
              ;; run all dse instances
              (for-each (lambda (i) (instance:run s i)) (instance:alldses s))
              ;; run all output instances
              (for-each (lambda (i) (instance:run s i)) (instance:alloutputs s))
            ))
          )
        ))
      (store-list))
      ;; (watchdog)
      ;; (##gc)
      #t
    )
    #f
  )
)

(define (scheduler-cleanup)
  (scheduler:log 2 "cleanup on all instances")
  (for-each (lambda (s) (for-each (lambda (i) (instance:end s i)) (instance:all s))) (store-list))
)

;; All rolling logfiles at start of cases.. useful for a separate log of each case
(define log:lastrolled 0.)
(define log:lastcaseid #f)
(define (log-rollcase store)
  (let ((caseid (store-ref store "CaseID" #f)))
    (if (fl> (- ##now log:lastrolled) 1.0) (begin
      (if (not (equal? caseid log:lastcaseid)) (begin
        (log-system "Log continues at log_" caseid ".txt")
        (set! log:file (string-append log:path (system-pathseparator) "log_"
          (if caseid caseid (time->string (current-time) "%Y%m%d_%H%M%S")) ".txt"))
        (log-system "Application " (system-appname) " built " (system-builddatetime))
        (log-system "Git hash " (system-buildhash))
        (log-system "Log file rolled at " (if caseid "start" "end") " of case: " caseid)
      ))
      (set! log:lastrolled ##now)
      (set! log:lastcaseid caseid)
    ))
  ))

;; eof
