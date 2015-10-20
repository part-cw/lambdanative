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

;; trend output plugin (writes trends to csv file)

(define (trendoutput:stop store instance)
  (let ((fh (instance-refvar store instance "Handle"))
        (casefile (instance-refvar store instance "FilePath")))
    (if (port? fh) (begin
      (close-output-port fh)
      (if (and casefile (function-exists? "timestamp-gettimestamp"))
        ((eval 'timestamp-gettimestamp) casefile)
      )
    ))
    (instance-setvar! store instance "Handle" #f)
  ))

(define (trendoutput:start store instance)
  (let* ((casepath (instance-refvar store instance "CasePath" #f))
         (caseid (store-ref store "CaseID" #f))
         (namesuffix (instance-refvar store instance "NameSuffix" #f))
         (casefile (string-append casepath (system-pathseparator) "Trends_" (seconds->string ##now "%Y%m%d_%H%M%S")
                                  (if (string? namesuffix) namesuffix "") ".csv"))
         (fh (open-output-file casefile))
         (trends (instance-refvar store instance "Trends" '())))
   (instance-setvar! store instance "Handle" fh)
   (instance-setvar! store instance "FilePath" casefile)
   (instance-setvar! store instance "NextRun" 0.)
   (if fh (begin
     (display (number->string ##now) fh) (display ", " fh) (display caseid fh) (display "\n" fh)
     (display (system-appname) fh) (display "," fh) (display (system-builddatetime) fh) (display "," fh)
     (display (system-buildhash) fh) (display "," fh) (display (system-platform) fh) (display "\n" fh)
     (display "Time," fh)
     (for-each (lambda (t) (display t fh) (display "," fh)) trends)
     (display "Event\n" fh)
     (force-output fh)
   ))
 ))

(define (trendoutput:run store instance)
  (let* ((fh (instance-refvar store instance "Handle"))
         (trends (instance-refvar store instance "Trends" '()))
         (deltat (fl- ##now (store-ref store "Start" 0.)))
         (prv (instance-refvar store instance "Prv" 0.))
         (nextrun (instance-refvar store instance "NextRun" 0.))
         (interval (instance-refvar store instance "Interval" 1.)))
    (if (and fh (fl>= deltat nextrun)) (begin
      (display (number->string (fix (floor deltat))) fh)
      (for-each (lambda (t) (let ((val (store-ref store t "")))
          (display "," fh)
          (display (if (number? val) (float->string (flo val) 5) (if (boolean? val) (if val "1" "") val)) fh)
        ))
        trends)
      (let ((es (store-event-listnew store prv)))
        (for-each (lambda (e) (display "," fh) (display (cadr e) fh)) es)
      )
      (display "\n" fh)
      (force-output fh)
      (instance-setvar! store instance "Prv" ##now)
      (if (> nextrun 0)
        (let ((skipintervals (flfloor (fl/ (flo (- deltat nextrun)) (flo interval)))))
          (if (fl> skipintervals 0.) (log-system "trendoutput: !! skipped " skipintervals " intervals"))
          (instance-setvar! store instance "NextRun" (fl+ nextrun (fl* (flo interval) (fl+ 1. skipintervals))))
        ) (instance-setvar! store instance "NextRun" (flo interval)))
    ))
  ))

;; plugin hooks
(define (trendoutput:init store instance)
  (instance-setvar! store instance "Handle" #f)
  #t)

(define (trendoutput:caseinit store instance)
  (if (store-ref store "CaseID" #f) (begin
    (let* ((opath (instance-refvar store instance "OutputPath" scheduler:outputpath))
           (casepath (string-append opath (system-pathseparator) (store-ref store "CaseID" #f))))
      (if (not (file-exists? opath)) (create-directory opath))
      (if (not (file-exists? casepath)) (create-directory casepath))
      (instance-setvar! store instance "CasePath" casepath)
    )
    (trendoutput:start store instance)
  )))

(define (trendoutput:caserun store instance)
  (if (and (store-ref store "CaseID" #f)
           ;; forget disk space for now
           #t ;; (> (store-ref store "DiskSpace" 0) 0)
      )
    (trendoutput:run store instance)
  )
  #t)

(define (trendoutput:caseend store instance)
  (if (instance-refvar store instance "Handle" #f)
    (begin
      (store-event-add store 0 "EventNumber" (length (store-event-listnew store)))
      (trendoutput:run store instance)
    )
  )
  (trendoutput:stop store instance)
  #t)

(define (trendoutput:end store instance)
  (if (instance-refvar store instance "Handle" #f)
    (trendoutput:stop store instance))
  #t)

;; register the plugin
(plugin-register "trendoutput" trendoutput:init trendoutput:caseinit trendoutput:caserun
                 trendoutput:caseend trendoutput:end 'output)

;; eof
