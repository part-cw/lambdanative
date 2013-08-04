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

;; waveform output plugin

(define waveoutput:debuglevel 0)
(define (waveoutput:log level . x) (if (>= waveoutput:debuglevel level) (apply log-system (append (list "waveoutput: ") x))))

(define (waveoutput:stop store instance)
  (let ((fh (instance-refvar store instance "Handle")))
    (if (port? fh) (close-output-port fh))
    (instance-setvar! store instance "Handle" #f)
    (waveoutput:log 2 "stop")
  ))

(define (waveoutput:start store instance)
  (let* ((casepath (instance-refvar store instance "CasePath" #f))
         (casefile (string-append (instance-refvar store instance "Source" #f) ".csv"))
         (fh (open-output-file (string-append casepath (system-pathseparator) casefile))))
   (instance-setvar! store instance "Handle" fh)
   (waveoutput:log 2 "start fh=" fh)
   (if (not fh) (log-error "waveoutput:start failed to open file for writing"))
 ))

(define (waveoutput:run store instance)
  (let* ((fh (instance-refvar store instance "Handle"))
         (src (instance-refvar store instance "Source" #f))
         (data (if src (store-ref store src '()) '()))
        )
      (waveoutput:log 2 "run src=" src " [" (length data) "]")
#|
      ;; Sadly interpolating is too slow to be used on the Soekris box; hence we can't do this here.
      (if (and fh (fx> (length data) 0))
        (let ((deltat (fl/ 1. (flo (length data))))
              (ct (fl- (flo (length data)) 1.)))
          (for-each (lambda (s) (display (fl- ##now (fl* ct deltat)) fh) (set! ct (fl- ct 1.)) (display "," fh)
            (display s fh) (display "\n" fh)) data)
          (force-output fh)
        ))
|#
      (if (and fh (fx> (length data) 0)) (begin
        (for-each (lambda (s) (display s fh) (display "\n" fh)) (list-head data (fx- (length data) 1)))
        (for-each (lambda (s) (display s fh)) (list (car (list-tail data (fx- (length data) 1))) "," ##now "\n"))
        (force-output fh)
      ))
  ))

(define (waveoutput:init store instance)
  (instance-setvar! store instance "Handle" #f)
  #t)

(define (waveoutput:caseinit store instance)
  (if (store-ref store "CaseID" #f) (begin
    (let* ((opath (instance-refvar store instance "OutputPath" scheduler:outputpath))
           (casepath (string-append opath (system-pathseparator) (store-ref store "CaseID" #f))))
      (if (not (file-exists? opath)) (create-directory opath))
      (if (not (file-exists? casepath)) (create-directory casepath))
      (instance-setvar! store instance "CasePath" casepath)
    )
    (waveoutput:start store instance))
  ))

(define (waveoutput:run store instance)
  (if (and (store-ref store "CaseID" #f)
           ;; forget disk space for now
           #t ;; (> (store-ref store "DiskSpace" 0) 0)
      )
    (waveoutput:run store instance)
  )
  #t)

(define (waveoutput:caseend store instance)
  (if (instance-refvar store instance "Handle" #f) (waveoutput:run store instance))
  (waveoutput:stop store instance)
  #t)

(define (waveoutput:end store instance)
  (if (instance-refvar store instance "Handle" #f) (waveoutput:stop store instance))
  #t)

(plugin-register "waveoutput" waveoutput:init waveoutput:caseinit waveoutput:run 
                 waveoutput:caseend waveoutput:end 'output)

;; eof