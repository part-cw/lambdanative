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

;; raw output plugin (outputs raw data to a binary file raw.bin)

(define (rawoutput:stop store instance)
  (let ((fh (instance-refvar store instance "Handle")))
    (if (port? fh) (close-output-port fh))
    (instance-setvar! store instance "Handle" #f)
  ))

(define (rawoutput:start store instance)
  (let* ((casepath (instance-refvar store instance "CasePath" #f))
         (namesuffix (instance-refvar store instance "NameSuffix" #f))
         (casefile (string-append casepath (system-pathseparator) "Raw_" 
                                  (time->string (current-time) "%Y%m%d_%H%M%S")
                                  (if (string? namesuffix) namesuffix "") ".bin"))
         (fh (open-output-file casefile)))
    (instance-setvar! store instance "Handle" fh)
    (if (not fh) (log-error "rawoutput:start failed to open file for writing"))
  ))

(define (rawoutput:run store instance)
  (let* ((fh (instance-refvar store instance "Handle"))
         (src (instance-refvar store instance "Source" #f))
         (data (if src (store-ref store src (u8vector)) (u8vector))))
    (if fh (begin
      (write-subu8vector data 0 (u8vector-length data) fh)
      (force-output fh)
    ))
    ;; Prevent stale values from being written
    (store-set! store src #f)
  ))

;; plugin hooks
(define (rawoutput:init store instance)
  (instance-setvar! store instance "Handle" #f)
  #t)

(define (rawoutput:caseinit store instance)
  (if (store-ref store "CaseID" #f) (begin
    (let* ((opath (instance-refvar store instance "OutputPath" scheduler:outputpath))
           (casepath (string-append opath (system-pathseparator) (store-ref store "CaseID" #f))))
      (if (not (file-exists? opath)) (create-directory opath))
      (if (not (file-exists? casepath)) (create-directory casepath))
      (instance-setvar! store instance "CasePath" casepath)
    )
    (rawoutput:start store instance)
  )))

(define (rawoutput:caserun store instance)
  (if (and (store-ref store "CaseID" #f) 
           #t ;;  (> (store-ref store "DiskSpace" 0) 0) ;; forget disk space for now
      )
    (rawoutput:run store instance)
  )
  #t)

(define (rawoutput:caseend store instance)
  (if (instance-refvar store instance "Handle" #f) (rawoutput:run store instance))
  (rawoutput:stop store instance)
  #t)

(define (rawoutput:end store instance)
  (if (instance-refvar store instance "Handle" #f) (rawoutput:stop store instance))
  #t)

;; register the plugin
(plugin-register "rawoutput" rawoutput:init rawoutput:caseinit rawoutput:caserun
                 rawoutput:caseend rawoutput:end 'output)

;; eof
