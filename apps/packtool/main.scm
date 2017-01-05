#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
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

;; lambdanative tool to package files for scheme inclusion

(define srcfile "EMBED")
(define tgtfile "embed.scm")
(if (fx> (system-cmdargc) 1)
  (set! tgtfile (system-cmdarg))
)

(define (packtool:pack file overwrite)
  (if (file-exists? file)
    (let* ((data (file->u8vector file))
           (cdata (u8vector-compress data)))
       `(packtool-unpack ,file ',cdata ,overwrite))))

(if (file-exists? srcfile)
  (let* ((srcfiles (string-split (string-trim (with-input-from-file srcfile (lambda () (read-line)))) #\space))
         (tgttime (if (file-exists? tgtfile) (time->seconds (file-last-modification-time tgtfile)) 0.))
         (srcfile-updated? (> (time->seconds (file-last-modification-time srcfile)) tgttime))
         (dirty (let loop ((files srcfiles)(flag srcfile-updated?))
           (if (= (length files) 0) flag
             (let* ((file0 (string-split (car files) #\!))
                    (file (if (fx= (length file0) 2) (cadr file0) (car file0))))
               (loop (cdr files) (or flag (and (file-exists? file)
                 (> (time->seconds (file-last-modification-time file)) tgttime)))))))))
    (if dirty (begin
      (with-output-to-file tgtfile (lambda () (display ";; automatically generated. Do not edit.\n")))
      (let loop ((files srcfiles))
        (if (> (length files) 0)
          (let* ((tmp (string-split (car files) #\!))
                 (overwrite (fx= (length tmp) 2))
                 (file (if (fx= (length tmp) 2) (cadr tmp) (car tmp))))
            (if (file-exists? file) (begin
              (for-each display (list " => embedding " file " " (if overwrite "(overwrite)" "(write once)") "..\n"))
              (with-output-to-file (list path: tgtfile append: #t) (lambda ()
                (write (packtool:pack file overwrite))(newline)))))
            (loop (cdr files))))))
     (display " => embedded data is up to date, nothing to do.\n")))
)

;; eof
