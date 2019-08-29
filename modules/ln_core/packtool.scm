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

;; simple code to package files within an application
;; this will allow default arbitrary files to be transferred to any platform

(define (packtool:prep file)
  (let* ((tmp (string-split file #\/))
         (basename (car (reverse tmp))))
    (let loop ((dirs (reverse (cdr (reverse tmp))))
               (path (system-directory)))
      (if (= (length dirs) 0) (string-append path (system-pathseparator) basename)
        (let ((newpath (string-append path (system-pathseparator) (car dirs))))
          (if (not (file-exists? newpath)) (create-directory newpath))
          ;; sometimes directories are not instantly available??
          (let loop2 ()
            (if (not (file-exists? newpath)) (begin
              (thread-sleep! 0.1)
              (loop2))))
          (loop (cdr dirs) newpath))))))

;; extract an embedded file
(define (packtool-unpack file cdata overwrite)
  (let ((rootpath (system-directory)))
    (if (not (file-exists? rootpath)) (create-directory rootpath)))
  (let ((path (packtool:prep file)))
    (if (or overwrite (not (file-exists? path)))
      (let ((data (u8vector-decompress cdata)))
        (log-system "packtool: extracting " path)
        (u8vector->file data path)))))

;; eof
