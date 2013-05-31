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

;; Procedures for saving and loading settings to/from a file
;; (Supports only one unique settings table, and is not thread-safe!)

;; The table of settings
(define settings:table #f)

(define settings:file (string-append (system-directory) (system-pathseparator) "config" (system-pathseparator) "settings.scm"))

(define (settings-init defaults)
  ;; Create the settings table from the defaults with init as the value of any keys with no value
  (set! settings:table (list->table defaults init: #f))
  ;; Load settings from file, create settings file if none found
  (if (file-exists? settings:file)
    (begin
      (let ((settings_in (list->table (with-input-from-file (list path: settings:file) (lambda () (read))) init: "")))
        ;; Check for any missing settings, fill them in with defaults
        (let loop ((names (table->list settings:table)))
          (if (fx> (length names) 0)
            (let* ((name (caar names))
                   (invalue (table-ref settings_in name)))
              ;; If no value for one of the settings, (is equal to "")
              (if (and (string? invalue) (string=? invalue "")) 
                (table-set! settings_in name (cdar names))
              )
              (loop (cdr names))
            )))
        ;; Set the config file
        (set! settings:table settings_in)
      ))
    (settings:save)
  )
)

(define (settings:save)
  (with-output-to-file (list path: settings:file) (lambda () (write (table->list settings:table))))
)

(define (settings-set! key value)
  (table-set! settings:table key value)
  (settings:save)
)

(define (settings-ref key . fback)
  (let ((fallback (if (= (length fback) 1) (car fback) #f)))
    (table-ref settings:table key fallback)
  ))

;; eof