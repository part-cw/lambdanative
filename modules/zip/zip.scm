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

;; minimal bindings for the libzip archive library
;; only reading is implemented at the moment

(define zip:debuglevel 0)
(define (zip:log level . x)
   (if (>= zip:debuglevel level) (apply log-system (append (list "zip: ") x))))

(c-declare  #<<end-of-c-declare

#include <zip.h>

end-of-c-declare
)

(define ZIP_CHECKCONS ((c-lambda () int "___result = ZIP_CHECKCONS;")))
(define ZIP_CREATE ((c-lambda () int "___result = ZIP_CREATE;")))
(define ZIP_EXCL ((c-lambda () int "___result = ZIP_EXCL;")))
(define ZIP_TRUNCATE ((c-lambda () int "___result = ZIP_TRUNCATE;")))

(define ZIP_ER_EOF ((c-lambda () int "___result = ZIP_ER_EOF;")))
(define ZIP_ER_INTERNAL ((c-lambda () int "___result = ZIP_ER_INTERNAL;")))
(define ZIP_ER_INVAL ((c-lambda () int "___result = ZIP_ER_INVAL;")))
(define ZIP_ER_MEMORY ((c-lambda () int "___result = ZIP_ER_MEMORY;")))
(define ZIP_ER_NOZIP ((c-lambda () int "___result = ZIP_ER_NOZIP;")))
(define ZIP_ER_READ ((c-lambda () int "___result = ZIP_ER_READ;")))
(define ZIP_ER_RENAME ((c-lambda () int "___result = ZIP_ER_RENAME;")))
(define ZIP_ER_SEEK ((c-lambda () int "___result = ZIP_ER_SEEK;")))
(define ZIP_ER_TMPOPEN ((c-lambda () int "___result = ZIP_ER_TMPOPEN;")))
(define ZIP_ER_WRITE ((c-lambda () int "___result = ZIP_ER_WRITE;")))
(define ZIP_ER_ZLIB ((c-lambda () int "___result = ZIP_ER_ZLIB;")))

(define ZIP_FL_NOCASE ((c-lambda () int "___result = ZIP_FL_NOCASE;")))
(define ZIP_FL_NODIR ((c-lambda () int "___result = ZIP_FL_NODIR;")))
(define ZIP_FL_ENC_RAW ((c-lambda () int "___result = ZIP_FL_ENC_RAW;")))
(define ZIP_FL_ENC_GUESS ((c-lambda () int "___result = ZIP_FL_ENC_GUESS;")))
(define ZIP_FL_ENC_STRICT ((c-lambda () int "___result = ZIP_FL_ENC_STRICT;")))

(define ZIP_FL_COMPRESSED ((c-lambda () int "___result = ZIP_FL_COMPRESSED;")))
(define ZIP_FL_UNCHANGED ((c-lambda () int "___result = ZIP_FL_UNCHANGED;")))

;;(define ZIP_ ((c-lambda () int "___result = ZIP_;")))

(define zip-open (c-lambda (char-string int) (pointer void) "___result=zip_open(___arg1,___arg2,0);"))

(define zip-get-num-entries (c-lambda ((pointer void) int) int64 "zip_get_num_entries"))

(define zip-name-locate (c-lambda ((pointer void) char-string int) int64 "zip_name_locate")) 
(define zip-get-name (c-lambda ((pointer void) unsigned-int64 int) char-string "zip_get_name"))

(define zip-fopen (c-lambda ((pointer void) char-string int) (pointer void) "zip_fopen"))
(define zip-fopen-index (c-lambda ((pointer void) unsigned-int64 int) (pointer void) "zip_fopen_index"))

(define (zip-fread ptr u8v)
  (let ((u8len (u8vector-length u8v)))
    ((c-lambda ((pointer void) scheme-object unsigned-int64) int
       "___result=zip_fread(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)),___arg3);") ptr u8v u8len)))

(define zip-fclose (c-lambda ((pointer void)) int "zip_fclose"))

(define zip-close (c-lambda ((pointer void)) int "zip_close"))
(define zip-discard (c-lambda ((pointer void)) void "zip_discard"))

(define (zip-dir zipfile)
  (let* ((zh (zip-open zipfile 0))
         (n (zip-get-num-entries zh ZIP_FL_UNCHANGED))
         (lst (let loop ((i 0)(res '()))
           (if (= i n) res (loop (fx+ i 1) (append res (list (zip-get-name zh i 
              (bitwise-ior ZIP_FL_UNCHANGED ZIP_FL_ENC_RAW)))))))))
    (zip-close zh) lst))

(define (zipentry->u8vector zipfile filename)
  (let* ((zh (zip-open zipfile 0))
         (entry (zip-name-locate zh filename ZIP_FL_ENC_RAW))
         (buffer (make-u8vector 100000))
         (fh (zip-fopen-index zh entry 0)))
   (let loop ((res (u8vector)))
     (let ((n_read (zip-fread fh buffer)))
       (if (<= n_read 0) (begin (zip-fclose fh) (zip-close zh) res)
        (loop (u8vector-append res (subu8vector buffer 0 n_read))))))))

(define (zipentry->file zipfile zipfilename . dstfile)
  (let ((dst (if (= (length dstfile) 1) (car dstfile)
          (string-append (system-directory) (system-pathseparator) zipfilename))))
    (with-output-to-file dst (lambda ()
      (let* ((data (zipentry->u8vector zipfile zipfilename))
             (datalen (u8vector-length data)))
        (write-subu8vector data 0 datalen))))))

;; eof
