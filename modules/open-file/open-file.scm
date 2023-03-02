#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2023, University of British Columbia
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

;; File opening
;; Currently only supported on Android
;; Note: for video files, can also use the videoplayer module

(define open-file:debuglevel 0)
(define (open-file:log level . x)
   (if (>= open-file:debuglevel level) 
      (apply log-system (append (list "open-file: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef ANDROID
  void android_open_file(char *, char *, int orientation);
#endif

static void open_file(char *filepath, char *filetype, int orientation)
{
#ifdef ANDROID
  android_open_file(filepath, filetype, orientation);
#endif
}

end-of-c-declare
)

(define (open-file filepath filetype . orientation)
  (open-file:log 1 filepath)
  (let ((orient (if (and (list? orientation) (fx= (length orientation) 1)) (car orientation) GUI_PORTRAIT)))
    ((c-lambda (char-string char-string int) void "open_file") filepath filetype orient))
  #t
 )

;;eof
