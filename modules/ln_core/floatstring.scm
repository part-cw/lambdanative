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
;; formatting of floating numbers

(define (float->string x p)
  (let* ((pe (flo (expt 10 (min 10 p))))
         (s (number->string (fl/ (flfloor (fl+ (fl* (flo x) pe) 0.5)) pe)))
         (sl (string-length s))
         (b (substring s 0 1))
         (e (substring s (- sl 1) sl)))
    (string-append (if (string=? b ".") "0" "")
       (if (string=? e ".") (substring s 0 (- sl 1)) s))))

;; padstring 100 2 >  100.00

(define (float->zeropaddedstring x p) 
  (let* ((s (float->string x p))
         (tmp (string-split s #\.))
         (decimal? (= (length tmp) 2))
         (decimals (if decimal? (cadr tmp) ""))
         (missing  (- p (string-length decimals)))
         (padding  (if (> missing 0) (make-string missing #\0) "")))
    (string-append s (if (not decimal?) "." "") padding)))


(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char valstr[16];

static char *double_to_choppedstring(double val, int precision)
{
  memset(valstr,0,16);
  // NOTE: snprintf in some versions of mingw is broken! fall back on _snprintf (the MS version)
#ifdef WIN32
  _snprintf(valstr,16,"%f",val);
#else
  snprintf(valstr,16,"%f",val);
#endif
  int p=precision;
  if (val<0) p++;
  valstr[p]=0;
  if (valstr[p-1]=='.') valstr[p-1]=0;
  return valstr;
}

end-of-c-declare
)

(define (float->choppedstring v p) ((c-lambda (double int) char-string "double_to_choppedstring") (flo v) p))

;; eof
