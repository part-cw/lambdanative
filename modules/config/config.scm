#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2020, University of British Columbia
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

;; init.scm
;; this is the glue between the native launcher and the portable code

(c-declare  #<<end-of-c-declare

#include "LNCONFIG.h"
#include "lambdanative.h"

#ifdef ANDROID
  extern char* android_getFilesDir();
  extern char* android_getPackageCodePath();
#else
  char* android_getFilesDir(){
    char* buf="";
    return buf;
  }
  char* android_getPackageCodePath(){
    char* buf="";
    return buf;
  }
#endif

void force_terminate()
{
  lambdanative_exit(0);
}

end-of-c-declare
)

(define ##now 0.)

(define (system-pathseparator)
  (string ((c-lambda () char "system_pathseparator"))))

(define (system-directory) ((c-lambda () char-string "system_dir")))
(define (system-appdirectory) ((c-lambda () char-string "system_appdir")))
(define (system-platform) ((c-lambda () char-string "system_platform")))
(define (system-appname) ((c-lambda () char-string "system_appname")))
(define (system-appversion) ((c-lambda () char-string "system_appversion")))
(define (system-buildhash) ((c-lambda () char-string "system_buildhash")))
(define (system-buildepoch) ((c-lambda () unsigned-int "system_buildepoch")))
(define (system-builddate) (seconds->string (system-buildepoch) "%Y-%m-%d"))
(define (system-builddatetime) (seconds->string (system-buildepoch) "%Y-%m-%d %H:%M:%S"))

(define system-cmdargv (c-lambda (int) char-string "system_cmdargv"))
(define system-cmdargc (c-lambda () int "system_cmdargc"))
(define (system-cmdarg) (system-cmdargv 1)) ;; backwards compatibility

(define force-terminate (c-lambda () void "force_terminate"))

(define microgl-swapbuffers
  (c-lambda
   () void "
#if !defined(STANDALONE)
 microgl_swapbuffers();
#endif
"))

(define microgl-window
  (c-lambda
   (int int) void "
#if !defined(STANDALONE) && !defined(USECONSOLE)
 microgl_window(___arg1, ___arg2);
#endif
"))

(define microgl-fullscreen
  (c-lambda
   (int int) void "
#if !defined(STANDALONE) && !defined(USECONSOLE)
 microgl_fullscreen(___arg1, ___arg2);
#endif
"))

(c-declare
 #<<end-of-decl
#define MICROGL_REDRAW_PERIOD 250000 // micro seconds
unsigned int microgl_redraw_period(unsigned int arg) {
 // microgl_check_period is for the C side to query.
 static unsigned int x = MICROGL_REDRAW_PERIOD;
  // if arg is zero, return current value only
  if( arg > 0 ) x = arg;
  return x;
}
end-of-decl
)

(define (microgl-redraw-period . arg)
  (cond
   ((null? arg) ((c-lambda (unsigned-int) unsigned-int "microgl_redraw_period") 0))
   (else ((c-lambda (unsigned-int) unsigned-int "microgl_redraw_period") (car arg)))))

;; Cleanup and exit with given exit code.  (Unlike force-terminate,
;; which always exists with zero.)
;;
;; Overriding ##exit helps to avoid segfault when leaving a gambit
;; repl and simillar situations.
(set! ##exit
      (lambda (#!optional (code 0))
        ((c-lambda (int) void "lambdanative_exit") code)))

(if (not (file-exists? (system-directory)))
  (with-exception-catcher (lambda (e) #f)
    (lambda () (create-directory (system-directory)))))

;; Gain access to Android app_directory_files and app_code_path
(define android-get-filesdir (c-lambda () char-string "android_getFilesDir"))
(define android-get-codepath (c-lambda () char-string "android_getPackageCodePath"))

;; eof
