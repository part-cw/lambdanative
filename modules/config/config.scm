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
;; init.scm
;; this is the glue between the native launcher and the portable code

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "CONFIG.h"

#if defined(MACOSX) || defined(IOS)
#include <CoreFoundation/CoreFoundation.h>
#endif

#ifndef USECONSOLE

#if  defined(IOS)
extern char* iphone_directory;
#endif

#endif // !USECONSOLE

#ifdef USECONSOLE
extern 
#endif
char *cmd_arg1;

//#include "CONFIG.h"

#if defined(IOS) || defined(MACOSX)
static char *macosx_directory=0;
void find_macosx_app_directory() 
{
  char path[1024];
  CFBundleRef mainBundle = CFBundleGetMainBundle();
  CFURLRef mainBundleURL = CFBundleCopyBundleURL( mainBundle);
  CFStringRef cfStringRef = CFURLCopyFileSystemPath( mainBundleURL, kCFURLPOSIXPathStyle);
  CFStringGetCString( cfStringRef, path, 1024, kCFStringEncodingASCII);
  CFRelease( mainBundleURL);
  CFRelease( cfStringRef);
  macosx_directory=strdup(path);
}
#endif

// store the program info
static unsigned int sys_buildepoch;
static char *sys_dir, *sys_appdir, *sys_platform, *sys_appname, *sys_appversion, *sys_cmdarg;
static char *sys_buildhash;
static char *sys_repository, *sys_repositorydate;
static void system_init(void)
{

#ifndef USECONSOLE

// get system path on the iphone
#if defined(IOS)
  find_macosx_app_directory();
  // check for jail break
  // XXX does this violate app store requirements?
  FILE *fd = fopen("/var/mobile/tmp.341231","a");
  if (fd) {
    fclose(fd);
    remove("/var/mobile/tmp.341231");
    sys_dir = strdup(macosx_directory);
  } else {
    sys_dir = strdup(iphone_directory);
  }
  sys_appdir=strdup(macosx_directory);
#endif

// get system path on a mac
#if defined(MACOSX)
  find_macosx_app_directory();
  sys_dir = strdup(macosx_directory);
#endif

#endif // !USECONSOLE

#if defined(LINUX) || defined(OPENBSD) || defined(NETBSD) || defined(WIN32) || defined(USECONSOLE)
  // application path on unixy systems
  char path[1024];
  getcwd(path,1024);
  sys_dir = strdup(path);
#endif

#if defined(ANDROID)
// we put files on the sdcard, that's the only sane place (?)
  char path[1024];
  sprintf(path,"/sdcard/%s", SYS_APPNAME);
  sys_dir=strdup(path);
#endif

  sys_platform=strdup(SYS_PLATFORM);
  sys_appname=strdup(SYS_APPNAME);
  sys_appversion=strdup(SYS_APPVERSION);
  sys_buildhash=strdup(SYS_BUILDHASH);
  sys_buildepoch=SYS_BUILDEPOCH;
}

// report the path separator
#ifdef WIN32
#define PATH_SEPARATOR '\\'
#else
#define PATH_SEPARATOR '/'
#endif
static char system_pathseparator(void) { return PATH_SEPARATOR; }

// report the system info
char *system_dir(void) { return sys_dir; }
static char *system_appdir(void) { return sys_appdir; }
static char *system_platform(void) { return sys_platform; }
static char *system_appname(void) { return sys_appname; }
static char *system_appversion(void) { return sys_appversion; }
static char *system_cmdarg(void) { return cmd_arg1; }
static char *system_buildhash(void) { return sys_buildhash; }
static unsigned int system_buildepoch(void) { return sys_buildepoch; }

void force_terminate()
{
  ___cleanup();
  exit(0);
}

end-of-c-declare
)

;; prep the system info
(c-initialize "system_init();")

(if (string=? (system-platform) "android") (##heartbeat-interval-set! -1.0))

(define ##now 0.)

(define (system-pathseparator) 
  (string ((c-lambda () char "system_pathseparator"))))

(define (system-directory) ((c-lambda () char-string "system_dir")))
(define (system-appdirectory) ((c-lambda () char-string "system_appdir")))
(define (system-platform) ((c-lambda () char-string "system_platform")))
(define (system-appname) ((c-lambda () char-string "system_appname")))
(define (system-appversion) ((c-lambda () char-string "system_appversion")))
(define (system-cmdarg) ((c-lambda () char-string "system_cmdarg")))
(define (system-buildhash) ((c-lambda () char-string "system_buildhash")))
(define (system-buildepoch) ((c-lambda () unsigned-int "system_buildepoch")))
(define (system-builddate) (seconds->string (system-buildepoch) "%Y-%m-%d"))
(define (system-builddatetime) (seconds->string (system-buildepoch) "%Y-%m-%d %H:%M:%S"))

(define force-terminate (c-lambda () void "force_terminate"))

(if (not (file-exists? (system-directory)))
  (with-exception-catcher (lambda (e) #f) 
    (lambda () (create-directory (system-directory)))))

;; eof
