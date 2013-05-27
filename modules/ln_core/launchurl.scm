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

;; Launch a URL in a browser window

(c-declare  #<<end-of-c-declare

#ifdef IOS
#import <UIKit/UIKit.h>
static void ios_launch_url(char *urlchar){
  NSString* urlString = [NSString stringWithUTF8String: urlchar];
  NSURL *url = [NSURL URLWithString:urlString];
  [[UIApplication sharedApplication] openURL:url];
}
#endif

#ifdef WIN32
#include <windows.h>
#endif

#ifdef MACOSX
#import <ApplicationServices/ApplicationServices.h>
static void macosx_launch_url(char *urlstring){
  CFStringRef cfurlstring = CFStringCreateWithCString(NULL,urlstring,0);
  CFURLRef url = CFURLCreateWithString ( kCFAllocatorDefault, cfurlstring, NULL );
  if (url) LSOpenCFURLRef(url,NULL);
  CFRelease(url);
  CFRelease(cfurlstring);
}
#endif

#ifdef ANDROID
// Actual code has to be in Java, so glued using JNI
void android_launch_url(char *urlstring);
#endif

#ifdef LINUX
#include <unistd.h>
#include <stdlib.h>
static void linux_launch_url(char *url){
  int pid=fork();
  if (pid == 0) {
    execl("/usr/bin/xdg-open", "xdg-open", url, (char *)0);
    exit(1);
  }
}
#endif

static void launch_url(char *urlstring){
  #if defined(WIN32)
    ShellExecute(NULL,"open",urlstring,NULL,NULL,SW_SHOWDEFAULT);
  #endif
  #ifdef IOS
    ios_launch_url(urlstring);
  #endif
  #if defined(MACOSX)
    macosx_launch_url(urlstring);
  #endif
  #if defined(LINUX)
    linux_launch_url(urlstring);
  #endif
  #ifdef ANDROID
    android_launch_url(urlstring);
  #endif
}

end-of-c-declare
)

(define launch-url (c-lambda (char-string) void "launch_url"))

;; eof
