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

;; camera support

(define camera:debuglevel 0)
(define (camera:log level . x)
   (if (>= camera:debuglevel level) 
      (apply log-system (append (list "camera: ") x))))

;; Maximum length of video in seconds
;; Default is no maximum (represented as 0)
(define camera:maxlength-video 0)

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef QNX
  void qnx_camera_start(char *);
  void qnx_videocamera_start(char *);
#endif

#ifdef IOS
  void ios_camera_start(char *);
  void ios_videocamera_start(char *, int);
#endif 

#ifdef ANDROID
  void android_camera_start(char *, char *);
  void android_camera_start_video(char *, char *, int);
#endif

static void camera_start(char *filename)
{
#ifdef ANDROID
  char tmp_name[1024];
  int i,len=strlen(filename)-1;
  for (i=len;i>0;i--) { if (filename[i]=='/') break; }
  filename[i]=0;
  sprintf(tmp_name,"%s/_camera_tmp.jpg",filename);
  filename[i]='/';
  android_camera_start(filename,tmp_name);
#endif
#ifdef IOS
  ios_camera_start(filename);
#endif
#ifdef QNX
  qnx_camera_start(filename);
#endif
}

static void camera_start_video(char *filename, int maxlength)
{
#ifdef ANDROID
  char tmp_name[1024];
  int i,len=strlen(filename)-1;
  for (i=len;i>0;i--) { if (filename[i]=='/') break; }
  filename[i]=0;
  sprintf(tmp_name,"%s/_videocamera_tmp.mp4",filename);
  filename[i]='/';
  android_videocamera_start(filename,tmp_name,maxlength);
#endif
#ifdef IOS
  ios_videocamera_start(filename, maxlength);
#endif
#ifdef QNX
  qnx_videocamera_start(filename);
#endif
}

end-of-c-declare
)

(define (camera-start imagefile)
  (camera:log 1 "camera-start " imagefile)
  (if (file-exists? imagefile) (delete-file imagefile))
  ((c-lambda (char-string) void "camera_start") imagefile)
 )


(define (camera-set-max-length-video l)
  (set! camera:maxlength-video l))

(define (camera-start-video videofile)
  (camera:log 1 "camera-start-video " videofile)
  (if (file-exists? videofile) (delete-file videofile))
  ((c-lambda (char-string int) void "camera_start_video") videofile camera:maxlength-video)
 )

;;eof
