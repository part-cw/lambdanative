#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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

(c-declare #<<end-of-c-declare

#ifdef IOS
  extern char localnotification_msg[100];
  extern double localnotification_timestamp;
  extern int localnotification_gotmsg;
  int ios_localnotification_schedule(char*, double, int, char*);
  int ios_localnotification_schedule_batch(char*[], double*, int*, char*[], int, int*);
  int ios_localnotification_cancelall();
  int ios_localnotification_cancel(int id);
  void ios_localnotification_renumber();
#elif ANDROID
  extern char localnotification_msg[100];
  extern double localnotification_timestamp;
  extern int localnotification_gotmsg;
  int android_localnotification_schedule(char*, double, int, char*);
  int android_localnotification_schedule_batch(char*[], double*, int*, char*[], int, int*);
  int android_localnotification_cancel(int id);
  int android_localnotification_cancelall();
#else
  char localnotification_msg[100];
  double localnotification_timestamp = 0;
  int localnotification_gotmsg = 0;
#endif

void localnotification_renumber(){
#ifdef IOS
  ios_localnotification_renumber();
#endif
}

int localnotification_schedule_batch(char* text[], double* time, int* repeattime, char* sounds[], int len, int* ret){
#ifdef IOS
  return ios_localnotification_schedule_batch(text,time,repeattime,sounds,len,ret);
#elif ANDROID
  return android_localnotification_schedule_batch(text,time,repeattime,sounds,len,ret);
#endif
  return 0;
}

int localnotification_schedule(char* text, double time, int repeatmin, char* soundfile){
#ifdef IOS
  return ios_localnotification_schedule(text, time, repeatmin, soundfile);
#elif ANDROID
  return android_localnotification_schedule(text, time, repeatmin, soundfile);
#endif
  return 0;
}

int localnotification_cancelall(){
#ifdef IOS
  return ios_localnotification_cancelall();
#elif ANDROID
  return android_localnotification_cancelall();
#endif
  return 0;
}

int localnotification_cancel(int id){
#ifdef IOS
  return ios_localnotification_cancel(id);
#elif ANDROID
  return android_localnotification_cancel(id);
#endif
  return 0;
}

void localnotification_resetmsg(){
  localnotification_gotmsg = 0;
}

end-of-c-declare
)

;; Internal variables
(define localnotification:msg "")
(define localnotification:timestamp 0.)

;; Create local notification
(define (localnotification:validate lst)
  (let ((str (car lst))
        (time (flo (cadr lst)))
        (repeat (if (fx>= (length lst) 3) (fix (caddr lst)) 0))
        (sound (if (fx= (length lst) 4) (cadddr lst) "")))
    (if (and (> time ##now) (string? str) (fx<= (string-length str) 100))
      (list str time repeat sound)
      (list "" 0 0 "")
    )
  ))

(define (localnotification-schedule str time . repeataftermin_customsound)
  (let ((nf (localnotification:validate (append (list str time) repeataftermin_customsound))))
    (if (> (cadr nf) 0)
      (let ((ret ((c-lambda (char-string double int char-string) int
                   "___result=localnotification_schedule(___arg1,___arg2,___arg3,___arg4);")
                   (car nf) (cadr nf) (caddr nf) (cadddr nf))))
        (if (fx> ret 0) (localnotification:renumber))
        (if (fx> ret 0) ret #f)
      )
      #f
    )
  ))

(define (localnotification-schedule-batch nfs0)
  (let* ((nfs (map localnotification:validate nfs0))
         (len (length nfs))
         (ret (make-u32vector len 0))
         (texts (map car nfs))
         (times (list->f64vector (map cadr nfs)))
         (repeats (list->u32vector (map caddr nfs)))
         (sounds (map cadddr nfs))
         (r ((c-lambda (nonnull-char-string-list scheme-object scheme-object nonnull-char-string-list int scheme-object) bool
           "___result=localnotification_schedule_batch(___CAST(char**,___BODY_AS(___arg1,___tSUBTYPED)),
            ___CAST(double*,___BODY_AS(___arg2,___tSUBTYPED)),___CAST(int*,___BODY_AS(___arg3,___tSUBTYPED)),
            ___CAST(char**,___BODY_AS(___arg4,___tSUBTYPED)),___arg5,___CAST(int*,___BODY_AS(___arg6,___tSUBTYPED)));")
           texts times repeats sounds len ret)))
    (if r (localnotification:renumber))
    (map (lambda (l) (if (fx> l 0) l #f)) (u32vector->list ret))
  ))

;; Renumber notifications
(define (localnotification:renumber)
  ((c-lambda () void "localnotification_renumber")))

;; Clear notifications
(define (localnotification:cancel id)
  ((c-lambda (int) int "___result=localnotification_cancel(___arg1);") id))
(define (localnotification-cancel id)
  (let ((ret (localnotification:cancel id)))
    (if (fx> ret 0) (localnotification:renumber))
    ret
  ))
(define (localnotification-cancel-batch ids)
  (let ((ret (map (lambda (n) (localnotification:cancel n)) ids)))
    (localnotification:renumber)
    ret
  ))
(define (localnotification-cancelall)
  ((c-lambda () int "___result=localnotification_cancelall();")))

;; Retrieve local notification
(define (localnotification-getalert)
  (if ((c-lambda () bool "___result=localnotification_gotmsg;"))
    (begin
      ((c-lambda () void "localnotification_resetmsg"))
      (set! localnotification:msg ((c-lambda () char-string "___result=localnotification_msg;")))
      (set! localnotification:timestamp ((c-lambda () double "___result=localnotification_timestamp;")))
      localnotification:msg
    )
    #f
  ))

;;eof
