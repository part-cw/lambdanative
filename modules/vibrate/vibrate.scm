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

;; Trigger vibration of the devices (ios and android only)
;; Note that the timed vibrations only work on Android and
;; there is no vibration on iPods, only on iPhones.
;; For vibration to work on an iPhone there must be no audio,
;; or the audio must be temporarily stopped.

(c-declare  #<<end-of-c-declare

#ifdef IOS
  void iphone_vibrate();
#endif

#ifdef ANDROID
  void android_vibrate();
  void android_timed_vibrate(int milli);
#endif

static void vibrate(){
#ifdef IOS
   iphone_vibrate();
#elif ANDROID
   android_vibrate();
#endif
}

// Vibrate for the given number of milliseconds, milliseconds ignored on iOS.
static void timed_vibrate(int milli){
#ifdef IOS
   iphone_vibrate();
#elif ANDROID
   android_timed_vibrate(milli);
#endif
}

end-of-c-declare
)

;; Vibrate for 400 milliseconds, the vibration time on an iPhone
(define vibrate (c-lambda () void "vibrate"))

;; Vibrate for a given number of milliseconds, only works on Android
(define vibrate-timed (c-lambda (int) void "timed_vibrate"))
;; eof
