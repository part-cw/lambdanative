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

;; get gyroscope data (iphone 4+, some Android devices)

(c-declare  #<<end-of-c-declare

#ifdef IOS
  double ios_gyro_yaw(void);
  double ios_gyro_pitch(void);
  double ios_gyro_roll(void);
#endif

#ifdef ANDROID
  double android_gyro_yaw(void);
  double android_gyro_pitch(void);
  double android_gyro_roll(void);
#endif

static double gyro_yaw(void){
#ifdef IOS
  return ios_gyro_yaw();
#elif ANDROID
  return android_gyro_yaw();
#else
  return 0;
#endif
}

static double gyro_pitch(void) {
#ifdef IOS
  return ios_gyro_pitch();
#elif ANDROID
  return android_gyro_pitch();
#else
  return 0;
#endif
}

static double gyro_roll(void) {
#ifdef IOS
  return ios_gyro_roll();
#elif ANDROID
  return android_gyro_roll();
#else
  return 0;
#endif
}

end-of-c-declare
)

(define gyro-yaw (c-lambda () double "gyro_yaw"))
(define gyro-pitch (c-lambda () double "gyro_pitch"))
(define gyro-roll (c-lambda () double "gyro_roll"))

;; eof