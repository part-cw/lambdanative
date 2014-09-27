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

;; get physical location from devices (ios and android only)

(c-declare  #<<end-of-c-declare

#include <android/log.h>

#ifdef IOS
  double ios_location_getlatitude(void);
  double ios_location_getlongitude(void);
  double ios_location_getaltitude(void);
  double ios_location_getaccuracy(void);
  double ios_location_gettimestamp(void);
#endif

#ifdef ANDROID
  void android_location_toggleGPS(int);
  double android_location_getlatitude(void);
  double android_location_getlongitude(void);
  double android_location_getaltitude(void);
  double android_location_getaccuracy(void);
  double android_location_gettimestamp(void);
#endif

#ifdef BB10
  double qnx_location_getlatitude(void);
  double qnx_location_getlongitude(void);
  double qnx_location_getlatitude(void);
  double qnx_location_getaccuracy(void);
  double qnx_location_gettimestamp(void);
#endif

void enable_gps(void){
#ifdef IOS

#elif ANDROID
  android_location_toggleGPS(1);
#elif BB10

#else
   return;
#endif
}

void disable_gps(void){
#ifdef IOS

#elif ANDROID
  android_location_toggleGPS(0);
#elif BB10

#else
   return;
#endif
}


static double longitude(void){
#ifdef IOS
   return ios_location_getlongitude();
#elif ANDROID
   return android_location_getlongitude();
#elif BB10
   return qnx_location_getlongitude();
#else
   return 0;
#endif
}

static double latitude(void){
#ifdef IOS
   return ios_location_getlatitude();
#elif ANDROID
   return android_location_getlatitude();
#elif BB10
   return qnx_location_getlatitude();
#else
   return 0;
#endif
}

static double altitude(void){
#ifdef IOS
   return ios_location_getaltitude();
#elif ANDROID
   return android_location_getaltitude();
#elif BB10
   return qnx_location_getaltitude();
#else
   return 0;
#endif
}

static double accuracy(void){
#ifdef IOS
   return ios_location_getaccuracy();
#elif ANDROID
   return android_location_getaccuracy();
#elif BB10
   return qnx_location_getaccuracy();
#else
   return 0;
#endif
}

static double timestamp(void){
#ifdef IOS
   return ios_location_gettimestamp();
#elif ANDROID
   return android_location_gettimestamp();
#elif BB10
   return qnx_location_getaccuracy();
#else
   return 0;
#endif
}


end-of-c-declare
)

(define gps-enable (c-lambda () void "enable_gps"))
(define gps-disable (c-lambda () void "disable_gps"))
(define gps-latitude (c-lambda () double "latitude"))
(define gps-longitude (c-lambda () double "longitude"))
(define gps-accuracy (c-lambda () double "accuracy"))
(define gps-altitude (c-lambda () double "altitude"))
(define gps-timestamp (c-lambda () double "timestamp"))
;; eof
