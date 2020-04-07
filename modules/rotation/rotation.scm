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
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURrot ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DArotES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DArotE.
|#

;; get rotation vextor data from devices (only android, iOS to do)

(c-declare  #<<end-of-c-declare

#ifdef IOS
  double ios_rot_getq1(void);
  double ios_rot_getq2(void);
  double ios_rot_getq3(void);
  double ios_rot_getq4(void);
  double ios_rot_getaccuracy(void);
#endif

#ifdef ANDROID
  double android_rot_getq1(void);
  double android_rot_getq2(void);
  double android_rot_getq3(void);
  double android_rot_getq4(void);
  double android_rot_getaccuracy(void);
#endif


static double rot_q1(void) {
#ifdef IOS
  return 0; //ios_rot_getq1();
#elif ANDROID
  return android_rot_getq1();
#else
  return 0.;
#endif
}
static double rot_q2(void) {
#ifdef IOS
  return 0; //ios_rot_getq2();
#elif ANDROID
  return android_rot_getq2();
#else
  return 0.;
#endif
}
static double rot_q3(void) {
#ifdef IOS
  return 0; //ios_rot_getq3();
#elif ANDROID
  return android_rot_getq3();
#else
  return 0.;
#endif
}      
static double rot_q4(void) {
#ifdef IOS
  return 0; //ios_rot_getq4();
#elif ANDROID
  return android_rot_getq4();
#else
  return 0.;
#endif
}             
static double rot_accuracy(void) {
#ifdef IOS
  return 0; //ios_rot_getaccuracy();
#elif ANDROID
  return android_rot_getaccuracy();
#else
  return 0.;
#endif           
}             
end-of-c-declare
)

(define rot-q1 (c-lambda () double "rot_q1")) ;;x*sin(θ/2)
(define rot-q2 (c-lambda () double "rot_q2")) ;;[1]: y*sin(θ/2)
(define rot-q3 (c-lambda () double "rot_q3")) ;;[2]: z*sin(θ/2)
(define rot-q4 (c-lambda () double "rot_q4")) ;;[3]: cos(θ/2)
(define rot-accuracy (c-lambda () double "rot_accuracy")) ;;[4]: estimated heading Accuracy (in radians) (-1 if unavailable)

;; eof
