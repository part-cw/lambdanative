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
  double ios_rot_getx(void);
  double ios_rot_gety(void);
  double ios_rot_getz(void);
  double ios_rot_getw(void);
#endif

#ifdef ANDROID
  double android_rot_getq1(void);
  double android_rot_getq2(void);
  double android_rot_getq3(void);
  double android_rot_getq4(void);
  double android_rot_getaccuracy(void);
#endif


static double rotation_x(void) {
#ifdef IOS
  return ios_rot_getx();
#elif ANDROID
  return android_rot_getq1();
#else
  return 0.;
#endif
}
static double rotation_y(void) {
#ifdef IOS
  return ios_rot_gety();
#elif ANDROID
  return android_rot_getq2();
#else
  return 0.;
#endif
}
static double rotation_z(void) {
#ifdef IOS
  return ios_rot_getz();
#elif ANDROID
  return android_rot_getq3();
#else
  return 0.;
#endif
}
static double rotation_w(void) {
#ifdef IOS
  return ios_rot_getw();
#elif ANDROID
  return android_rot_getq4();
#else
  return 0.;
#endif
}
static double rot_accuracy(void) {
#ifdef IOS
  return -1;
#elif ANDROID
  return android_rot_getaccuracy();
#else
  return -1;
#endif
}
end-of-c-declare
)

(define rotation-x (c-lambda () double "rotation_x")) ;;x*sin(θ/2)
(define rotation-y (c-lambda () double "rotation_y")) ;;[1]: y*sin(θ/2)
(define rotation-z (c-lambda () double "rotation_z")) ;;[2]: z*sin(θ/2)
(define rotation-w (c-lambda () double "rotation_w")) ;;[3]: cos(θ/2)
(define rotation-accuracy (c-lambda () double "rot_accuracy")) ;;[4]: estimated heading Accuracy (in radians) (-1 if unavailable)

;; eof
