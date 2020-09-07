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

;; returns the rotation vector as list x y z w
(define (get-rotation)
  (let ((rot (list (rotation-x) (rotation-y) (rotation-z) (rotation-w))))
     rot)
  )

;; converts into radians from angle;  number or list
(define (rotation:rad angle)
   (let ((pi (* (atan 1.) 4)))
     (if (list? angle) (map (lambda (a) (flo (/ (* a pi) 180.))) angle)
  (/ (* angle pi) 180.))
  ))

;; converts into angles from radians; rad is number or list
(define (rotation:angle rad)
  (let ((pi (* (atan 1.) 4)))
    (if (list? rad) (map (lambda (r) (flo (* (/ r pi) 180. ))) rad)
  (* (/ rad pi) 180. ))
))

;; gets roll from rotation list rot or current sensor value
(define (rotation-roll . rot)
; roll (x-axis rotation)
 (let* ((v (if (> (length rot) 0) rot (get-rotation)))
        (x (car v))(y (cadr v))(z (caddr v))(w (cadddr v))
        (sinr-cosp (* 2.  (+ (* z y) (* w x))))
       (cosr-cosp (- 1.   (* 2.  (+ (* x  y) (* y y))))))
    (atan sinr-cosp  cosr-cosp))
)

;; gets pitch from rotation list or current sensor value
(define (rotation-pitch . rot)
; pitch (y-axis (rotation)
 (let* ((v (if (> (length rot) 0) rot (get-rotation)))
        (x (car v))(y (cadr v))(z (caddr v))(w (cadddr v))
      (pi (* (atan 1.) 4))
      (sinp   (* 2.  (-  (* w y) (* z x)))))
    (if (< 1. (abs sinp))
        (*  (/ pi 2. ) (sign sinp)) (asin sinp))
  ))

;; gets yaw from rotation list or current sensor value
(define (rotation-yaw . rot)
;; yaw (z-axis (rotation)
 (let* ((v (if (> (length rot) 0) rot (get-rotation)))
       (x (car v))(y (cadr v))(z (caddr v))(w (cadddr v))
       (siny-cosp (* 2.  (+ (* x y) (* w z)))) 
       (cosy-cosp (- 1.   (* 2.  (+ (* z z) (* y y))))))
   (atan siny-cosp  cosy-cosp)
))

;; get orientation angles from rotation list or current sensor values
(define (get-orientation-angles . rot)
  (let* ((v (if (> (length rot) 0) rot (get-rotation)))
         (r (rotation-roll ))
         (p (rotation-pitch))
         (y (rotation-yaw ))
         (o (list r p y)))
     (rotation:angle o))
  )

;; eof
