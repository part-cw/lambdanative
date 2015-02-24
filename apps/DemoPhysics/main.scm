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

;; simple physics example adapted from chipmunk documentation

(define physicsTime 0.)
(define space #f)
(define ground #f)
(define ballBody #f)
(define ballShape #f)

(define (physics-setup)
  (set! space (cpSpaceNew))
  (cpSpaceSetGravity space (cpv 0. -100.))
  (set! ground (cpSegmentShapeNew (cpSpaceGetStaticBody space) (cpv -20. 5.) (cpv 20. -5.) 0.))
  (cpShapeSetFriction ground 1.)
  (cpSpaceAddShape space ground)
  (let* ((radius 5.)
         (mass 1.)
         (moment (cpMomentForCircle mass 0. radius cpvzero)))
    (set! ballBody (cpSpaceAddBody space (cpBodyNew  mass moment)))
    (cpBodySetPos ballBody (cpv 0. 15.))
    (set! ballShape (cpSpaceAddShape space (cpCircleShapeNew ballBody radius cpvzero)))
  )
  (cpShapeSetFriction ballShape 0.7)
)

(define (physics-reset)
  (cpBodySetVel ballBody (cpv 0. 0.))
  (cpBodySetPos ballBody (cpv 0. 15.))
  (set! physicsTime 0.))

(define (physics-step)
  (let ((timeStep (/ 1. 60.))
        (pos (cpBodyGetPos ballBody))
        (vel (cpBodyGetVel ballBody))
        (angle (cpBodyGetAngle ballBody)))
;;    (for-each display (list "Time is " physicsTime " ballBody is at (" 
;;       (cpVect.x pos) "," (cpVect.y pos) "). It's velocity is (" 
;;       (cpVect.x vel) "," (cpVect.y vel) "). It's angle is " angle "\n"))
    (glgui-widget-set! gui soccerball 'y (* 30 (cpVect.y pos)))
    (glgui-widget-set! gui soccerball 'x (+ 100. (* 30 (cpVect.x pos))))
    (glgui-widget-set! gui soccerball 'angle (* angle 30))
    (glgui-widget-set! gui soccerball 'hidden #f)
    (cpSpaceStep space timeStep)
    (set! physicsTime (+ physicsTime timeStep))))

(define (physics-cleanup)
  (cpShapeFree ballShape)
  (cpBodyFree ballBody)
  (cpShapeFree ground)
  (cpSpaceFree space))

(define gui #f)
(define soccerball #f)

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (physics-setup)
    (let ((w 320)(h 480))
      (set! gui (make-glgui))
      (glgui-box gui 0 0 w h (list LightCyan LightCyan White White))
      (let ((wgt (glgui-pixmap gui 0 0 field.img w 240)))
        (glgui-widget-set! gui wgt 'color (color-shade LawnGreen 0.8)))
      (set! soccerball (glgui-sprite gui 'x 0 'y 0 'image soccer.img 'color Black))
      (glgui-widget-set! gui soccerball 'hidden #t)
    )
  )
;; events
  (lambda (t x y)
    (physics-step)
    (if (> physicsTime 1.) (physics-reset))
    (if (= t EVENT_KEYPRESS) (begin
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-event gui t x y))
;; termination
  (lambda () (physics-cleanup))
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof  
