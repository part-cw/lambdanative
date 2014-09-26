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

;; bindings to the chipmunk physics engine

(c-declare  #<<end-of-c-declare

#include <chipmunk/chipmunk.h>

end-of-c-declare
)

(c-define-type cpFloat double)
(c-define-type cpBool int)

(c-define-type cpLayers "cpLayers")
(c-define-type cpGroup "cpGroup")
(c-define-type cpBB "cpBB")

(c-define-type cpSegmentQueryInfo "cpSegmentQueryInfo")
(c-define-type cpSegmentQueryInfo* (pointer cpSegmentQueryInfo))
(c-define-type cpNearestPointQueryInfo "cpNearestPointQueryInfo")
(c-define-type cpNearestPointQueryInfo* (pointer cpNearestPointQueryInfo))

(c-define-type cpVect "cpVect")
(c-define-type cpVect* (pointer cpVect))
(c-define-type cpSpace "cpSpace")
(c-define-type cpSpace* (pointer cpSpace))
(c-define-type cpShape "cpShape")
(c-define-type cpShape* (pointer cpShape))
(c-define-type cpShape** (pointer cpShape*))
(c-define-type cpCircleShape "cpCircleShape")
(c-define-type cpCircleShape* (pointer cpCircleShape))
(c-define-type cpSegmentShape "cpSegmentShape")
(c-define-type cpSegmentShape* (pointer cpSegmentShape))
(c-define-type cpArray "cpArray")
(c-define-type cpArray* (pointer cpArray))
(c-define-type cpBody "cpBody")
(c-define-type cpBody* (pointer cpBody))
(c-define-type cpBody** (pointer cpBody*))
(c-define-type cpConstraint "cpConstraint")
(c-define-type cpConstraint* (pointer cpConstraint))
(c-define-type cpArbiter "cpArbiter")
(c-define-type cpArbiter* (pointer cpArbiter))

(c-define-type cpContactPointSet "cpContactPointSet")

(define cpv (c-lambda (cpFloat cpFloat) cpVect "cpv"))
(define cpvzero (cpv 0. 0.))

(define cpVect.x (c-lambda (cpVect) cpFloat "___result=___arg1.x;"))
(define cpVect.y (c-lambda (cpVect) cpFloat "___result=___arg1.y;"))

;; Arbiter

(define cpArbiterGetSurfaceVelocity (c-lambda (cpArbiter*) cpVect "cpArbiterGetSurfaceVelocity"))
(define cpArbiterSetSurfaceVelocity (c-lambda (cpArbiter* cpVect) void "cpArbiterSetSurfaceVelocity"))
(define cpArbiterTotalImpulse (c-lambda (cpArbiter*) cpVect "cpArbiterTotalImpulse"))
(define cpArbiterTotalImpulseWithFriction (c-lambda (cpArbiter*) cpVect "cpArbiterTotalImpulseWithFriction"))
(define cpArbiterTotalKE (c-lambda (cpArbiter*) cpFloat "cpArbiterTotalKE"))
(define cpArbiterIgnore (c-lambda (cpArbiter*) void "cpArbiterIgnore"))
(define cpArbiterGetBodies (c-lambda (cpArbiter* cpBody** cpBody**) void "cpArbiterGetBodies"))
(define cpArbiterGetShapes (c-lambda (cpArbiter* cpShape** cpShape**) void "cpArbiterGetShapes"))
(define cpArbiterGetDepth (c-lambda (cpArbiter* int) cpFloat "cpArbiterGetDepth"))
(define cpArbiterGetPoint (c-lambda (cpArbiter* int) cpVect "cpArbiterGetPoint"))
(define cpArbiterGetNormal (c-lambda (cpArbiter* int) cpVect "cpArbiterGetNormal"))
(define cpArbiterGetCount (c-lambda (cpArbiter*) int "cpArbiterGetCount"))
(define cpArbiterIsFirstContact (c-lambda (cpArbiter*) cpBool "cpArbiterIsFirstContact"))
(define cpArbiterGetContactPointSet (c-lambda (cpArbiter*) cpContactPointSet "cpArbiterGetContactPointSet"))

;; BB

(define cpBBWrapVect (c-lambda (cpBB cpVect) cpVect "cpBBWrapVect"))
(define cpBBClampVect (c-lambda (cpBB cpVect) cpVect "cpBBClampVect"))
(define cpBBIntersectsSegment (c-lambda (cpBB cpVect cpVect) cpBool "cpBBIntersectsSegment"))
(define cpBBSegmentQuery (c-lambda (cpBB cpVect cpVect) cpFloat "cpBBSegmentQuery"))
(define cpBBMergedArea (c-lambda (cpBB cpBB) cpFloat "cpBBMergedArea"))
(define cpBBArea (c-lambda (cpBB) cpFloat "cpBBArea"))
(define cpBBCenter (c-lambda (cpBB) cpVect "cpBBCenter"))
(define cpBBExpand (c-lambda (cpBB cpVect) cpBB "cpBBExpand"))
(define cpBBMerge (c-lambda (cpBB cpBB) cpBB "cpBBMerge"))
(define cpBBContainsVect (c-lambda (cpBB cpVect) cpBool "cpBBContainsVect"))
(define cpBBContainsBB (c-lambda (cpBB cpBB) cpBool "cpBBContainsBB"))
(define cpBBIntersects (c-lambda (cpBB cpBB) cpBool "cpBBIntersects"))
(define cpBBNewForCircle (c-lambda (cpVect cpFloat) cpBB "cpBBNewForCircle"))
(define cpBBNew (c-lambda (cpFloat cpFloat cpFloat cpFloat) cpBB "cpBBNew"))

;; space

(define cpSpaceAlloc (c-lambda () cpSpace* "cpSpaceAlloc"))
(define cpSpaceNew (c-lambda () cpSpace* "cpSpaceNew"))
(define cpSpaceInit (c-lambda (cpSpace*) cpSpace* "cpSpaceInit"))
(define cpSpaceDestroy (c-lambda (cpSpace*) void "cpSpaceDestroy"))
(define cpSpaceFree (c-lambda (cpSpace*) void "cpSpaceFree"))
(define cpSpaceAddShape (c-lambda (cpSpace* cpShape*) cpShape* "cpSpaceAddShape"))
(define cpSpaceAddStaticShape (c-lambda (cpSpace* cpShape*) cpShape* "cpSpaceAddStaticShape"))
(define cpSpaceAddBody (c-lambda (cpSpace* cpBody*) cpBody* "cpSpaceAddBody"))
(define cpSpaceAddConstraint (c-lambda (cpSpace* cpConstraint*) cpConstraint* "cpSpaceAddConstraint"))
(define cpSpaceRemoveShape (c-lambda (cpSpace* cpShape*) void "cpSpaceRemoveShape"))
(define cpSpaceRemoveStaticShape (c-lambda (cpSpace* cpShape*) void "cpSpaceRemoveStaticShape"))
(define cpSpaceRemoveBody (c-lambda (cpSpace* cpBody*) void "cpSpaceRemoveBody"))
(define cpSpaceRemoveConstraint (c-lambda (cpSpace* cpConstraint*) void "cpSpaceRemoveConstraint"))
(define cpSpaceContainsShape (c-lambda (cpSpace* cpShape*) cpBool "cpSpaceContainsShape"))
(define cpSpaceContainsBody (c-lambda (cpSpace* cpBody*) cpBool "cpSpaceContainsBody"))
(define cpSpaceContainsConstraint (c-lambda (cpSpace* cpConstraint*) cpBool "cpSpaceContainsConstraint"))
(define cpSpaceConvertBodyToStatic (c-lambda (cpSpace* cpBody*) void "cpSpaceConvertBodyToStatic"))
(define cpSpaceConvertBodyToDynamic (c-lambda (cpSpace* cpBody* cpFloat cpFloat) void "cpSpaceConvertBodyToDynamic"))
(define cpSpacePointQueryFirst (c-lambda (cpSpace* cpVect cpLayers cpGroup) cpShape* "cpSpacePointQueryFirst"))
(define cpSpaceNearestPointQueryNearest (c-lambda (cpSpace* cpVect cpFloat cpLayers cpGroup cpNearestPointQueryInfo*) cpShape*
  "cpSpaceNearestPointQueryNearest"))
(define cpSpaceSegmentQueryFirst (c-lambda (cpSpace* cpVect cpVect cpLayers cpGroup cpSegmentQueryInfo*) cpShape* 
   "cpSpaceSegmentQueryFirst"))
(define cpSpaceActivateShapesTouchingShape (c-lambda (cpSpace* cpShape*) void "cpSpaceActivateShapesTouchingShape"))
(define cpSpaceReindexStatic (c-lambda (cpSpace*) void "cpSpaceReindexStatic"))
(define cpSpaceReindexShape (c-lambda (cpSpace* cpShape*) void "cpSpaceReindexShape"))
(define cpSpaceReindexShapesForBody (c-lambda (cpSpace* cpBody*) void "cpSpaceReindexShapesForBody"))
(define cpSpaceUseSpatialHash (c-lambda (cpSpace* cpFloat int) void "cpSpaceUseSpatialHash"))
(define cpSpaceStep (c-lambda (cpSpace* cpFloat) void "cpSpaceStep"))
(define cpSpaceSetGravity (c-lambda (cpSpace* cpVect) void "cpSpaceSetGravity"))
(define cpSpaceGetGravity (c-lambda (cpSpace*) cpVect "cpSpaceGetGravity"))
(define cpSpaceGetStaticBody (c-lambda (cpSpace*) cpBody* "___result=___arg1->staticBody;"))

;; shape

(define cpShapeDestroy (c-lambda (cpShape*) void "cpShapeDestroy"))
(define cpShapeFree (c-lambda (cpShape*) void "cpShapeFree"))
(define cpShapeCacheBB (c-lambda (cpShape*) cpBB "cpShapeCacheBB"))
(define cpShapeUpdate (c-lambda (cpShape* cpVect cpVect) cpBB "cpShapeUpdate"))
(define cpShapePointQuery (c-lambda (cpShape* cpVect) cpBool "cpShapePointQuery"))
(define cpShapeNearestPointQuery (c-lambda (cpShape* cpVect cpNearestPointQueryInfo*) cpBool "cpShapeNearestPointQuery"))
(define cpShapeSegmentQuery (c-lambda (cpShape* cpVect cpVect cpSegmentQueryInfo*) cpBool "cpShapeSegmentQuery"))
(define cpSegmentQueryHitPoint (c-lambda (cpVect cpVect cpSegmentQueryInfo) cpVect "cpSegmentQueryHitPoint"))
(define cpSegmentQueryHitDist (c-lambda (cpVect cpVect cpSegmentQueryInfo) cpFloat "cpSegmentQueryHitDist"))
(define cpShapeSetBody (c-lambda (cpShape* cpBody*) void "cpShapeSetBody"))
(define cpResetShapeIdCounter (c-lambda () void "cpResetShapeIdCounter"))
(define cpCircleShapeAlloc (c-lambda () cpCircleShape* "cpCircleShapeAlloc"))
(define cpCircleShapeInit (c-lambda (cpCircleShape* cpBody* cpFloat cpVect) cpCircleShape* "cpCircleShapeInit"))
(define cpCircleShapeNew (c-lambda (cpBody* cpFloat cpVect) cpShape* "cpCircleShapeNew"))
(define cpSegmentShapeAlloc (c-lambda () cpSegmentShape* "cpSegmentShapeAlloc"))
(define cpSegmentShapeInit (c-lambda (cpSegmentShape* cpBody* cpVect cpVect cpFloat) cpSegmentShape* "cpSegmentShapeInit"))
(define cpSegmentShapeNew (c-lambda (cpBody* cpVect cpVect cpFloat) cpShape* "cpSegmentShapeNew"))
(define cpShapeSetFriction (c-lambda (cpShape* cpFloat) void "cpShapeSetFriction"))
(define cpShapeGetFriction (c-lambda (cpShape*) cpFloat "cpShapeGetFriction"))

;; body

(define cpBodyAlloc (c-lambda () cpBody* "cpBodyAlloc"))
(define cpBodyInit (c-lambda (cpBody* cpFloat cpFloat) cpBody* "cpBodyInit"))
(define cpBodyNew (c-lambda (cpFloat cpFloat) cpBody* "cpBodyNew"))
(define cpBodyInitStatic (c-lambda (cpBody*) cpBody* "cpBodyInitStatic"))
(define cpBodyNewStatic (c-lambda () cpBody* "cpBodyNewStatic"))
(define cpBodyDestroy (c-lambda (cpBody*) void "cpBodyDestroy"))
(define cpBodyFree (c-lambda (cpBody*) void "cpBodyFree"))
(define cpBodyActivate (c-lambda (cpBody*) void "cpBodyActivate"))
(define cpBodyActivateStatic (c-lambda (cpBody* cpShape*) void "cpBodyActivateStatic"))
(define cpBodySleep (c-lambda (cpBody*) void "cpBodySleep"))
(define cpBodySleepWithGroup (c-lambda (cpBody* cpBody*) void "cpBodySleepWithGroup"))
(define cpBodyIsSleeping (c-lambda (cpBody*) cpBool "cpBodyIsSleeping"))
(define cpBodyIsStatic (c-lambda (cpBody*) cpBool "cpBodyIsStatic"))
;;(define cpBodyIsRouge (c-lambda (cpBody*) cpBool "cpBodyIsRouge"))
(define cpBodyUpdateVelocity (c-lambda (cpBody* cpVect cpFloat cpFloat) void "cpBodyUpdateVelocity"))
(define cpBodyUpdatePosition (c-lambda (cpBody* cpFloat) void "cpBodyUpdatePosition"))
(define cpBodyLocal2World (c-lambda (cpBody* cpVect) cpVect "cpBodyLocal2World"))
(define cpBodyWorld2Local (c-lambda (cpBody* cpVect) cpVect "cpBodyWorld2Local"))
(define cpBodyResetForces (c-lambda (cpBody*) void "cpBodyResetForces"))
(define cpBodyApplyForce (c-lambda (cpBody* cpVect cpVect) void "cpBodyApplyForce"))
(define cpBodyApplyImpulse (c-lambda (cpBody* cpVect cpVect) void "cpBodyApplyImpulse"))
(define cpBodyGetVelAtWorldPoint (c-lambda (cpBody* cpVect) cpVect "cpBodyGetVelAtWorldPoint"))
(define cpBodyGetVelAtLocalPoint (c-lambda (cpBody* cpVect) cpVect "cpBodyGetVelAtLocalPoint"))
(define cpBodyKineticEnergy (c-lambda (cpBody*) cpFloat "cpBodyKineticEnergy"))
(define cpBodySetPos (c-lambda (cpBody* cpVect) void "cpBodySetPos"))
(define cpBodyGetPos (c-lambda (cpBody*) cpVect "cpBodyGetPos"))
(define cpBodySetVel (c-lambda (cpBody* cpVect) void "cpBodySetVel"))
(define cpBodyGetVel (c-lambda (cpBody*) cpVect "cpBodyGetVel"))
(define cpBodyGetAngle (c-lambda (cpBody*) cpFloat "cpBodyGetAngle"))

;; misc

(define cpMomentForCircle (c-lambda (cpFloat cpFloat cpFloat cpVect) cpFloat "cpMomentForCircle"))

;; eof
