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
;; minimal OpenGL subset 
;; NOTE: texture dimensions is an issue, although is supposed to be supported!
;; In a word, the iphone will silently refuse to render non-power of two textures
;; For maximum portability graphics should adhere to power of two restriction!

(c-declare  #<<end-of-c-declare

#ifdef MACOSX
#include <Carbon/Carbon.h>
#include <OpenGL/OpenGL.h>
#include <AGL/agl.h>
#endif

#if defined(IPHONE) || defined(IPHONESIM)
#include <OpenGLES/ES1/gl.h>
#endif

#if defined(MAEMO) || defined(MAEMOSIM) || defined(ANDROID)
#define GL_API
#define GL_APIENTRY
#include <GLES/gl.h>
#endif

#if defined(WIN32) || defined(LINUX) || defined(OPENBSD)
#include <GL/gl.h>
#endif

end-of-c-declare
)

;; definitions 

(define GL_VERTEX_ARRAY ((c-lambda () int "___result = GL_VERTEX_ARRAY;")))
(define GL_COLOR_ARRAY ((c-lambda () int "___result = GL_COLOR_ARRAY;")))
(define GL_BLEND ((c-lambda () int "___result = GL_BLEND;")))
(define GL_SRC_ALPHA ((c-lambda () int "___result = GL_SRC_ALPHA;")))
(define GL_ONE_MINUS_SRC_ALPHA ((c-lambda () int "___result = GL_ONE_MINUS_SRC_ALPHA;")))
(define GL_TRIANGLES ((c-lambda () int "___result = GL_TRIANGLES;")))
(define GL_TRIANGLE_STRIP ((c-lambda () int "___result = GL_TRIANGLE_STRIP;")))
(define GL_TRIANGLE_FAN ((c-lambda () int "___result = GL_TRIANGLE_FAN;")))
(define GL_LINES ((c-lambda () int "___result = GL_LINES;")))
(define GL_LINE_STRIP ((c-lambda () int "___result = GL_LINE_STRIP;")))
(define GL_LINE_LOOP ((c-lambda () int "___result = GL_LINE_LOOP;")))
(define GL_POINTS ((c-lambda () int "___result = GL_POINTS;")))
(define GL_UNSIGNED_BYTE ((c-lambda () int "___result = GL_UNSIGNED_BYTE;")))
(define GL_FLOAT ((c-lambda () int "___result = GL_FLOAT;")))
(define GL_PROJECTION ((c-lambda () int "___result = GL_PROJECTION;")))
(define GL_MODELVIEW ((c-lambda () int "___result = GL_MODELVIEW;")))
(define GL_COLOR_BUFFER_BIT ((c-lambda () int "___result = GL_COLOR_BUFFER_BIT;")))
(define GL_TEXTURE_2D ((c-lambda () int "___result = GL_TEXTURE_2D;")))
(define GL_TEXTURE_COORD_ARRAY ((c-lambda () int "___result = GL_TEXTURE_COORD_ARRAY;")))
(define GL_SHORT ((c-lambda () int "___result = GL_SHORT;")))
(define GL_TEXTURE_MIN_FILTER ((c-lambda () int "___result = GL_TEXTURE_MIN_FILTER;")))
(define GL_TEXTURE_MAG_FILTER ((c-lambda () int "___result = GL_TEXTURE_MAG_FILTER;")))
(define GL_LINEAR ((c-lambda () int "___result = GL_LINEAR;")))
(define GL_NEAREST ((c-lambda () int "___result = GL_NEAREST;")))
(define GL_LINEAR_MIPMAP_LINEAR ((c-lambda () int "___result = GL_LINEAR_MIPMAP_LINEAR;")))
;; 20100729: not defined on mingw32 cross compiler
;;(define GL_GENERATE_MIPMAP ((c-lambda () int "___result = GL_GENERATE_MIPMAP;")))
(define GL_RGB ((c-lambda () int "___result = GL_RGB;")))
(define GL_RGBA ((c-lambda () int "___result = GL_RGBA;")))
(define GL_ALPHA ((c-lambda () int "___result = GL_ALPHA;")))
(define GL_SCISSOR_TEST ((c-lambda () int "___result = GL_SCISSOR_TEST;")))
(define GL_TEXTURE_WRAP_S ((c-lambda () int "___result = GL_TEXTURE_WRAP_S;")))
(define GL_TEXTURE_WRAP_T ((c-lambda () int "___result = GL_TEXTURE_WRAP_T;")))
(define GL_REPEAT ((c-lambda () int "___result = GL_REPEAT;")))
;;  GL_CLAMP/GL_CLAMP_TO_EDGE confusion
(define GL_CLAMP ((c-lambda () int "#ifdef WIN32
___result = GL_CLAMP;
#else
___result = GL_CLAMP_TO_EDGE;
#endif")))
(define GL_INVALID_VALUE ((c-lambda () int "___result = GL_INVALID_VALUE;")))
;;(define GL_ ((c-lambda () int "___result = GL_;")))

;; functions

;; glOrtho is different in OpenGL and GLES. Why, thanks dudes!
(define (glOrtho arg1 arg2 arg3 arg4 arg5 arg6) 
  ((c-lambda (float float float float float float) void 
   "#if defined(IPHONE)||defined(IPHONESIM)||defined(ANDROID)||defined(SYMBIAN)
     glOrthof(___arg1,___arg2,___arg3,___arg4,___arg5,___arg6);
   #else
     glOrtho(___arg1,___arg2,___arg3,___arg4,___arg5,___arg6);
   #endif") (flo arg1) (flo arg2) (flo arg3) (flo arg4) (flo arg5) (flo arg6)))

(define glLoadIdentity (c-lambda () void "glLoadIdentity"))

(define glClear (c-lambda (int) void "glClear"))

(define glClearColor (c-lambda (float float float float) void "glClearColor"))

(define (glColor4f a b c d) 
  ((c-lambda (float float float float) void "glColor4f")
    (flo a) (flo b) (flo c) (flo d)))

(define glMatrixMode (c-lambda (int) void "glMatrixMode"))

(define glPushMatrix (c-lambda () void "glPushMatrix"))

(define glPopMatrix (c-lambda () void "glPopMatrix"))

(define (glTranslatef a b c) 
  ((c-lambda (float float float) void "glTranslatef") 
     (flo a) (flo b) (flo c)))

(define (glScalef a b c) 
  ((c-lambda (float float float) void "glScalef") 
     (flo a) (flo b) (flo c)))

(define (glRotatef a b c d) 
  ((c-lambda (float float float float) void "glRotatef")
     (flo a) (flo b) (flo c) (flo d)))

(define glEnable (c-lambda (int) void "glEnable"))

(define glDisable (c-lambda (int) void "glDisable"))

(define glBlendFunc (c-lambda (int int) void "glBlendFunc"))

(define glEnableClientState (c-lambda (int) void "glEnableClientState"))

(define glDisableClientState (c-lambda (int) void "glDisableClientState"))

(define glDrawArrays (c-lambda (int int int) void "glDrawArrays"))

(define (glVertexPointer arg1 arg2 arg3 arg4)
  ((c-lambda (int int int scheme-object) void
      "glVertexPointer(___arg1,___arg2,___arg3,
          ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)));")
         arg1 arg2 arg3 arg4))

(define (glColorPointer arg1 arg2 arg3 arg4)
  ((c-lambda (int int int scheme-object) void
      "glColorPointer(___arg1,___arg2,___arg3,
          ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)));")
         arg1 arg2 arg3 arg4))

(define (glTexCoordPointer arg1 arg2 arg3 arg4)
  ((c-lambda (int int int scheme-object) void
      "glTexCoordPointer(___arg1,___arg2,___arg3,
          ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)));")
         arg1 arg2 arg3 arg4))

(define (glGenTextures arg1 arg2)
  ((c-lambda (int scheme-object) void
    "glGenTextures(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)));")
         arg1 arg2))

;; this was (int int), which caused cfun conversion errors on android
(define glBindTexture (c-lambda (unsigned-int unsigned-int) void "glBindTexture"))

(define (glDeleteTextures arg1 arg2)
  ((c-lambda (unsigned-int scheme-object) void
    "glDeleteTextures(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)));")
         arg1 arg2))

(define glTexParameteri (c-lambda (int int int) void "glTexParameteri"))

(define (glTexImage2D arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)
  ((c-lambda (int int int int int int int int scheme-object) void
    "glTexImage2D(___arg1,___arg2,___arg3,___arg4,___arg5,___arg6,___arg7,___arg8,
        ___CAST(void*,___BODY_AS(___arg9,___tSUBTYPED)));")
         arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9))

;; 20100730: added for updating existing textures
(define (glTexSubImage2D arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9)
  ((c-lambda (int int int int int int int int scheme-object) void
    "glTexSubImage2D(___arg1,___arg2,___arg3,___arg4,___arg5,___arg6,___arg7,___arg8,
        ___CAST(void*,___BODY_AS(___arg9,___tSUBTYPED)));")
         arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9))

;; eof
