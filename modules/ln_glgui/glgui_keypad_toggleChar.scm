#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2016, University of British Columbia
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

;; Automatically generated. Do not edit.
;; png2scm ver 3.0. type=3
(c-declare  #<<end-of-c-declare
#include <string.h>
static unsigned char glgui_keypad_toggleChar_texture[]={
// length=294 [1024]
1, 1,
0, 0, 224, 182, 1, 2, 215, 255, 44, 32, 193, 4, 43, 255, 214, 0, 255, 32, 0, 3, 250, 236, 165, 16, 32, 14, 5, 24, 160, 236, 243, 198,
74, 32, 8, 4, 0, 0, 134, 255, 118, 32, 5, 2, 123, 255, 133, 96, 31, 32, 0, 4, 164, 0, 0, 17, 222, 32, 7, 2, 255, 252, 72, 32,
24, 1, 0, 53, 32, 9, 64, 0, 0, 52, 32, 31, 15, 0, 0, 2, 72, 255, 243, 0, 0, 138, 255, 199, 35, 22, 187, 255, 202, 32, 30, 2,
0, 0, 227, 64, 29, 2, 255, 226, 0, 96, 31, 13, 8, 85, 255, 228, 0, 0, 216, 255, 58, 0, 0, 40, 191, 63, 32, 29, 8, 0, 0, 147,
255, 99, 0, 92, 255, 147, 64, 31, 64, 0, 6, 253, 137, 0, 0, 248, 255, 13, 32, 24, 160, 0, 6, 67, 255, 180, 0, 175, 255, 66, 192, 31,
6, 198, 25, 0, 0, 245, 255, 11, 160, 28, 32, 0, 6, 3, 237, 247, 24, 246, 236, 3, 64, 31, 13, 0, 0, 4, 80, 255, 191, 0, 0, 210,
255, 57, 0, 0, 37, 160, 95, 5, 0, 161, 255, 173, 255, 160, 32, 7, 64, 31, 13, 2, 64, 255, 245, 0, 0, 130, 255, 202, 39, 20, 183, 255,
209, 32, 20, 32, 0, 0, 80, 32, 118, 0, 79, 32, 7, 32, 6, 64, 0, 4, 174, 0, 0, 12, 215, 64, 8, 1, 255, 82, 32, 20, 32, 0,
4, 9, 245, 255, 245, 9, 32, 7, 32, 17, 4, 255, 251, 235, 159, 14, 32, 10, 5, 20, 152, 231, 248, 207, 83, 32, 8, 224, 253, 0, 224, 239,
0, 2, 0, 0, 0};
end-of-c-declare
)
(define glgui_keypad_toggleChar.z  (let ((u8v (make-u8vector 295))) ((c-lambda (scheme-object int) void "memcpy(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),glgui_keypad_toggleChar_texture,___arg2);") u8v 295) u8v))
(define glgui_keypad_toggleChar.raw (glCoreTextureCreate 32 32 (u8vector-decompress glgui_keypad_toggleChar.z)))(define glgui_keypad_toggleChar.img (list 31 21 glgui_keypad_toggleChar.raw 0. 0. 0.968750 0.656250))
