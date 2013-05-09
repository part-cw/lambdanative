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
(define glgui_keypad_shift.raw (glCoreTextureCreate 32 32 '#u8(
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 255 255 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 255 255 255 0 255 255 255 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 255 255 255 0 0 0 255 255 255 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 255 255 255 0 0 0 0 0 255 255 255 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 255 255 255 0 0 0 0 0 0 0 255 255 255 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 255 0 0 0 0 0 0 0 0 0 255 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 255 255 255 0 0 0 5 251 255 251 5 0 0 0 255 255 255 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 255 255 255 0 0 0 0 113 255 255 255 112 0 0 0 0 255 255 255 0 0 0 0 0 0 
0 0 0 0 0 0 
255 255 255 255 255 0 0 0 189 255 183 255 188 0 0 0 255 255 255 255 255 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 0 0 0 249 249 50 248 249 0 0 0 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 0 0 108 255 189 0 186 255 107 0 0 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 0 0 184 255 114 0 113 255 183 0 0 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 0 0 247 252 17 0 17 252 247 0 0 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 0 104 255 255 255 255 255 255 255 103 0 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 0 180 255 255 255 255 255 255 255 179 0 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 0 245 251 30 0 0 0 36 251 245 0 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 99 255 185 0 0 0 0 0 187 255 98 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 175 255 98 0 0 0 0 0 100 255 174 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 175 236 0 0 0 0 0 0 0 238 175 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 0 0 0 0 0 0 0 0 0 0 0 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 255 255 0 0 0 0 0 0 0 0 0 0 0 255 255 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 
0 0 0 0 0 0 
0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
)))
(define glgui_keypad_shift.img (list 21 27 glgui_keypad_shift.raw 0. 1. .65625000000000000000 .15625000000000000000))
