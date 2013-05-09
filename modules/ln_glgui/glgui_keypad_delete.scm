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
(define glgui_keypad_delete.raw (glCoreTextureCreate 32 32 '#u8(
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
0 0 0 0 0 0 0 0 0 0 83 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 
255 255 255 0 0 0 
0 0 0 0 0 0 0 0 0 83 240 160 160 160 160 160 160 160 160 160 160 160 160 160 160 160 
160 160 255 0 0 0 
0 0 0 0 0 0 0 0 83 246 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 160 255 0 0 0 
0 0 0 0 0 0 0 83 253 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 160 255 0 0 0 
0 0 0 0 0 0 83 255 160 0 0 0 0 0 152 255 225 22 0 0 0 22 225 255 152 0 
0 160 255 0 0 0 
0 0 0 0 0 83 251 160 0 0 0 0 0 0 5 194 255 194 5 0 5 195 255 195 5 0 
0 160 255 0 0 0 
0 0 0 0 83 234 160 0 0 0 0 0 0 0 0 22 226 255 152 0 153 255 227 23 0 0 
0 160 255 0 0 0 
0 0 0 83 245 160 0 0 0 0 0 0 0 0 0 0 50 246 255 178 255 247 52 0 0 0 
0 160 255 0 0 0 
0 0 83 246 160 0 0 0 0 0 0 0 0 0 0 0 0 88 255 255 255 93 0 0 0 0 
0 160 255 0 0 0 
0 83 241 160 0 0 0 0 0 0 0 0 0 0 0 0 0 14 233 255 220 4 0 0 0 0 
0 160 255 0 0 0 
0 0 83 249 160 0 0 0 0 0 0 0 0 0 0 0 0 169 255 255 255 139 0 0 0 0 
0 160 255 0 0 0 
0 0 0 83 255 160 0 0 0 0 0 0 0 0 0 0 104 255 242 94 250 255 80 0 0 0 
0 160 255 0 0 0 
0 0 0 0 83 255 160 0 0 0 0 0 0 0 0 46 247 255 87 0 113 255 240 35 0 0 
0 160 255 0 0 0 
0 0 0 0 0 83 250 160 0 0 0 0 0 0 11 216 255 153 0 0 0 173 255 208 8 0 
0 160 255 0 0 0 
0 0 0 0 0 0 83 255 160 0 0 0 0 0 161 255 210 8 0 0 0 14 219 255 157 0 
0 160 255 0 0 0 
0 0 0 0 0 0 0 83 251 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 160 255 0 0 0 
0 0 0 0 0 0 0 0 83 248 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 160 255 0 0 0 
0 0 0 0 0 0 0 0 0 83 255 160 160 160 160 160 160 160 160 160 160 160 160 160 160 160 
160 160 255 0 0 0 
0 0 0 0 0 0 0 0 0 0 83 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 
255 255 255 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 
)))
(define glgui_keypad_delete.img (list 31 23 glgui_keypad_delete.raw 0. 1. .96875000000000000000 .28125000000000000000))
