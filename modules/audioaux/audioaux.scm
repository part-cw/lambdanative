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

;; audio headphone detection and volume control (iOS and Android)

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef IOS
  void iphone_setvolume(double);
  double iphone_getvolume();
  int iphone_headphonepresent();
#endif

#ifdef ANDROID
  void SoundPoolInit();
  int SoundPoolSetVolume(float vol);
  float SoundPoolGetVolume();
  int SoundPoolHeadphonePresent(void);
#endif

static int audioaux_headphonepresent()
{
  int res=0;
#ifdef IOS
  res=iphone_headphonepresent();
#endif
#ifdef ANDROID
  SoundPoolInit();
  res = SoundPoolHeadphonePresent();
#endif
  return res;
}

static void audioaux_setvolume(double v)
{
#ifdef IOS
  iphone_setvolume(v);
#endif
#ifdef ANDROID
  SoundPoolInit();
  SoundPoolSetVolume((float)v);
#endif
}

static double audioaux_getvolume()
{
  double res=0;
#ifdef IOS
  res=iphone_getvolume();
#endif
#ifdef ANDROID
  SoundPoolInit();
  res = (double) SoundPoolGetVolume();
#endif
  return res;
}

end-of-c-declare
)

(define (audioaux-headphonepresent) (fx= ((c-lambda () int "audioaux_headphonepresent")) 1))

(define audioaux-setvolume (c-lambda (double) void "audioaux_setvolume"))
(define audioaux-getvolume (c-lambda () double "audioaux_getvolume"))

;; eof
