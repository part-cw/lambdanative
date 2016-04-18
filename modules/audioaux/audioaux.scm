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

;; audio headphone detection and volume control (iOS, Android, OS X)

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

#ifdef MACOSX

#include <AudioToolbox/AudioToolbox.h>

// adapted from ISSoundAdditions, copyright notice:

//  ISSoundAdditions.m (ver 1.2 - 2012.10.27)
//
//	Created by Massimo Moiso (2012-09) InerziaSoft
//	based on an idea of Antonio Nunes, SintraWorks
//
// Permission is granted free of charge to use this code without restriction
// and without limitation, with the only condition that the copyright
// notice and this permission shall be included in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT 0T LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND 0NINFRINGEMENT. IN 0 EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#define THRESHOLD 0.005

static AudioDeviceID obtainDefaultOutputDevice()
{
  AudioDeviceID theAnswer = kAudioObjectUnknown;
  UInt32 theSize = sizeof(AudioDeviceID);
  AudioObjectPropertyAddress theAddress;
  theAddress.mSelector = kAudioHardwarePropertyDefaultOutputDevice;
  theAddress.mScope = kAudioObjectPropertyScopeGlobal;
  theAddress.mElement = kAudioObjectPropertyElementMaster;
  if (! AudioObjectHasProperty(kAudioObjectSystemObject, &theAddress) )  { return theAnswer; }
  OSStatus theError = AudioObjectGetPropertyData(kAudioObjectSystemObject, &theAddress, 0, NULL, &theSize, &theAnswer);
  return theAnswer;
}

static double macosx_getvolume()
{ 
  AudioDeviceID defaultDevID = kAudioObjectUnknown;
  UInt32 theSize = sizeof(Float32);
  OSStatus theError;
  Float32 theVolume = 0;
  AudioObjectPropertyAddress  theAddress;
  defaultDevID = obtainDefaultOutputDevice();
  if (defaultDevID == kAudioObjectUnknown) return 0.0;    //device not found: return 0
  theAddress.mSelector = kAudioHardwareServiceDeviceProperty_VirtualMasterVolume;
  theAddress.mScope = kAudioDevicePropertyScopeOutput;
  theAddress.mElement = kAudioObjectPropertyElementMaster;
  if (! AudioObjectHasProperty(defaultDevID, &theAddress) ) { return 0.0; }
  theError = AudioObjectGetPropertyData(defaultDevID, &theAddress, 0, NULL, &theSize, &theVolume);
  if ( theError != noErr )  { return 0.0; }
  theVolume = theVolume > 1.0 ? 1.0 : (theVolume < 0.0 ? 0.0 : theVolume);
  return (double)theVolume;
}

static void macosx_setvolume(double v)
{
  float theVolume = (float)v;
  float newValue = theVolume;
  AudioObjectPropertyAddress  theAddress;
  AudioDeviceID defaultDevID;
  OSStatus theError = noErr;
  UInt32 muted;
  Boolean canSetVol = 1, muteValue;
  Boolean hasMute = 1, canMute = 1;
  defaultDevID = obtainDefaultOutputDevice();
  if (defaultDevID == kAudioObjectUnknown) { return; }
  newValue = theVolume > 1.0 ? 1.0 : (theVolume < 0.0 ? 0.0 : theVolume);
  theAddress.mElement = kAudioObjectPropertyElementMaster;
  theAddress.mScope = kAudioDevicePropertyScopeOutput;
  if ( (muteValue = (newValue < THRESHOLD)) ) {
    theAddress.mSelector = kAudioDevicePropertyMute;
    hasMute = AudioObjectHasProperty(defaultDevID, &theAddress);
    if (hasMute) {
      theError = AudioObjectIsPropertySettable(defaultDevID, &theAddress, &canMute);
      if (theError != noErr || !canMute) { canMute = 0; }
    }
    else canMute = 0;
  } else {
    theAddress.mSelector = kAudioHardwareServiceDeviceProperty_VirtualMasterVolume;
  }
  if (! AudioObjectHasProperty(defaultDevID, &theAddress)) { return; }
  theError = AudioObjectIsPropertySettable(defaultDevID, &theAddress, &canSetVol);
  if ( theError!=noErr || !canSetVol ) { return; }
  if (muteValue && hasMute && canMute) {
    muted = 1;
    theError = AudioObjectSetPropertyData(defaultDevID, &theAddress, 0, NULL, sizeof(muted), &muted);
    if (theError != noErr) { return; }
  } else  {
    theError = AudioObjectSetPropertyData(defaultDevID, &theAddress, 0, NULL, sizeof(newValue), &newValue);
    if (hasMute && canMute) {
      theAddress.mSelector = kAudioDevicePropertyMute;
      muted = 0;
      theError = AudioObjectSetPropertyData(defaultDevID, &theAddress, 0, NULL, sizeof(muted), &muted);
    }
  }
}

static int macosx_headphonepresent()
{
  int res=0;
  AudioObjectPropertyAddress sourceAddr;
  AudioDeviceID defaultDevID = obtainDefaultOutputDevice();
  UInt32 dataSourceId = 0;
  UInt32 dataSourceIdSize = sizeof(UInt32);
  sourceAddr.mSelector = kAudioDevicePropertyDataSource;
  sourceAddr.mScope = kAudioDevicePropertyScopeOutput;
  sourceAddr.mElement = kAudioObjectPropertyElementMaster;
  AudioObjectGetPropertyData(defaultDevID, &sourceAddr, 0, NULL, &dataSourceIdSize, &dataSourceId);
  if (dataSourceId == 'hdpn') res=1;
  return res;
}

#endif // MACOSX

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
#ifdef MACOSX
  res = macosx_headphonepresent();
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
#ifdef MACOSX
  macosx_setvolume((float)v);
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
#ifdef MACOSX
  res = (double) macosx_getvolume();
#endif
  return res;
}

end-of-c-declare
)

(define (audioaux-headphonepresent) (fx= ((c-lambda () int "audioaux_headphonepresent")) 1))

(define audioaux-setvolume (c-lambda (double) void "audioaux_setvolume"))
(define audioaux-getvolume (c-lambda () double "audioaux_getvolume"))

;; eof
