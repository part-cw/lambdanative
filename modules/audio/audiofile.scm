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

;; audio driver based on files

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// %%%%%%%%%%%%%%%%%%%%%%
// what's your poison?

#ifdef MACOSX
//#define USE_APPLE_NATIVE
#define USE_PORTAUDIO
#endif

#if defined(IOS)
//#define USE_APPLE_NATIVE
#define USE_IOS_REALTIME
#endif

#ifdef ANDROID
#define USE_ANDROID_NATIVE
#endif

#ifdef WIN32
//#define USE_WIN32_NATIVE
#define USE_PORTAUDIO
#endif

#ifdef LINUX
#define USE_PORTAUDIO
#endif

#ifdef OPENBSD
#define USE_PORTAUDIO
#endif

#ifdef NETBSD
#define USE_PORTAUDIO
#endif

#ifdef BB10
#define USE_BB10_PCM
#endif

// %%%%%%%%%%%%%%%%%%%%%%
#ifdef USE_IOS_REALTIME

#define SHORT2FLOAT(x) ((float)(x)/32767.)

#define AF_SRATE 44100
#define AF_FRAMESIZE 1024

// prototypes
int iphone_realtime_audio_init(double,unsigned int);
int iphone_realtime_audio_start(void (*)(float*,unsigned int, void*));
int iphone_realtime_audio_stop();
void iphone_setvolume(double);
double iphone_getvolume();

static void iphone_realtime_callback( float *buffer, unsigned int framesize, void* userData)
{
  int16 l,r;
  static double t=0;
  unsigned int i;
  for (i=0; i<framesize; i++) {
    if (cur) { 
      audiofile_nextsample(&l,&r); 
      buffer[2*i]=SHORT2FLOAT(l);
      buffer[2*i+1]=SHORT2FLOAT(r);
    } else {
      buffer[2*i] = buffer[2*i+1] = 0;
    } 
    t+=1./AF_SRATE;
  }
}

int iphone_realtime_init() {
  int res = iphone_realtime_audio_init(AF_SRATE,AF_FRAMESIZE);
  if (res) res = iphone_realtime_audio_start(iphone_realtime_callback);
//  iphone_setvolume(1.0);
  return res;
}

static void iphone_realtime_play(int id)
{
  audiofile_select(id);
}

#endif

// %%%%%%%%%%%%%%%%%%%%%%

#ifdef USE_WIN32_NATIVE

#include <windows.h>

static int soundfile_load(char *fname)
{
  return (int)strdup(fname);
}

static int soundfile_play(int id)
{
  char *fname=(char*)id;
  PlaySound(fname, NULL, SND_FILENAME | SND_ASYNC); 
}

#endif

// %%%%%%%%%%%%%%%%%%%%%%

#ifdef USE_PORTAUDIO

#include <portaudio.h>

#define AF_FRAMES_PER_CALLBACK         1024
#define OUTPUT_DEVICE  (Pa_GetDefaultOutputDevice())
#define SAMPLE_FORMAT  paInt16

static PaStream *stream;

static int af_portaudio_cb( const void *inputBuffer, void *outputBuffer,
   unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
   PaStreamCallbackFlags statusFlags, void *userData )
{
  int16 l,r;
  int i;
//  short *out=(short*)outputBuffer;
  int16 *out=(int16 *)outputBuffer;
  for (i=0;i<framesPerBuffer;i++) { 
    if (cur) {
      audiofile_nextsample(&l,&r);
      out[2*i]=l; out[2*i+1]=r;
    } else {
      out[2*i]=0; out[2*i+1]=0;
    }
  }
  return 0;
}

static int portaudio_init(void)
{
  PaStreamParameters inputParameters, outputParameters;
  PaError err = Pa_Initialize();
  if( err != paNoError ) goto error;
  outputParameters.device = OUTPUT_DEVICE; 
  if (outputParameters.device == paNoDevice) { goto error; }
  outputParameters.channelCount = 2;
  outputParameters.sampleFormat = SAMPLE_FORMAT;
  outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;
  outputParameters.hostApiSpecificStreamInfo = NULL;
  err = Pa_OpenStream( &stream, 0, &outputParameters, 44100, AF_FRAMES_PER_CALLBACK, paClipOff, af_portaudio_cb, 0);
  if( err != paNoError ) goto error;
  err = Pa_StartStream( stream );
  if( err != paNoError ) goto error;
  return 0;
error:
  Pa_Terminate();
  return -1;
}

static void portaudio_play(int id)
{
  audiofile_select(id);
}

#endif 

// %%%%%%%%%%%%%%%%%%%%%%

#ifdef USE_ANDROID_NATIVE
void SoundPoolInit(void);
int SoundPoolLoadSFX(const char*, int);
int SoundPoolLoadSFXAsset(const char*, int);
int SoundPoolPlaySound(int, float, float, int, int, float);
float SoundPoolGetVolume();
#endif

// %%%%%%%%%%%%%%%%%%%%%%

#ifdef USE_APPLE_NATIVE
#include <AudioToolbox/AudioToolbox.h>
#endif 

// %%%%%%%%%%%%%%%%%%%%%%

void audiofile_init(void) { 
#ifdef USE_ANDROID_NATIVE
  SoundPoolInit();
#endif
#ifdef USE_PORTAUDIO
  portaudio_init();
#endif
#ifdef USE_IOS_REALTIME
  iphone_realtime_init();
#endif
}

int audiofile_load(char *name)
{
  #ifdef USE_ANDROID_NATIVE
  return SoundPoolLoadSFX(name, 0);
  #endif
  #ifdef USE_APPLE_NATIVE
  SystemSoundID sid;
  CFURLRef soundFileURLRefClick  = CFBundleCopyResourceURL (CFBundleGetMainBundle (), 
     CFStringCreateWithCString(NULL,name,0), CFSTR ("wav"), NULL);
  AudioServicesCreateSystemSoundID (soundFileURLRefClick, &sid);
  return (int)sid;
  #endif
  #if defined(USE_PORTAUDIO)|| defined(USE_IOS_REALTIME) || defined(USE_BB10_PCM)
  return audiofile_loadany(name);
  #endif
}

int audiofile_start()
{
#ifdef USE_IOS_REALTIME
  return iphone_realtime_audio_start(iphone_realtime_callback);
#endif
}

void audiofile_stop()
{
#ifdef USE_IOS_REALTIME
  iphone_realtime_audio_stop();
#endif
}

void audiofile_play(int id)
{
#ifdef USE_ANDROID_NATIVE
 SoundPoolPlaySound(id,1.0,1.0,0,0,1.);
#endif
#ifdef USE_IOS_REALTIME
 iphone_realtime_play(id);
#endif
#ifdef USE_APPLE_NATIVE
  SystemSoundID sid=(SystemSoundID)id;
  AudioServicesPlaySystemSound(sid);
#endif
#ifdef USE_PORTAUDIO
  portaudio_play(id);  
#endif
#ifdef USE_WIN32_NATIVE
  soundfile_play(id);
#endif
#ifdef USE_BB10_PCM
  audiofile_select(id);
#endif
}

// don't ever allow them to silence us for critical alerts
void audiofile_forceplay(int id)
{
#ifdef USE_IOS_REALTIME
  if (iphone_getvolume()==0.) iphone_setvolume(0.1);
#endif
  audiofile_play(id);
}

// Returns a float between 0 and 1 on Android and iOS, returns -1 on other platforms - not yet supported
float audiofile_getvolume()
{
#ifdef USE_IOS_REALTIME
  return iphone_getvolume();
#elif ANDROID
  return SoundPoolGetVolume();
#endif
return -1;
}

end-of-c-declare
)

(define audiofile-init (c-lambda () void "audiofile_init"))

(define audiofile:load (c-lambda (char-string) int "audiofile_load"))

(define (audiofile-load name)
  (define (autoext name)
    (string-append name (if (file-exists? (string-append name ".wav")) ".wav" ".ogg")))
  (let* ((file  (cond
           ((member (system-platform) '("ios" "bb10"))
              (autoext (string-append (system-appdirectory) (system-pathseparator) name)))
           ((member (system-platform) '("win32" "linux" "openbsd"))
              (autoext (string-append (system-appdirectory) (system-pathseparator) "sounds" (system-pathseparator) name)))
           ((member (system-platform) '("macosx"))
              (autoext (string-append (system-appdirectory) (system-pathseparator) "sounds" (system-pathseparator) name)))
           ((string=? (system-platform) "android") (string-downcase name))
           (else #f)))
          (filesane (or (string=? (system-platform) "android") (file-exists? file)))
          (loadres (if filesane (audiofile:load file) 0)))
    (if (> loadres 0) loadres (begin
      (log-system "audiofile: file " file " not found") #f))))
 
(define (audiofile-play id) (if (and id (> id 0)) ((c-lambda (int) void "audiofile_play") id)))
(define (audiofile-forceplay id) (if (and id (> id 0)) ((c-lambda (int) void "audiofile_forceplay") id)))

(define audiofile-start (c-lambda () int "audiofile_start"))
(define audiofile-stop (c-lambda () void "audiofile_stop"))

(define audiofile-getvolume (c-lambda () float "audiofile_getvolume"))

;;eof
