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

#ifdef PLAYBOOK
#define USE_BB10_PCM
#endif

// %%%%%%%%%%%%%%%%%%%%%%
#ifdef USE_IOS_REALTIME

#define SHORT2FLOAT(x) ((float)(x)/32767.)

#define AF_SRATE 44100
#define AF_FRAMESIZE 1024

// prototypes
int iphone_realtime_audio_init(double,unsigned int,int);
int iphone_realtime_audio_start(void (*)(float*,unsigned int, void*));
int iphone_realtime_audio_stop();
extern void iphone_setvolume(double);
extern double iphone_getvolume();

static void iphone_realtime_callback( float *buffer, unsigned int framesize, void* userData)
{
  int16 l,r;
  static double t=0;
  unsigned int i;
  unsigned int j;
  for (i=0; i<framesize; i++) {
    buffer[2*i] = 0.0f;
    buffer[2*i+1] = 0.0f;
    for(j=0; j<audiofile_count; j++){
      if (audiofiles[j].playing == PLAYING || audiofiles[j].playing == LOOPING) {
        audiofile_nextsample_from(&audiofiles[j], &l,&r);
        buffer[2*i]+=(SHORT2FLOAT(l));
        buffer[2*i+1]+=(SHORT2FLOAT(r));
      }
    }
    t+=1./AF_SRATE;
  }
}

int iphone_realtime_init(int activateInput) {
  int res = iphone_realtime_audio_init(AF_SRATE,AF_FRAMESIZE,activateInput);
  if (res) res = iphone_realtime_audio_start(iphone_realtime_callback);
//  iphone_setvolume(1.0);
  return res;
}

static void iphone_realtime_play(int id)
{
  audiofile_select(id);
}

static void iphone_realtime_loop(int id)
{
  audiofile_select(id);
  audiofiles[id-1].playing = LOOPING;
}

static void iphone_realtime_stop(int id){
  audiofiles[id-1].playing = NOT_PLAYING;
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
  PlaySound(fname, NULL, SND_FILENAME | SND_ASYNC); // This | just fixes highlighting
}

#endif

// %%%%%%%%%%%%%%%%%%%%%%

#ifdef USE_PORTAUDIO

#include <portaudio.h>

extern int portaudio_needsinit;
extern int portaudio_odev;

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
  int j;
  for (i=0;i<framesPerBuffer;i++) {
    out[2*i]=0;
    out[2*i+1]=0;
    for(j=0;j<audiofile_count;j++){
      if (audiofiles[j].playing == PLAYING || audiofiles[j].playing == LOOPING) {
        audiofile_nextsample_from(&audiofiles[j],&l,&r);
        out[2*i]=out[2*i]+l;
        out[2*i+1]=out[2*i+1]+r;
      }
    }
  }
  return 0;
}


static int portaudio_init(void)
{
  PaError err;
  PaStreamParameters inputParameters, outputParameters;
  if (portaudio_needsinit) {
    err = Pa_Initialize();
    if( err != paNoError ) goto error;
    portaudio_needsinit=0;
  }
  outputParameters.device = (portaudio_odev<0?OUTPUT_DEVICE:portaudio_odev);
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

static void portaudio_loop(int id)
{
  audiofile_select(id);
  audiofiles[id-1].playing = LOOPING;
}

static void portaudio_stopfile(int id)
{
  audiofiles[id-1].playing = NOT_PLAYING;
}

static void portaudio_stop(void){
  int id;
  for (id=0;id<audiofile_count;id++){
    audiofiles[id].playing = NOT_PLAYING;
  }
}

#endif

// %%%%%%%%%%%%%%%%%%%%%%

#ifdef USE_ANDROID_NATIVE
int android_audio_loadfile(int, const char*, int);
int android_audio_playfile(int, float, float, int, int, float);
int android_audio_stopfile(int);
int android_audio_stop();
int android_audio_setvolume(float vol);
#else
int android_audio_setvolume(float vol){ return 0;}
#endif

// %%%%%%%%%%%%%%%%%%%%%%

#ifdef USE_APPLE_NATIVE
#include <AudioToolbox/AudioToolbox.h>
#endif

// %%%%%%%%%%%%%%%%%%%%%%

void audiofile_init(void) {
#ifdef USE_PORTAUDIO
  portaudio_init();
#endif
#ifdef USE_IOS_REALTIME
  iphone_realtime_init(0);
#endif
}

int audiofile_load(int id, char *name)
{
  #ifdef USE_ANDROID_NATIVE
  return android_audio_loadfile(id, name, 0);
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
#ifdef USE_PORTAUDIO
  portaudio_stop();
#endif
#ifdef USE_IOS_REALTIME
  iphone_realtime_audio_stop();
#endif
#ifdef USE_ANDROID_NATIVE
  android_audio_stop();
#endif
}

void audiofile_stop_specific(int id)
{
#ifdef USE_PORTAUDIO
  portaudio_stopfile(id);
#endif
#ifdef USE_IOS_REALTIME
  iphone_realtime_stop(id);
#endif
#ifdef USE_ANDROID_NATIVE
  android_audio_stopfile(id);
#endif
}

void audiofile_play(int id)
{
#ifdef USE_ANDROID_NATIVE
 android_audio_playfile(id,1.0,1.0,0,0,1.);
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

void audiofile_loop(int id)
{
#ifdef USE_ANDROID_NATIVE
 android_audio_playfile(id,1.0,1.0,0.0,-1,1.0);
#endif
#ifdef USE_IOS_REALTIME
 iphone_realtime_loop(id);
#endif
#ifdef USE_APPLE_NATIVE
  SystemSoundID sid=(SystemSoundID)id;
  AudioServicesPlaySystemSound(sid);
#endif
#ifdef USE_PORTAUDIO
  portaudio_loop(id);
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

end-of-c-declare
)

(define audiofile-init (c-lambda () void "audiofile_init"))

(define audiofile:load
  (cond-expand
   (android
    (lambda (fn)
      (let* ((result (eventloop-open-channel))
             (reply-id (mutex-specific result)))
        ((c-lambda (int char-string) int "audiofile_load") reply-id fn)
        (eventloop-await-channel result))))
   (else
    (lambda (fn)
      ((c-lambda (int char-string) int "audiofile_load") 0 fn)))))

(define (audiofile-load name)
  (define (autoext name)
    (string-append name (if (file-exists? (string-append name ".wav")) ".wav" ".ogg")))
  (let* ((file  (cond
           ((member (system-platform) '("ios" "bb10" "playbook"))
             (autoext (string-append (system-appdirectory) (system-pathseparator) name)))
           ((member (system-platform) '("win32" "linux" "openbsd" "freebsd" "netbsd"))
             (autoext (string-append (system-appdirectory) (system-pathseparator) "sounds" (system-pathseparator) name)))
           ((member (system-platform) '("macosx"))
             (autoext (string-append (system-appdirectory) (system-pathseparator) "sounds" (system-pathseparator) name)))
           ((string=? (system-platform) "android")
	           (string-downcase name))
           (else #f)))
         (filesane (or (string=? (system-platform) "android") (file-exists? file)))
         (loadres (if filesane (audiofile:load file) 0)))
    (if (> loadres 0)
      loadres
      (begin
        (log-system "audiofile: file " file " not found")
        #f
       )
     )
  ))

(define (audiofile-play id)
  (if (and id (> id 0))
    ((c-lambda (int) void "audiofile_play") id)
  ))

(define (audiofile-loop id)
  (if (and id (> id 0))
    ((c-lambda (int) void "audiofile_loop") id)
  ))

(define (audiofile-forceplay id)
  (if (and id (> id 0))
    ((c-lambda (int) void "audiofile_forceplay") id)
  ))

(define audiofile-start (c-lambda () int "audiofile_start"))
(define audiofile-stop-all (c-lambda () void "audiofile_stop"))
(define audiofile-stop-one (c-lambda (int) void "audiofile_stop_specific"))
(define (audiofile-stop #!optional (id #f))
  (if (eq? id #f)
      (audiofile-stop-all)
      (audiofile-stop-one id)))

;; Compabibility
(define (audiofile-getvolume) (audioaux-getvolume))
(define (audiofile-setvolume vol)
  (audioaux-setvolume vol)
  (if (string=? (system-platform) "android")
    ((c-lambda (double) void "android_audio_setvolume") vol)
  )
)

;; unit tests
;; -----------------

(unit-test "audiofile" "1000 sine waves played at random intervals"
  (lambda () (audiofile-init)
    (let ((af (audiofile-load "test")))
      (let loop ((n 1000))
        (if (fx= n 0)
          #t
          (if (not (audiofile-play af))
            #f
            (begin (thread-sleep! (random-real)) (loop (fx- n 1)))
          )
      ))
    )
  ))

;;eof
