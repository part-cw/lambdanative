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
;; real-time audio support

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

#define RT_SRATE 8000

#ifdef MACOSX
#define USE_PORTAUDIO
#endif

#ifdef IPHONE
#define USE_IPHONEAUDIO
#endif

#ifdef ANDROID
#define USE_ANDROIDAUDIO
#endif

#ifdef WIN32
#define USE_PORTAUDIO
#endif

#ifdef LINUX
#define USE_PORTAUDIO
#endif

void (*rtaudio_initcb)(int)=0;
void (*rtaudio_inputcb)(float)=0;
void (*rtaudio_outputcb)(float*,float*)=0;
void (*rtaudio_closecb)(void)=0;

void rtaudio_register(void (*initcb)(int), void (*inputcb)(float), void (*outputcb)(float*,float*), void (*closecb)(void))
{
  rtaudio_initcb = initcb;
  rtaudio_inputcb = inputcb;
  rtaudio_outputcb = outputcb;
  rtaudio_closecb = closecb;
}

// %%%%%%%%%%%%%%%%%%%%%%%%
#ifdef USE_IPHONEAUDIO

#define RT_FRAMESIZE 128

int iphone_realtime_audio_init(double,unsigned int);
int iphone_realtime_audio_start(void (*)(float*,unsigned int, void*));
int iphone_realtime_audio_stop();
void iphone_setvolume(double);

static void rtaudio_callback( float *buffer, unsigned int framesize, void* userData)
{
  unsigned int i;
  if (1) {
    for (i=0; i<framesize; i++) {
      if (rtaudio_inputcb) rtaudio_inputcb(buffer[2*i]);
      if (rtaudio_outputcb) rtaudio_outputcb(&buffer[2*i], &buffer[2*i+1]); 
    }
  } else {
    for (i=0; i<framesize; i++) {
      buffer[2*i] = buffer[2*i+1] = 0;
    }
  }
}

static void rtaudio_start(int s2, double volume) 
{
   static int needsinit=1;
  if (needsinit) {
    if (rtaudio_initcb) rtaudio_initcb(s2);
    iphone_realtime_audio_init(RT_SRATE,RT_FRAMESIZE);
    iphone_setvolume(volume);
    needsinit=0;
  }
  iphone_realtime_audio_start(rtaudio_callback);
}

static void rtaudio_stop(void) {
  if (rtaudio_closecb) rtaudio_closecb();
  iphone_realtime_audio_stop();
}

#endif // USE_IPHONEAUDIO

// %%%%%%%%%%%%%%%%%%%%%%%%
#ifdef USE_PORTAUDIO

#include <portaudio.h>

//#define FRAMES_PER_CALLBACK         1024
#define RT_FRAMES_PER_CALLBACK         64
#define INPUT_DEVICE   (Pa_GetDefaultInputDevice())
#define OUTPUT_DEVICE  (Pa_GetDefaultOutputDevice())
#define SAMPLE_FORMAT  paInt16
//typedef short SAMPLE;

#define FLO(x) ((double)x/32767.)
#define FIX(x) ((short)(32767.*x))

static PaStream *stream;

static int rt_portaudio_cb( const void *inputBuffer, void *outputBuffer,
   unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
   PaStreamCallbackFlags statusFlags, void *userData )
{
  int i;
  short *ibuf = (short*)inputBuffer;
  short *obuf=(short*)outputBuffer;
  if (!ibuf||!obuf) return 0;
  for (i=0;i<framesPerBuffer;i++) {
     float ival = FLO(ibuf[2*i]), oval1,oval2;
     if (rtaudio_inputcb) rtaudio_inputcb(ival);
     if (rtaudio_outputcb) rtaudio_outputcb(&oval1,&oval2);
     obuf[2*i]=FIX(oval1); obuf[2*i+1]=FIX(oval2);
  }
  return 0;
}

void rtaudio_start(int s2, double volume)  
{
  PaError err;
  static int needsinit=1;
  // XXX set volume here
  if (needsinit) { 
    if (rtaudio_initcb) rtaudio_initcb(s2);
    PaStreamParameters inputParameters, outputParameters;
    err = Pa_Initialize();
    if( err != paNoError ) goto error;
    inputParameters.device = INPUT_DEVICE;              /* default input device */
    if (inputParameters.device == paNoDevice) { goto error; }
    inputParameters.channelCount = 2;
    inputParameters.sampleFormat = SAMPLE_FORMAT;
    inputParameters.suggestedLatency = Pa_GetDeviceInfo( inputParameters.device )->defaultLowInputLatency;
    inputParameters.hostApiSpecificStreamInfo = NULL;
    outputParameters.device = OUTPUT_DEVICE; 
    if (outputParameters.device == paNoDevice) { goto error; }
    outputParameters.channelCount = 2;
    outputParameters.sampleFormat = SAMPLE_FORMAT;
    outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;
    outputParameters.hostApiSpecificStreamInfo = NULL;
    err = Pa_OpenStream( &stream, &inputParameters, &outputParameters, RT_SRATE, RT_FRAMES_PER_CALLBACK, paClipOff, rt_portaudio_cb, 0);
    if( err != paNoError ) goto error;
    needsinit=0;
  }
  err = Pa_StartStream( stream );
  if( err != paNoError ) goto error;
  return;
error:
  Pa_Terminate();
  return;
}

void rtaudio_stop(void) 
{ 
  Pa_StopStream( stream );
}

#endif // USE_PORTAUDIO

// %%%%%%%%%%%%%%%%%%%%%%%%
#ifdef USE_ANDROIDAUDIO

#ifdef RTAUDIO
extern int opensl_on;
#endif

void SoundPoolInit(void);
int SoundPoolSetVolume(float);

void rtaudio_start(int s2, double volume)  
{ 
  SoundPoolInit();
  SoundPoolSetVolume(volume);
  if (rtaudio_initcb) rtaudio_initcb(s2);
}

void rtaudio_stop(void) 
{ 
#ifdef RTAUDIO
  opensl_on = 0;
#endif
}

#endif // USE_ANDROIDAUDIO


end-of-c-declare
)

;; ffi

(define rtaudio-start (c-lambda (int double) void "rtaudio_start"))
(define rtaudio-stop (c-lambda () void "rtaudio_stop"))

;; eof
