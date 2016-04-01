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

#ifdef MACOSX
#define USE_PORTAUDIO
#endif

#ifdef IOS
#define USE_IOSAUDIO
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

#ifdef OPENBSD
#define USE_PORTAUDIO
#endif

#ifdef NETBSD
#define USE_PORTAUDIO
#endif

#ifdef FREEBSD
#define USE_PORTAUDIO
#endif

#ifdef BB10
#define USE_BB10AUDIO
#endif 

static int rtaudio_srate=0;

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

int rtaudio_pa_map_output=0;
int rtaudio_pa_chmap_out[16];

int rtaudio_pa_map_input=0;
int rtaudio_pa_chmap_in[2];

// %%%%%%%%%%%%%%%%%%%%%%%%
#ifdef USE_IOSAUDIO

#define RT_FRAMESIZE 128

int iphone_realtime_audio_init(double,unsigned int);
int iphone_realtime_audio_start(void (*)(float*,unsigned int, void*));
int iphone_realtime_audio_stop();
void iphone_setvolume(double);
void ln_setMicrophoneGain(float);

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

static void rtaudio_start(int samplerate, double volume) 
{
  static int needsinit=1;
  if (needsinit) {
    rtaudio_srate = samplerate;
    if (rtaudio_initcb) rtaudio_initcb(samplerate);
    iphone_realtime_audio_init(rtaudio_srate,RT_FRAMESIZE);
    iphone_setvolume(volume);
    needsinit=0;
  }
  iphone_realtime_audio_start(rtaudio_callback);
}

static void rtaudio_stop(void) {
  if (rtaudio_closecb) rtaudio_closecb();
  iphone_realtime_audio_stop();
}

#endif // USE_IOSAUDIO

// %%%%%%%%%%%%%%%%%%%%%%%%
#ifdef USE_PORTAUDIO

//#define USE_PA_INT16 1
#define USE_PA_INT32 1
//#define USE_PA_FLOAT32 1

#include <portaudio.h>

#ifdef MACOSX
#include <pa_mac_core.h>
#endif

//#define FRAMES_PER_CALLBACK         1024
#define RT_FRAMES_PER_CALLBACK         64
#define INPUT_DEVICE   (Pa_GetDefaultInputDevice())
#define OUTPUT_DEVICE  (Pa_GetDefaultOutputDevice())
#ifdef USE_PA_INT16
#define SAMPLE_FORMAT  paInt16
#define SAMPLE_TYPE    short
#define FLO(x) ((double)x/32767.)
#define FIX(x) ((short)(32767.*x))
#endif
#ifdef USE_PA_INT32
#define SAMPLE_FORMAT  paInt32
#define SAMPLE_TYPE    int
#define FLO(x) ((double)x/2147483647.)
#define FIX(x) ((int)(2147483647.*x))
#endif
#ifdef USE_PA_FLOAT32
#define SAMPLE_FORMAT  paFloat32
#define SAMPLE_TYPE    float
#define FLO(x) (x)
#define FIX(x) (x)
#endif

extern int portaudio_needsinit;
extern int portaudio_idev;
extern int portaudio_odev;

static int portaudio_use_mono=0;

static PaStream *stream;

static int rt_portaudio_cb( const void *inputBuffer, void *outputBuffer,
   unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
   PaStreamCallbackFlags statusFlags, void *userData )
{
  int i;
  SAMPLE_TYPE *ibuf = (SAMPLE_TYPE*)inputBuffer;
  SAMPLE_TYPE *obuf=(SAMPLE_TYPE*)outputBuffer;
  if (!ibuf||!obuf) return 0;
  for (i=0;i<framesPerBuffer;i++) {
    float ival = (portaudio_use_mono?FLO(ibuf[i]):FLO(ibuf[2*i]));
    float oval1,oval2;
     if (rtaudio_inputcb) rtaudio_inputcb(ival);
     if (rtaudio_outputcb) rtaudio_outputcb(&oval1,&oval2);
     obuf[2*i]=FIX(oval1); obuf[2*i+1]=FIX(oval2);
  }
  return 0;
}

void rtaudio_start(int samplerate, double volume)
{
  PaError err;
  static int needsinit=1;
  // XXX set volume here
  if (needsinit) { 
    rtaudio_srate=samplerate;
    if (rtaudio_initcb) rtaudio_initcb(samplerate);
    PaStreamParameters inputParameters, outputParameters;
    if (portaudio_needsinit) {
      err = Pa_Initialize();
      if( err != paNoError ) goto error;
      portaudio_needsinit=0;
    }
    inputParameters.device = (portaudio_idev<0?INPUT_DEVICE:portaudio_idev);
    if (inputParameters.device == paNoDevice) { goto error; }
    PaDeviceInfo *deviceInfo = Pa_GetDeviceInfo(inputParameters.device);
    if (deviceInfo->maxInputChannels==1) {
      portaudio_use_mono=1;
      inputParameters.channelCount = 1;
    } else {
      portaudio_use_mono=0;
      inputParameters.channelCount = 2;
    }
    inputParameters.sampleFormat = SAMPLE_FORMAT;
    inputParameters.suggestedLatency = Pa_GetDeviceInfo( inputParameters.device )->defaultLowInputLatency;
    outputParameters.device = (portaudio_odev<0?OUTPUT_DEVICE:portaudio_odev);
    if (outputParameters.device == paNoDevice) { goto error; }
    outputParameters.channelCount = 2;
    outputParameters.sampleFormat = SAMPLE_FORMAT;
    outputParameters.suggestedLatency = Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;
#ifdef MACOSX
    static PaMacCoreStreamInfo macInfoOut;
    if (rtaudio_pa_map_output) {
      PaMacCore_SetupStreamInfo( &macInfoOut, paMacCorePlayNice );
      int chcount = Pa_GetDeviceInfo( outputParameters.device )->maxOutputChannels;
      PaMacCore_SetupChannelMap(&macInfoOut, rtaudio_pa_chmap_out, chcount);
      outputParameters.hostApiSpecificStreamInfo = &macInfoOut;    
    } else outputParameters.hostApiSpecificStreamInfo = NULL;
    static PaMacCoreStreamInfo macInfoIn;
    if (rtaudio_pa_map_input) {
      PaMacCore_SetupStreamInfo(&macInfoIn, paMacCorePlayNice);
      PaMacCore_SetupChannelMap(&macInfoIn, rtaudio_pa_chmap_in, 2);
      inputParameters.hostApiSpecificStreamInfo = &macInfoIn;    
    } else inputParameters.hostApiSpecificStreamInfo = NULL;
#else
    outputParameters.hostApiSpecificStreamInfo = NULL;
    inputParameters.hostApiSpecificStreamInfo = NULL;
#endif
    err = Pa_OpenStream( &stream, &inputParameters, &outputParameters, 
        rtaudio_srate, RT_FRAMES_PER_CALLBACK, paClipOff, rt_portaudio_cb, 0);
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

void rtaudio_stop(void) { 
  if (rtaudio_closecb) rtaudio_closecb();
  Pa_StopStream( stream );
}

void ln_setMicrophoneGain(float inputGain) {}

#endif // USE_PORTAUDIO

// %%%%%%%%%%%%%%%%%%%%%%%%
#ifdef USE_ANDROIDAUDIO

int opensl_on=0;
int opensl_rate=0;

void SoundPoolInit(void);
int SoundPoolSetVolume(float);

void rtaudio_start(int samplerate, double volume)  
{
  static int opensl_needsinit=1;
  if (opensl_needsinit) {
    rtaudio_srate=samplerate;
    SoundPoolInit();
    if (volume>=0) SoundPoolSetVolume(volume);
    if (rtaudio_initcb) rtaudio_initcb(samplerate);
    opensl_needsinit=0;
  }
  opensl_on=1;
  opensl_rate=rtaudio_srate;
}

void rtaudio_stop(void) { 
  if (opensl_on&&rtaudio_closecb) rtaudio_closecb();
  opensl_on=0;
}

void ln_setMicrophoneGain(float inputGain) {}

#endif // USE_ANDROIDAUDIO

// %%%%%%%%%%%%%%%%%%%%%%%%
#ifdef USE_BB10AUDIO

void rtaudio_start(int samplerate, double volume) 
{
  rtaudio_srate=samplerate; 
}
void rtaudio_stop(void) { }
void ln_setMicrophoneGain(float inputGain) {}

#endif // USE_BB10AUDIO

end-of-c-declare
)

;; ffi

(define rtaudio-start (c-lambda (int double) void "rtaudio_start"))
(define rtaudio-stop (c-lambda () void "rtaudio_stop"))

(define rtaudio-set-gain (c-lambda (float) void "ln_setMicrophoneGain"))

;; eof
