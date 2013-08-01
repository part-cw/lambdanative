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

// %%%%%%%%%%%%%%%%%%%%%%
#if defined(USE_PORTAUDIO) || defined(USE_IOS_REALTIME)

static int data_idx=0;
static int data_len=0;
static short *data=0;

struct wavfile {
  char riff[4];
  unsigned int filesize;
  char rifftype[4];
  char format_chunk_id[4];
  unsigned int format_chunksize;
  short wFormatTag;
  short nChannels;
  unsigned int nSamplesPerSec;
  unsigned int nAvgBytesPerSec;
  short nBlockAlign;
  short wBitsPerSample;
  char data_chunk_id[4];
  unsigned int data_chunksize;
  short *data;
};

static double catmullrom_interpolate( double y0,double y1, double y2,double y3, double mix)
{
   double a0,a1,a2,a3,mix2;
   mix2 = mix*mix;
   a0 = -0.5*y0 + 1.5*y1 - 1.5*y2 + 0.5*y3;
   a1 = y0 - 2.5*y1 + 2*y2 - 0.5*y3;
   a2 = -0.5*y0 + 0.5*y2;
   a3 = y1;
   return(a0*mix*mix2+a1*mix2+a2*mix+a3);
}

static short *wavdata_resample(unsigned char* olddata, unsigned int oldlen, 
   unsigned int oldrate, unsigned int *newlen, unsigned int bps, unsigned int newrate)
{
  int i;
  *newlen = ((bps==8?oldlen:oldlen/2)*newrate)/oldrate;
  short *newdata = (short*)malloc(*newlen*sizeof(short));
  for (i=0;i<*newlen;i++) { 
    int oldidx = (i*oldrate)/newrate;
    double t  = (double)i/(double)newrate;
    double t1 = (double)oldidx/(double)oldrate; 
    double t2 = (double)(oldidx+1)/(double)oldrate; 
    double y0,y1,y2,y3;
    if (bps==8){
      y0 = (double)olddata[(oldidx==0?oldidx:oldidx-1)]-127.;
      y1 = (double)olddata[oldidx]-127.;
      y2 = (double)olddata[(oldidx==oldlen-1?oldidx:oldidx+1)]-127.;
      y3 = (double)olddata[(oldidx>=oldlen-2?oldidx:oldidx+2)]-127.;
    }else{
      y0 = (double)(*(short*)(olddata+2*(oldidx==0?oldidx:oldidx-1)));
      y1 = (double)(*(short*)(olddata+2*oldidx));
      y2 = (double)(*(short*)(olddata+2*(oldidx==oldlen-1?oldidx:oldidx+1)));
      y3 = (double)(*(short*)(olddata+2*(oldidx>=oldlen-2?oldidx:oldidx+2)));
    }
    double mix = (t-t1)/(t2-t1);
    double result = catmullrom_interpolate(y0,y1,y2,y3,mix);
    if (bps==8) {
      newdata[i]= (short)(256*result);
    } else {
      newdata[i]= (short)(result);
    }
  }
  return newdata;
}

static int wavdata_load(const char *fname)
{
  unsigned int tmplen;
  short *tmpdata;
  FILE *fd;
  struct wavfile *wf = (struct wavfile *)malloc(sizeof(struct wavfile));
  if ((fd=fopen(fname, "rb") )!=NULL ) {
    fread(wf,sizeof(struct wavfile)-4,1,fd);
    if (wf->nChannels!=1) { fclose (fd); free(wf); return 0; } // we only do mono
    wf->data=(short*)malloc(wf->data_chunksize);
    if (!wf->data) return 0;
    fread(wf->data, wf->data_chunksize,1,fd);
    fclose(fd);
    tmpdata = wavdata_resample((unsigned char *)wf->data, wf->data_chunksize, 
       (int)wf->nSamplesPerSec, &tmplen, wf->wBitsPerSample, 44100);
    free(wf->data); wf->data=tmpdata;
    wf->data_chunksize=tmplen<<1;
    wf->nSamplesPerSec=44100;
    return (int)wf;
  }
  free(wf);
  return 0;
}

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
  static double t=0;
  unsigned int i;
  for (i=0; i<framesize; i++) {
    if (data&&data_idx<data_len) { 
      buffer[2*i] = buffer[2*i+1] = SHORT2FLOAT(data[data_idx++]);
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
  struct wavfile *wf = (struct wavfile *)id;
  if (wf) { data=0; data_idx=0; data_len=(wf->data_chunksize>>1); data=wf->data; }  
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
//typedef short SAMPLE;

static PaStream *stream;

#define OUTPUT_MONO(a) \
  a=(data&&data_idx<data_len?data[data_idx++]:0)

static int af_portaudio_cb( const void *inputBuffer, void *outputBuffer,
   unsigned long framesPerBuffer, const PaStreamCallbackTimeInfo* timeInfo,
   PaStreamCallbackFlags statusFlags, void *userData )
{
  int i;
  short *out=(short*)outputBuffer;
  for (i=0;i<framesPerBuffer;i++) { OUTPUT_MONO(out[i]); }
  return 0;
}

static int portaudio_init(void)
{
  PaStreamParameters inputParameters, outputParameters;
  PaError err = Pa_Initialize();
  if( err != paNoError ) goto error;
  outputParameters.device = OUTPUT_DEVICE; 
  if (outputParameters.device == paNoDevice) { goto error; }
  outputParameters.channelCount = 1;
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
  struct wavfile *wf = (struct wavfile *)id;
  if (wf) { data=0; data_idx=0; data_len=(wf->data_chunksize>>1); data=wf->data; }  
}

#endif 

// %%%%%%%%%%%%%%%%%%%%%%

#ifdef USE_ANDROID_NATIVE
void SoundPoolInit(void);
int SoundPoolLoadSFX(const char*, int);
int SoundPoolLoadSFXAsset(const char*, int);
int SoundPoolPlaySound(int, float, float, int, int, float);
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
  #if defined(USE_PORTAUDIO)|| defined(USE_IOS_REALTIME)
    return wavdata_load(name);
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

(define (audiofile-load name)
  (let ((file  (cond
          ((string=? (system-platform) "ios") 
             (string-append (system-appdirectory) (system-pathseparator) name ".wav"))
          ((member (system-platform) '("win32" "linux" "macosx" "openbsd")) 
             (string-append (system-directory) (system-pathseparator) "sounds" (system-pathseparator) name ".wav"))
          ((string=? (system-platform) "android") (string-downcase name))
          (else #f))))
  (if (or (string=? (system-platform) "android") (file-exists? file)) 
    (begin
      (log-system "audiofile: loading " file)
      ((c-lambda (char-string) int "audiofile_load") file))
    (begin 
      (log-system "audiofile: file " file " not found") 
      #f))))

(define (audiofile-play id) (if id ((c-lambda (int) void "audiofile_play") id)))
(define (audiofile-forceplay id) (if id ((c-lambda (int) void "audiofile_forceplay") id)))

;;eof
