#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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

;; OPUS VOIP module, uses the OPUS codec http://www.opus-codec.org/ for voip purposes
(define opus:thread #f)

(c-declare  #<<end-of-c-declare

// Some function prototyes
void rtaudio_register(void (*)(int), void (*)(float), void (*)(float*,float*),void (*)(void));
void opusvoip_rtaudio_start(int flag, double volume);
void opusvoip_hangup();
void opusvoip_phone_hangup();

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#ifdef WIN32
  #include <winsock2.h>
#else
  #include <netinet/in.h>
#endif
#include <sys/types.h>
#include <opus/opus.h>
#ifdef ANDROID
  #include <fcntl.h>
  #include <sys/socket.h>
#else
  #include <sys/fcntl.h>
#endif

#ifdef IOS
  #include <sys/select.h>
  #include <unistd.h>
#endif

// OPUS PORT
#define OPUS_PORT 8032

// Variables for socket
static int sock_in, sock_out;
#define MAX_PACKETLEN 1276
static unsigned char *packet_in;
static unsigned char *packet_out;
static fd_set readfds;
static struct timeval timeout;
static char SEND_READY;
static int recv_bytes=0;
static int send_bytes=0;

// Variables for OPUS
static OpusEncoder *enc;
static OpusDecoder *dec;
static int frame_size;
static float *fin;
static float *fout;
static float *fout_send;
static float *fin_recv;
static int fin_recv_pos;
static int fin_pos=0;
static int fout_pos=0;
static char fin_reset=1;
#define FIN_BUF_FACTOR 5
#define PHONE_SAMPLINGRATE 8000

// Variables for call states
static int ringer=0;
static int state=0;
#define PHONE_ONHOOK 0
#define PHONE_CONNECTED 1
#define PHONE_RINGING 2
#define PHONE_ACCEPT 3
#define PHONE_DIALING 4

// ------------------------
//    LIBOPUS functions
// ------------------------

// internal initialization function - use this one if you want different settings!
int init_opus_settings(int Fs, int channels, int application){
  // Make buffer for receiving;
  packet_in=malloc(MAX_PACKETLEN);
  // Make buffer for sending and reserve first 3 bytes for header info.
  packet_out=malloc(MAX_PACKETLEN);
  packet_out+=3;

  //Now make the Opus buffers
  frame_size=Fs*0.060; // Optimal size is 20msec, can be up to 60msec.
  int buf_len=frame_size*sizeof(float);
  fin=malloc(buf_len*FIN_BUF_FACTOR);
  fin_recv=malloc(buf_len);
  fout=malloc(buf_len);
  fout_send=malloc(buf_len);
  fin_recv_pos=frame_size*(FIN_BUF_FACTOR-1);
  int error;
  //Decoder
  dec=opus_decoder_create(Fs, channels, &error);
  if (error!=OPUS_OK) return error;

  //Encoder
  enc=opus_encoder_create(Fs, channels, application, &error);
  return error;
}

// Initialize Opus encoder and decoder
int init_opus(){
  int Fs=PHONE_SAMPLINGRATE;
  int channels=1;
  int application=OPUS_APPLICATION_VOIP;
  return init_opus_settings(Fs,channels,application);
}

// Clear our data buffers
int opusvoip_clear_buffers(){
  int i;
  int buf_len=frame_size;
  for (i=0;i<buf_len*FIN_BUF_FACTOR;i++) fin[i]=0.;
  for (i=0;i<buf_len;i++) fin_recv[i]=0.;
  for (i=0;i<buf_len;i++) fout[i]=0.;
  for (i=0;i<buf_len;i++) fout_send[i]=0.;
  fout_pos=0;
  SEND_READY=0;
}


// ------------------------------------
//    UDP communication functions
// ------------------------------------

// Make UDP listening socket
int init_udp_listener(voip){
  //Make a socket
  #ifdef WIN32
    WSADATA wsaData;
    WSAStartup(MAKEWORD(2, 2), &wsaData);
  #endif
  sock_in=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if (sock_in==-1){
    return 1;
  }

  // Set socket timeout to 10us
  timeout.tv_sec=0;
  timeout.tv_usec=10;

  //Set up server address
  struct sockaddr_in sa_in;
  sa_in.sin_port=htons(OPUS_PORT);
  sa_in.sin_addr.s_addr=INADDR_ANY;
  sa_in.sin_family=AF_INET;
  memset(&sa_in.sin_zero, 0, sizeof(sa_in.sin_zero));

  // Make a server
  if (bind (sock_in, (struct sockaddr*)&sa_in, sizeof(struct sockaddr)) <0){
    return 2;
  }
  return 0;
}

// Open UDP sending socket
int init_udp_sender(int dst_ip){
  //Make a socket
  #ifdef WIN32
    WSADATA wsaData;
    WSAStartup(MAKEWORD(2, 2), &wsaData);
  #endif
  sock_out=socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  if (sock_out==-1){
    return 1;
  }

  // Set socket timeout to 10us
  timeout.tv_sec=0;
  timeout.tv_usec=10;

  //Set up server address
  struct sockaddr_in sa_out;
  sa_out.sin_port=htons(OPUS_PORT);
  sa_out.sin_addr.s_addr=htonl((unsigned int)dst_ip);
  sa_out.sin_family=AF_INET;
  memset(&sa_out.sin_zero, 0, sizeof(sa_out.sin_zero));

  // Connect to server
  if (connect (sock_out, (struct sockaddr*)&sa_out, sizeof(struct sockaddr)) <0){
    return 3;
  }
  return 0;
}


// Send UDP data
int udp_send(unsigned char *packet, int len, int type){
  packet[-3]=76;
  packet[-2]=78;
  packet[-1]=type;
  int ret=send(sock_out, packet-3, len+3, 0);
  if (ret > 0){
    send_bytes+=ret;
  }
  return ret;
}

// Receive UDP data
int udp_receive(){
  FD_ZERO(&readfds);
  FD_SET(sock_in, &readfds);
  int got_data=select(sizeof(readfds)*8, &readfds, NULL, NULL, &timeout);
  if (got_data>0){
    // Reset the fin loop position to start once we get data. This is to separate revc&output
    if (fin_reset==1){
      fin_pos=0;
      fin_reset=0;
    }
    return recv(sock_in,packet_in,MAX_PACKETLEN,0); //WARNING: THIS BLOCKS, so only do if data in buffer!
  }
  return 0;
}


// ------------------------------------
//    Phone interface functions
// ------------------------------------

// Setup the opus environment by initializing the encode, decoder and making the udp listening socket.
void opusvoip_setup(){
  int error=init_opus();
  if (error!=OPUS_OK) return;
  error=init_udp_listener();
  if (error>0) return;
}

// Take down the encoder, decoder and all sockets.
void opusvoip_close(){
  opus_encoder_destroy(enc);
  opus_decoder_destroy(dec);
  close(sock_in);
  opusvoip_hangup();
  #ifdef WIN32
    WSACleanup();
  #endif
}

// Hangup a phone call
void opusvoip_hangup(){
  // Send the close command to the other side
  if (state!=PHONE_ONHOOK) udp_send(packet_out, 0, PHONE_ONHOOK);
  close(sock_out);
  ringer=0;
  send_bytes=recv_bytes=0;
  state=PHONE_ONHOOK;
}

// Pick up the phone call
void opusvoip_pickup(){
  ringer=0;
  if (state!=PHONE_CONNECTED) udp_send(packet_out,0,PHONE_ACCEPT);
  send_bytes=recv_bytes=0;
  state=PHONE_CONNECTED;
}

// Place a phone call
void opusvoip_call(int localip, int remoteip){
  init_udp_sender(remoteip);
  packet_out[0]=(localip>>24) & 0xFF;
  packet_out[1]=(localip>>16) & 0xFF;
  packet_out[2]=(localip>>8) & 0xFF;
  packet_out[3]=localip & 0xFF;
  udp_send(packet_out,4,PHONE_DIALING);
  ringer=1;
  state=PHONE_DIALING;
}

// Deal with an incoming call
void opusvoip_ringing(int remoteip){
  init_udp_sender(remoteip);
  opusvoip_rtaudio_start(PHONE_SAMPLINGRATE,-1.);
  ringer=1;
  state=PHONE_RINGING;
}

int opusvoip_state(){
  return state;
}

int opusvoip_sendbytes(){
  return send_bytes;
}

int opusvoip_recvbytes(){
  return recv_bytes;
}

// ------------------------------------
//    Communication thread functions
// ------------------------------------

// Send UDP data to remote host
void opusvoip_sender(){
  SEND_READY=0;
  int len=opus_encode_float(enc, fout_send, frame_size, packet_out, MAX_PACKETLEN);
  udp_send(packet_out,len,PHONE_CONNECTED);
}

// Check if remote host send data and place in our audio buffer
void opusvoip_receiver(){
  // Check for new data from network
  int packet_len=udp_receive();

  // Check if our desired header is present
  if (packet_len<3 || packet_in[0]!=76 || packet_in[1]!=78)
  return;
  // Save the data len;
  recv_bytes+=packet_len;

  // Check if it is a hangup.
  if (packet_len>=3 && packet_in[2]==PHONE_ONHOOK){
    opusvoip_phone_hangup();
  // Or a call
  }else if (packet_len>=7 && packet_in[2]==PHONE_DIALING){
    int rip=(packet_in[3]<<24)+(packet_in[4]<<16)+(packet_in[5]<<8)+packet_in[6];
    opusvoip_ringing(rip);
  // Or a connection acceptance
  }else if (packet_len>=3 && packet_in[2]==PHONE_ACCEPT){
    opusvoip_pickup();
  }else if (packet_len>3 && packet_in[2]==PHONE_CONNECTED){
    int len=opus_decode_float(dec, packet_in+3, packet_len-3, fin_recv, frame_size, 0);
    //Inject the received data into the fin buffer for playback
    int i;
    for (i=0;i<len;i++){
      fin[fin_recv_pos++]=fin_recv[i];
      if (fin_recv_pos==(frame_size*FIN_BUF_FACTOR)){
        fin_recv_pos=0;
      }
    }
  }
}

// Function called by communication thread for udp and rtaudio data exchange
void opusvoip_handledata(){
  if (SEND_READY) opusvoip_sender();
  opusvoip_receiver();
}


// --------------------------
//    Bindings for rtaudio
// --------------------------

// AUDIO IN function
void opusvoip_audio_input(float val){
  fout[fout_pos++]=val;
  if (fout_pos==frame_size){

  // Don't send audio while still ringing.
    if (!ringer) SEND_READY=1;
    memcpy(fout_send,fout,frame_size*sizeof(float));
    fout_pos=0;
  }
}

// AUDIO OUT function
void opusvoip_audio_output(float *left, float *right){
  // Ring a call
  static double t=0;
  if (ringer) {
    fin[fin_pos]=(t>0.5?0.3*sin(2*M_PI*400*t):0);
    t+=1./PHONE_SAMPLINGRATE;
    if (t>1.5) t=0.;
  }

  // Export sound to be played and clear thereafter.
  *left=*right=fin[fin_pos];
  fin[fin_pos++]=0;
  // Reset buffer if at end
  if (fin_pos==(frame_size*FIN_BUF_FACTOR)){
    fin_pos=0;
  }
}

void opusvoip_audio_init(int flag){
  opusvoip_clear_buffers();
}

void opusvoip_audio_close(){
  opusvoip_clear_buffers();
  SEND_READY=0;
}

end-of-c-declare
)

;; Need a FFI so we can call rtaudio-start and rtaudio-stop from the protocol
(c-define (opusvoip:rtaudio_start flag vol) (int double) void "opusvoip_rtaudio_start" ""
  (rtaudio-start flag vol)
)
(c-define (opusvoip:hangup) () void "opusvoip_phone_hangup" ""
  (phone-hangup)
  (rtaudio-stop)
)

(define (opusvoip-getrecvbytes)
  ((c-lambda () int "___result=opusvoip_recvbytes();"))
)
(define (opusvoip-getsendbytes)
  ((c-lambda () int "___result=opusvoip_sendbytes();"))
)

;; register the opusvoip audio hooks
((c-lambda () void "rtaudio_register(opusvoip_audio_init,opusvoip_audio_input,opusvoip_audio_output,opusvoip_audio_close);"))

;; eof
