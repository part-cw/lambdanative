/*
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
*/
// glue MoMu C++ into C subsystem

// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// audio and vibration

#import "config_custom.h"

#import "mo_audio.h"
#import <MediaPlayer/MediaPlayer.h>
#import <AudioToolbox/AudioToolbox.h>

int iphone_audioroute = 0;

extern "C" int iphone_realtime_audio_init(double srate, unsigned int framesize){
  bool result;
  result = MoAudio::init(srate, framesize, 2);
  return (!result?0:1);
}

extern "C" int iphone_realtime_audio_start(MoCallback cb){
  bool result;
  result = MoAudio::start(cb, NULL);
  return (!result?0:1);
}

extern "C" int iphone_realtime_audio_stop(){
  MoAudio::stop();
  return 1;
}

extern "C" void iphone_setvolume(double v){
  [[MPMusicPlayerController applicationMusicPlayer] setVolume:v];
}

extern "C" double iphone_getvolume(){
  Float32 volume=0;
  UInt32 dataSize = sizeof(Float32);
  AudioSessionGetProperty(kAudioSessionProperty_CurrentHardwareOutputVolume,&dataSize,&volume);
  return (double)volume;
}

extern "C" int iphone_headphonepresent(){
  UInt32 routeSize = sizeof (CFStringRef);
  CFStringRef route;
  OSStatus error = AudioSessionGetProperty (kAudioSessionProperty_AudioRoute, &routeSize, &route);
  if (!error && (route != NULL)) {
    NSString* routeStr = (NSString*)route;
    NSRange headphoneRange = [routeStr rangeOfString : @"Head"];
    if (headphoneRange.location != NSNotFound) return 1;
  }
  return 0;
}

extern "C" void iphone_vibrate(){
  MoAudio::vibrate();
}

// %%%%%%%%%%%%%%%%%%%%%%%%%%%
// accelerometer

#import "mo_accel.h"

extern "C" double ios_accel_getx() { return MoAccel::getX(); }
extern "C" double ios_accel_gety() { return MoAccel::getY(); }
extern "C" double ios_accel_getz() { return MoAccel::getZ(); }

// %%%%%%%%%%%%%%%%%%%%%%%%%%%
// location

#import "mo_location.h"

extern "C" double ios_location_getlatitude(){
  CLLocation *tmp = MoLocation::getLocation();
  return tmp.coordinate.latitude;
}

extern "C" double ios_location_getlongitude(){
  CLLocation *tmp = MoLocation::getLocation();
  return tmp.coordinate.longitude;
}

extern "C" double ios_location_getaltitude(){
  CLLocation *tmp = MoLocation::getLocation();
  return tmp.altitude;
}

extern "C" double ios_location_getaccuracy(){
  CLLocation *tmp = MoLocation::getLocation();
  return tmp.horizontalAccuracy;
}

extern "C" int ios_location_gettimestamp(){
  CLLocation *tmp = MoLocation::getLocation();
  return (int)[tmp.timestamp timeIntervalSince1970];
}

// %%%%%%%%%%%%%%%%%%%%%%%%%%%
// launch url

extern "C" void ios_launch_url(char *urlchar){
  NSString* urlString = [NSString stringWithUTF8String: urlchar];
  NSURL *url = [NSURL URLWithString:urlString];
  [[UIApplication sharedApplication] openURL:url];
}

// %%%%%%%%%%%%%%%%%%%%%%%%%%%
// gyroscope

#ifdef USE_GYROSCOPE
#import <CoreMotion/CoreMotion.h>
#endif

static double ios_gyro(int idx)
{
  double value=1.0;
#ifdef USE_GYROSCOPE
  static int needsinit=1;
  static CMMotionManager *motionManager;
  if (needsinit) {
    motionManager = [[CMMotionManager alloc] init];
    motionManager.deviceMotionUpdateInterval = 1.0/60.0;
    if  (motionManager.isDeviceMotionAvailable) {
      [motionManager startDeviceMotionUpdates];
    }
    needsinit=0;
  }
  CMDeviceMotion *deviceMotion = motionManager.deviceMotion;      
  CMAttitude *attitude = deviceMotion.attitude;
  if (idx==0) value=(double)attitude.yaw; 
  if (idx==1) value=(double)attitude.pitch; 
  if (idx==2) value=(double)attitude.roll; 
#endif
  return value;
}

extern "C" double ios_gyro_yaw() { return ios_gyro(0); }
extern "C" double ios_gyro_pitch() { return ios_gyro(1); }
extern "C" double ios_gyro_roll() { return ios_gyro(2); }

// %%%%%%%%%%%%%%%%%%%%%%%%%%%
// Apple Push Notifications
char pushnotification_devicetoken[512];
int pushnotification_gottoken;

// %%%%%%%%%%%%%%%%%%%%%%%%%%%
// Apple Local Notifications

char localnotification_msg[100];
int localnotification_gotmsg;
double localnotification_timestamp;
extern "C" int ios_localnotification_schedule(char* text, double time) {
  NSArray *eventArray = [[UIApplication sharedApplication] scheduledLocalNotifications];
  UILocalNotification* localNotification = [[UILocalNotification alloc] init];
  int eventnum = [eventArray count];
  localNotification.timeZone = [NSTimeZone localTimeZone];
  localNotification.fireDate = [NSDate dateWithTimeIntervalSince1970: time];
  localNotification.alertBody = [NSString stringWithUTF8String: text];
  localNotification.applicationIconBadgeNumber =
    [UIApplication sharedApplication].applicationIconBadgeNumber + eventnum + 1;
  [[UIApplication sharedApplication] scheduleLocalNotification:localNotification];
  // see if we got it scheduled?
  eventArray = [[UIApplication sharedApplication] scheduledLocalNotifications];
  if ([eventArray count]>eventnum) return 1;
  return 0;
}

// %%%%%%%%%%%%%%%%%%%%%%%%%%%
// device identification

#import <sys/sysctl.h>
//#import "UIDevice-Hardware.h"

extern "C" int ios_hardware_id()
{
  int res=0;
  size_t size;
  sysctlbyname("hw.machine", NULL, &size, NULL, 0);
  char *model = (char*)malloc(size);
  sysctlbyname("hw.machine", model, &size, NULL, 0);
  if (!strcmp(model,"iPhone1,1")) res=1;
  if (!strcmp(model,"iPhone1,2")) res=2;
  if (!strcmp(model,"iPhone2,1")) res=3;
  if (!strcmp(model,"iPhone3,1")) res=4;
  if (!strcmp(model,"iPhone3,3")) res=5;
  if (!strcmp(model,"iPhone4,1")) res=6;
  if (!strcmp(model,"iPhone5,1")) res=7;
  if (!strcmp(model,"iPhone5,2")) res=8;
  if (!strcmp(model,"iPod1,1")) res=16+1;
  if (!strcmp(model,"iPod2,1")) res=16+2;
  if (!strcmp(model,"iPod3,1")) res=16+3;
  if (!strcmp(model,"iPod4,1")) res=16+4;
  if (!strcmp(model,"iPod5,1")) res=16+5;
  if (!strcmp(model,"iPad1,1")) res=32+1;
  if (!strcmp(model,"iPad2,1")) res=32+2;
  if (!strcmp(model,"iPad2,2")) res=32+3;
  if (!strcmp(model,"iPad2,3")) res=32+4;
  if (!strcmp(model,"iPad2,4")) res=32+5;
  if (!strcmp(model,"iPad2,5")) res=32+6;
  if (!strcmp(model,"iPad2,6")) res=32+7;
  if (!strcmp(model,"iPad2,7")) res=32+8;
  if (!strcmp(model,"iPad3,1")) res=32+9;
  if (!strcmp(model,"iPad3,2")) res=32+10;
  if (!strcmp(model,"iPad3,3")) res=32+11;
  if (!strcmp(model,"iPad3,4")) res=32+12;
  if (!strcmp(model,"iPad3,5")) res=32+13;
  if (!strcmp(model,"iPad3,6")) res=32+14;
  free(model); 
  return res;
}

// eof
