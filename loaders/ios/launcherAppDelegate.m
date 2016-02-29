/*
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
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

#import <LNCONFIG.h>

#import "config_custom.h"

#import "launcherAppDelegate.h"
#import "EAGLView.h"
#import "GLViewController.h"

#import <Foundation/Foundation.h>
#import <AVFoundation/AVFoundation.h>
#import <CoreAudio/CoreAudioTypes.h>

// for setting audio volume
#ifdef USE_AUDIOVOLUMEINIT
#import <MediaPlayer/MediaPlayer.h>
#endif

@class GLViewController;
@class EAGLView;

@implementation launcherAppDelegate

@synthesize window;
@synthesize glView;
@synthesize controller;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
  DMSG("applicationDidFinishLaunching");
  CGRect screenSize = [[UIScreen mainScreen] bounds];
  window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] applicationFrame]];

  GLViewController *theController = [[GLViewController alloc] init];
  self.controller = theController;
  [theController release];

  [self.window setRootViewController:self.controller];
  
  [window makeKeyAndVisible];

#ifdef USE_PUSHNOTIFICATION
// Let the device know we want to receive push notifications
  [[UIApplication sharedApplication] registerForRemoteNotificationTypes:
    (UIRemoteNotificationTypeBadge | UIRemoteNotificationTypeSound | UIRemoteNotificationTypeAlert)];
#endif

#ifdef USE_LOCALNOTIFICATION
// Let the device know we want to receive local notifications
  #ifdef __IPHONE_8_0
  if ([application respondsToSelector:@selector(registerUserNotificationSettings:)]) {
    [[UIApplication sharedApplication] registerUserNotificationSettings:
      (UIUserNotificationTypeBadge | UIUserNotificationTypeSound | UIUserNotificationTypeAlert)];
  }
  #endif
// Clear the badge icon
[UIApplication sharedApplication].applicationIconBadgeNumber = 0;
#endif

#ifdef USE_NOLOCK
// this will prevent the screen from locking up
   [UIApplication sharedApplication].idleTimerDisabled = YES;
#endif

#ifdef USE_ORIENTATION
  [[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
  [[NSNotificationCenter defaultCenter] addObserver:self
      selector:@selector(didRotate:)
      name:@"UIDeviceOrientationDidChangeNotification" object:nil];
#endif // USE_ORIENTATION

#ifdef USE_AUDIOINIT
// this is (possibly?) necessary for getting audio from sleeping/backgrounded apps
{
  NSError *myErr;
  BOOL    bAudioInputAvailable = FALSE;
  AVAudioSession *audioSession = [AVAudioSession sharedInstance];
  bAudioInputAvailable= [audioSession inputIsAvailable];
  if( bAudioInputAvailable)
  {
    [audioSession setCategory:AVAudioSessionCategoryPlayAndRecord error:&myErr];
#ifdef USE_AUDIOVOLUMEINIT
    [[MPMusicPlayerController applicationMusicPlayer] setVolume:1.0];
#endif // USE_AUDIOVOLUMEINIT
  }
  else {
    [audioSession setCategory:AVAudioSessionCategoryPlayback error:&myErr];
  }
  [audioSession setActive: YES error: &myErr];  
}
#endif // USE_AUDIOINIT

  ffi_event(EVENT_INIT,screenSize.size.width,screenSize.size.height);

}

- (void)applicationWillResignActive:(UIApplication *)application {
  DMSG("applicationWillResignActive");
  ffi_event(EVENT_SUSPEND,0,0);
  [glView stopRender];
}

- (void)applicationDidBecomeActive:(UIApplication *)application {
  DMSG("applicationDidBecomeActive");
  [glView startRender];
  ffi_event(EVENT_RESUME,0,0);
}

- (void)applicationDidEnterBackground:(UIApplication *)application {
  DMSG("applicationDidEnterBackground");
//  ffi_event(EVENT_SUSPEND,0,0);
//  [glView stopRender];
}

- (void)applicationWillEnterForeground:(UIApplication *)application {
  DMSG("applicationWillEnterForeground");
//  [glView startRender];
//  ffi_event(EVENT_RESUME,0,0);
}

- (void)applicationWillTerminate:(UIApplication *)application {
  DMSG("applicationWillTerminate");
  ffi_event(EVENT_CLOSE,0,0);
  ffi_event(EVENT_TERMINATE,0,0);
}

#ifdef USE_ORIENTATION
- (void) didRotate:(NSNotification *)notification
{
  DMSG("didRotate");
  int res = -1;
  UIDeviceOrientation orientation = [[UIDevice currentDevice] orientation];
  switch (orientation) {
    case UIDeviceOrientationPortrait:
      res=GUI_PORTRAIT; break;
    case UIDeviceOrientationLandscapeLeft:
      res=GUI_LANDSCAPE; break;
    case UIDeviceOrientationLandscapeRight:
      res=GUI_SEASCAPE; break;
    case UIDeviceOrientationPortraitUpsideDown:
      res=GUI_UPSIDEDOWN; break;
    case UIDeviceOrientationUnknown:
    case UIDeviceOrientationFaceUp:
    case UIDeviceOrientationFaceDown:
    default:
      break;
  }
  if (res>=0) ffi_event(EVENT_ORIENTATION,res,0);
}
#endif // USE_ORIENTATION

#ifdef USE_PUSHNOTIFICATION
extern char pushnotification_devicetoken[512];
extern int pushnotification_gottoken;
- (void)application:(UIApplication*)application didRegisterForRemoteNotificationsWithDeviceToken:(NSData*)deviceToken{
  int len = [deviceToken length];
  Byte *token = (Byte*)malloc(len);
  memcpy(token, [deviceToken bytes], len);
  int i;
  for (i=0;i<len;i++){
    pushnotification_devicetoken[i]=token[i];
  }
  free(token);
  pushnotification_gottoken=len;
}
- (void)application:(UIApplication*)application didFailToRegisterForRemoteNotificationsWithError:(NSError*)error {
  pushnotification_gottoken=0;
  NSLog(@"Failed to get APN token: %@", error);
}
#endif

#ifdef USE_LOCALNOTIFICATION
extern char localnotification_msg[100];
extern int localnotification_gotmsg;
extern double localnotification_timestamp;
- (void)application:(UIApplication*)application didReceiveLocalNotification:(UILocalNotification *)notification{
  int len = [notification.alertBody length];
  const char* msg = [notification.alertBody cStringUsingEncoding:[NSString defaultCStringEncoding]];
  int i;
  for (i=0;i<len;i++){
    localnotification_msg[i] = msg[i];
  }
  localnotification_msg[len] = 0;
  localnotification_timestamp = [notification.fireDate timeIntervalSince1970];
  localnotification_gotmsg = 1;
  UIApplicationState state = [application applicationState];
  ffi_event(EVENT_NOTIFICATION,0,0);
  [UIApplication sharedApplication].applicationIconBadgeNumber = 0;
}
#endif

- (void)dealloc {
  DMSG("dealloc");
  [window release];
  [glView release];
  [controller release];
  [super dealloc];
}

@end
