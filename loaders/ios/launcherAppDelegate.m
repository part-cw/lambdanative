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

#import <CONFIG.h>

#import "config_custom.h"

#import "launcherAppDelegate.h"
#import "EAGLView.h"

#import <Foundation/Foundation.h>
#import <AVFoundation/AVFoundation.h>
#import <CoreAudio/CoreAudioTypes.h>

// for setting audio volume
#ifdef USE_AUDIOVOLUMEINIT
#import <MediaPlayer/MediaPlayer.h>
#endif

@implementation launcherAppDelegate

@synthesize window;
@synthesize glView;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
   
   CGRect screenSize = [[UIScreen mainScreen] bounds];

  window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] applicationFrame]];
  glView = [[EAGLView alloc] initWithFrame:window.frame];
  
  [window addSubview:glView];
  [window makeKeyAndVisible]; 

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

  [glView startAnimation];
}

- (void)applicationWillResignActive:(UIApplication *)application {
  ffi_event(EVENT_SUSPEND,0,0);
  [glView stopRender];
}

- (void)applicationDidBecomeActive:(UIApplication *)application {
  [glView startRender];
  ffi_event(EVENT_RESUME,0,0);
}

- (void)applicationDidEnterBackground:(UIApplication *)application {
//  ffi_event(EVENT_SUSPEND,0,0);
//  [glView stopRender];
}

- (void)applicationWillEnterForeground:(UIApplication *)application {
//  [glView startRender];
//  ffi_event(EVENT_RESUME,0,0);
}

- (void)applicationWillTerminate:(UIApplication *)application {
  ffi_event(EVENT_CLOSE,0,0);
  ffi_event(EVENT_TERMINATE,0,0);
}

#ifdef USE_ORIENTATION
- (void) didRotate:(NSNotification *)notification
{
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

- (void)dealloc {
  [window release];
  [glView release];
  [super dealloc];
}

@end
