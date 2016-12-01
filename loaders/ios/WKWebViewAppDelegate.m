// LambdaNative WKWebView loader

/* portions:

 Copyright (c) 2014 Lee Barney
 Permission is hereby granted, free of charge, to any person obtaining a
 copy of this software and associated documentation files (the "Software"),
 to deal in the Software without restriction, including without limitation the
 rights to use, copy, modify, merge, publish, distribute, sublicense,
 and/or sell copies of the Software, and to permit persons to whom the Software
 is furnished to do so, subject to the following conditions:
 
 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.
 
 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
 OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*/

#ifdef USE_HYBRID

#import <LNCONFIG.h>
#import "config_custom.h"

#import "WKWebViewAppDelegate.h"
#import "WKWebViewController.h"

@class WKWebViewController;

@implementation WKWebViewAppDelegate

@synthesize window;
@synthesize controller;

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
  CGRect screenSize = [[UIScreen mainScreen] bounds];
  window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] applicationFrame]];
  WKWebViewController *theController = [[WKWebViewController alloc] init];
  self.controller = theController;
  [theController release];
  [self.window setRootViewController:self.controller];
  [window makeKeyAndVisible];
  ffi_event(EVENT_INIT,screenSize.size.width,screenSize.size.height);
  return YES;
}

- (void)applicationWillResignActive:(UIApplication *)application {
  ffi_event(EVENT_SUSPEND,0,0);
}

- (void)applicationDidEnterBackground:(UIApplication *)application {
}

- (void)applicationWillEnterForeground:(UIApplication *)application {
}

- (void)applicationDidBecomeActive:(UIApplication *)application {
  ffi_event(EVENT_RESUME,0,0);
}

- (void)applicationWillTerminate:(UIApplication *)application {
  ffi_event(EVENT_CLOSE,0,0);
  ffi_event(EVENT_TERMINATE,0,0);
}

- (void)dealloc {
  [window release];
  [controller release];
  [super dealloc];
}

@end

#endif // USE_HYBRID

