#import <LNCONFIG.h>
#import "config_custom.h"

#import <UIKit/UIKit.h>
#ifdef USE_HYBRID
#import "WKWebViewController.h"
#else
#import "GLViewController.h"
#endif

static BOOL applicationInitialized = NO;

@interface SceneDelegate : UIResponder <UIWindowSceneDelegate>
@property (nonatomic, retain) UIWindow *window;
#ifdef USE_HYBRID
@property (nonatomic, retain) WKWebViewController *controller;
#else
@property (nonatomic, retain) GLViewController *controller;
#endif
@end

@implementation SceneDelegate

- (void)scene:(UIScene *)scene
    willConnectToSession:(UISceneSession *)session
                 options:(UISceneConnectionOptions *)connectionOptions {
  if (![scene isKindOfClass:[UIWindowScene class]]) {
    return;
  }

  UIWindowScene *windowScene = (UIWindowScene *)scene;
  self.window = [[[UIWindow alloc] initWithWindowScene:windowScene] autorelease];
#ifdef USE_HYBRID
  self.controller = [[[WKWebViewController alloc] init] autorelease];
#else
  self.controller = [[[GLViewController alloc] init] autorelease];
#endif
  self.window.rootViewController = self.controller;
  [self.window makeKeyAndVisible];

  if (!applicationInitialized) {
    CGSize size = windowScene.coordinateSpace.bounds.size;
    ffi_event(EVENT_INIT, size.width, size.height);
    applicationInitialized = YES;
  }
}

- (void)sceneWillResignActive:(UIScene *)scene {
#ifndef USE_HYBRID
  [self.controller stopRender];
#endif
  ffi_event(EVENT_SUSPEND, 0, 0);
}

- (void)sceneDidBecomeActive:(UIScene *)scene {
#ifndef USE_HYBRID
  [self.controller startRender];
#endif
  ffi_event(EVENT_RESUME, 0, 0);
}

- (void)dealloc {
  [_window release];
  [_controller release];
  [super dealloc];
}

@end
