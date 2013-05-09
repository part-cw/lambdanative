// very primitive cocoa wrapper

#import <AppKit/AppKit.h>

#include "CONFIG.h"

#import "SimpleOpenGLView.h"

int main(int argc, const char *argv[]) {
  [NSAutoreleasePool new];
  [NSApplication sharedApplication];
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

  // minimal menu bar
  id menubar = [[NSMenu new] autorelease];
  id appMenuItem = [[NSMenuItem new] autorelease];
  [menubar addItem:appMenuItem];
  [NSApp setMainMenu:menubar];
  id appMenu = [[NSMenu new] autorelease];
  id appName = [[NSProcessInfo processInfo] processName];
  id quitTitle = [@"Quit " stringByAppendingString:appName];
  id quitMenuItem = [[[NSMenuItem alloc] initWithTitle:quitTitle
  action:@selector(terminate:) keyEquivalent:@"q"] autorelease];
  [appMenu addItem:quitMenuItem];
  [appMenuItem setSubmenu:appMenu];

  // dimensions
  int width, height, swidth, sheight, ofsx, ofsy;

  NSRect screenRect;
  NSArray *screenArray = [NSScreen screens];
  unsigned screenCount = [screenArray count];
  NSScreen *screen = [screenArray objectAtIndex: 0];
  screenRect = [screen visibleFrame];
  swidth = screenRect.size.width;
  sheight = screenRect.size.height;

  ffi_event(EVENT_INIT,swidth, sheight);

  width = scm_width();
  height = scm_height(); 
  ofsx=(swidth-width)/2;
  ofsy=(sheight-height)/2;

  // the window
  NSWindow *w = [[NSWindow alloc] initWithContentRect:NSMakeRect(ofsx,ofsy,width,height)
     styleMask:NSTitledWindowMask|NSClosableWindowMask|NSMiniaturizableWindowMask|NSResizableWindowMask
     backing:NSBackingStoreBuffered
     defer:YES];
  NSRect frame = [w contentRectForFrameRect:[w frame]];

  // opengl 
  unsigned int attrs[] = { NSOpenGLPFAAccelerated, 0 };
  NSOpenGLPixelFormat *pixelFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:(NSOpenGLPixelFormatAttribute*)attrs];
  NSOpenGLView  *view = [[SimpleOpenGLView alloc] initWithFrame:frame pixelFormat:pixelFormat];
  [pixelFormat release];

  // window properties
  [w setOpaque:YES];
  [w setContentView:view];
  [w makeFirstResponder:view];
  [w setContentMinSize:NSMakeSize(width,height)];
  [w setContentMaxSize:NSMakeSize(width,height)];
  [w setTitle:appName];
  [w makeKeyAndOrderFront: nil];
  [w setAcceptsMouseMovedEvents:YES];

  // propagate window close event
  NSNotificationCenter  *nc = [NSNotificationCenter defaultCenter];
  [nc addObserver:view selector:@selector(windowWillClose:) name:NSWindowWillCloseNotification object:w];

  [NSApp activateIgnoringOtherApps:YES]; 
  [NSApp run];
  return 0;

}

// eof
