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
// simple cocoa wrapper

#import "SimpleOpenGLView.h"

#import <OpenGL/gl.h>
#import <OpenGL/glu.h>

#include "LNCONFIG.h"

@implementation SimpleOpenGLView

- (void)drawRect:(NSRect)aRect
{
  static int needsinit=1;
  if (needsinit) {
    timer = [NSTimer timerWithTimeInterval: (1.0f/20.0f) target:self selector:@selector(animTimer:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:timer forMode:NSDefaultRunLoopMode];
    [[NSRunLoop currentRunLoop] addTimer:timer forMode:NSEventTrackingRunLoopMode];
    needsinit=0;
  } 
  ffi_event(EVENT_REDRAW,0,0);
  glFlush();
}

+ (NSOpenGLPixelFormat*) basicPixelFormat
{
    NSOpenGLPixelFormatAttribute attributes [] = {
        NSOpenGLPFAWindow,
        NSOpenGLPFADoubleBuffer,
        NSOpenGLPFADepthSize, (NSOpenGLPixelFormatAttribute)16,
        (NSOpenGLPixelFormatAttribute)nil
    };
    return [[[NSOpenGLPixelFormat alloc] initWithAttributes:attributes] autorelease];
}

-(id) initWithFrame : (NSRect) frameRect
{
 NSOpenGLPixelFormat * pf = [SimpleOpenGLView basicPixelFormat];
 return self = [super initWithFrame: frameRect pixelFormat: pf];
}

-(void) awakeFromNib
{
}

-(void) animTimer : (NSTimer *) atime
{
  [self setNeedsDisplay: YES];
  [self drawRect:[self bounds]];
}

- (void) mouseDragged:(NSEvent *)theEvent
{
  NSPoint location=[self convertPoint:[theEvent locationInWindow] fromView:nil];
  ffi_event(EVENT_MOTION,location.x, location.y);
}

- (void) mouseDown:(NSEvent *)theEvent { 
  NSPoint location=[self convertPoint:[theEvent locationInWindow] fromView:nil];
  ffi_event(EVENT_BUTTON1DOWN,location.x,location.y);
}
 
- (void) mouseUp:(NSEvent *)theEvent { 
  NSPoint location=[self convertPoint:[theEvent locationInWindow] fromView:nil];
  ffi_event(EVENT_BUTTON1UP,location.x,location.y);
} 

- (BOOL) acceptsFirstResponder
{
  return (YES);
}

- (void)windowWillClose:(NSNotification *)notification
{
  ffi_event(EVENT_CLOSE,0,0);
  ffi_event(EVENT_TERMINATE,0,0);
}

- (void)keyDown:(NSEvent*)event
{ 
  const char *chars = [[event characters] UTF8String];
  int acode = (int)chars[0];
  if (acode>31&&acode<127) {
    ffi_event(EVENT_KEYPRESS,acode,0);
  } else {
    int ecode=0;
    int kcode = [event keyCode];
    switch (kcode) {
     case 126: ecode = EVENT_KEYUP; break;
     case 125: ecode = EVENT_KEYDOWN; break;
     case 124: ecode = EVENT_KEYRIGHT; break;
     case 123: ecode = EVENT_KEYLEFT; break;
    }
    switch (acode) {
     case 27: ecode = EVENT_KEYESCAPE; break;
     case 9: ecode = EVENT_KEYTAB; break;
     case 13: ecode = EVENT_KEYENTER; break;
     case 127: ecode = EVENT_KEYBACKSPACE; break;
    }
    if (ecode) ffi_event(EVENT_KEYPRESS,ecode,0);
  }
}

- (void)keyUp:(NSEvent*)event
{ 
  const char *chars = [[event characters] UTF8String];
  int acode = (int)chars[0];
  if (acode>31&&acode<127) {
    ffi_event(EVENT_KEYRELEASE,acode,0);
  } else {
    int ecode=0;
    int kcode = [event keyCode];
    switch (kcode) {
     case 126: ecode = EVENT_KEYUP; break;
     case 125: ecode = EVENT_KEYDOWN; break;
     case 124: ecode = EVENT_KEYRIGHT; break;
     case 123: ecode = EVENT_KEYLEFT; break;
    }
    switch (acode) {
     case 27: ecode = EVENT_KEYESCAPE; break;
     case 9: ecode = EVENT_KEYTAB; break;
     case 13: ecode = EVENT_KEYENTER; break;
     case 127: ecode = EVENT_KEYBACKSPACE; break;
    }
    if (ecode) ffi_event(EVENT_KEYRELEASE,ecode,0);
  }
}

@end
