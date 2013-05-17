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

#import "config.h"

#import <CoreGraphics/CoreGraphics.h>

#import <QuartzCore/QuartzCore.h>
#import <OpenGLES/EAGLDrawable.h>

#import <UIKit/UIKit.h>

#import "EAGLView.h"

#define USE_DEPTH_BUFFER 0

@interface EAGLView ()

@property (nonatomic, retain) EAGLContext *context;
@property (nonatomic, assign) NSTimer *animationTimer;

- (BOOL) createFramebuffer;
- (void) destroyFramebuffer;

@end

@implementation EAGLView

@synthesize context;
@synthesize animationTimer;
@synthesize animationInterval;
@synthesize label;

+ (Class)layerClass {
	return [CAEAGLLayer class];
}

- (id)initWithFrame:(CGRect)frame {
      if ((self = [super initWithFrame:frame])) {
		CAEAGLLayer *eaglLayer = (CAEAGLLayer *)self.layer;
		eaglLayer.opaque = YES;
		eaglLayer.drawableProperties = [NSDictionary dictionaryWithObjectsAndKeys:
		   [NSNumber numberWithBool:NO], 
                    kEAGLDrawablePropertyRetainedBacking, 
                    kEAGLColorFormatRGBA8, kEAGLDrawablePropertyColorFormat, nil];
		context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES1];
		if (!context || ![EAGLContext setCurrentContext:context]) {
			[self release];
			return nil;
		}
// 20100422: fiddling around with this seems to have no effect on device performance
//	animationInterval = 1.0 / 60.0;
	animationInterval = 1.0 / 20.0;
	}
   
// 20100603: battery hack
  batterydev=[UIDevice currentDevice];
  [batterydev setBatteryMonitoringEnabled:YES];
  batteryidx=0;

#ifdef USE_MULTITOUCH
  [self setMultipleTouchEnabled:YES];
#endif 

  return self;
}

//// Event hooks

// equivalent to button press
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event {
  for( UITouch *t in touches ) {
    CGPoint p= [t locationInView:self];
#ifdef USE_MULTITOUCH
    ffi_event(EVENT_MULTITOUCH, (unsigned int)t,0);
#endif
    ffi_event(EVENT_BUTTON1DOWN, (int)(p.x),scm_screenheight()-(int)(p.y));
  }
}

// dragging motion
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event {
  for( UITouch *t in touches ) {
    CGPoint p= [t locationInView:self];
#ifdef USE_MULTITOUCH
    ffi_event(EVENT_MULTITOUCH, (unsigned int)t,0);
#endif
    ffi_event(EVENT_MOTION, (int)(p.x),scm_screenheight()-(int)(p.y));
  }
}

// equivalent to button release 
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event {
  for( UITouch *t in touches ) {
    CGPoint p= [t locationInView:self];
#ifdef USE_MULTITOUCH
    ffi_event(EVENT_MULTITOUCH, (unsigned int)t,0);
#endif
    ffi_event(EVENT_BUTTON1UP, (int)(p.x),scm_screenheight()-(int)(p.y));
  }
}

// ??
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event {
//    touches_cancelled(touches, event);
}

//// Rendering

- (void)drawView {
    if (render) {		
      NSString *labelText = [[NSString alloc] initWithCString:"" encoding: NSASCIIStringEncoding];
      [self.label setText:labelText];
      [EAGLContext setCurrentContext:context];
      glBindFramebufferOES(GL_FRAMEBUFFER_OES, viewFramebuffer);
      glViewport(0, 0, backingWidth, backingHeight);
    } 

// 20100603: battery update
     if (batteryidx==0) {
       ffi_event(EVENT_BATTERY,(int)(100.*[batterydev batteryLevel]),0);
     }
     if (batteryidx++==100) batteryidx=0;

     ffi_event(EVENT_REDRAW,render,0);

     if (render) {
       glBindRenderbufferOES(GL_RENDERBUFFER_OES, viewRenderbuffer);
      	[context presentRenderbuffer:GL_RENDERBUFFER_OES];
     }
}


- (void)layoutSubviews {
	[EAGLContext setCurrentContext:context];
	[self destroyFramebuffer];
	[self createFramebuffer];
	[self drawView];
}


- (BOOL)createFramebuffer {
	glGenFramebuffersOES(1, &viewFramebuffer);
	glGenRenderbuffersOES(1, &viewRenderbuffer);
	glBindFramebufferOES(GL_FRAMEBUFFER_OES, viewFramebuffer);
	glBindRenderbufferOES(GL_RENDERBUFFER_OES, viewRenderbuffer);
	[context renderbufferStorage:GL_RENDERBUFFER_OES fromDrawable:(CAEAGLLayer*)self.layer];
	glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_COLOR_ATTACHMENT0_OES, GL_RENDERBUFFER_OES, viewRenderbuffer);
	glGetRenderbufferParameterivOES(GL_RENDERBUFFER_OES, GL_RENDERBUFFER_WIDTH_OES, &backingWidth);
	glGetRenderbufferParameterivOES(GL_RENDERBUFFER_OES, GL_RENDERBUFFER_HEIGHT_OES, &backingHeight);
	if (USE_DEPTH_BUFFER) {
		glGenRenderbuffersOES(1, &depthRenderbuffer);
		glBindRenderbufferOES(GL_RENDERBUFFER_OES, depthRenderbuffer);
		glRenderbufferStorageOES(GL_RENDERBUFFER_OES, GL_DEPTH_COMPONENT16_OES, backingWidth, backingHeight);
		glFramebufferRenderbufferOES(GL_FRAMEBUFFER_OES, GL_DEPTH_ATTACHMENT_OES, GL_RENDERBUFFER_OES, depthRenderbuffer);
	}
	if(glCheckFramebufferStatusOES(GL_FRAMEBUFFER_OES) != GL_FRAMEBUFFER_COMPLETE_OES) {
		NSLog(@"failed to make complete framebuffer object %x", glCheckFramebufferStatusOES(GL_FRAMEBUFFER_OES));
		return NO;
	}
	return YES;
}


- (void)destroyFramebuffer {
	
	glDeleteFramebuffersOES(1, &viewFramebuffer);
	viewFramebuffer = 0;
	glDeleteRenderbuffersOES(1, &viewRenderbuffer);
	viewRenderbuffer = 0;
	if(depthRenderbuffer) {
		glDeleteRenderbuffersOES(1, &depthRenderbuffer);
		depthRenderbuffer = 0;
	}
}


- (void)startRender {
  render=1;
}

- (void)stopRender {
  render=0;
}

- (void)startAnimation {
	self.animationTimer = [NSTimer scheduledTimerWithTimeInterval:animationInterval target:self selector:@selector(drawView) userInfo:nil repeats:YES];
	render=1;
}


- (void)stopAnimation {
	self.animationTimer = nil;
}


- (void)setAnimationTimer:(NSTimer *)newTimer {
	[animationTimer invalidate];
	animationTimer = newTimer;
}


- (void)setAnimationInterval:(NSTimeInterval)interval {
	animationInterval = interval;
	if (animationTimer) {
		[self stopAnimation];
		[self startAnimation];
	}
}

- (void)dealloc {
	[self stopAnimation];
	if ([EAGLContext currentContext] == context) {
		[EAGLContext setCurrentContext:nil];
	}
	[context release];	
	[super dealloc];
}

@end
