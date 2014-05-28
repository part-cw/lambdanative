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
#import <UIKit/UIKit.h>
#include <LNCONFIG.h>

#import <Foundation/Foundation.h>

#import "launcherAppDelegate.h"

const char *iphone_directory;

// there are two places to save files on a jailed iphone:
// the application support directory and Documents directory
// Documents is preferrable, since it allows file transfer through itunes with iOS 4+
void find_iphone_appsupport_directory() {
  NSString *path = nil;
  NSArray *paths = NSSearchPathForDirectoriesInDomains(
      NSCachesDirectory, NSUserDomainMask, YES);
  if ([paths count])
  {
      NSString *bundleName =
          [[[NSBundle mainBundle] infoDictionary] objectForKey:@"CFBundleIdentifier"];
      path = [[paths objectAtIndex:0] stringByAppendingPathComponent:bundleName];
  }
  iphone_directory=[path UTF8String]; 
}

void find_iphone_documents_directory() {
  NSString *path = nil;
  NSArray *paths = NSSearchPathForDirectoriesInDomains(
      NSDocumentDirectory, NSUserDomainMask, YES);
  if ([paths count]) { path = [paths objectAtIndex:0]; }
  iphone_directory=[path UTF8String];
}

int main(int argc, char *argv[]) {
  NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];
  find_iphone_documents_directory();
//  int retVal = UIApplicationMain(argc, argv, nil, nil);
  int retVal = UIApplicationMain(argc, argv, nil, @"launcherAppDelegate");
  // is this ever reached?
  ffi_event(EVENT_TERMINATE,0,0);
  [pool release];
  return retVal;
}

// eof
