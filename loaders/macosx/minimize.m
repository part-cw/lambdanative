
#import <AppKit/AppKit.h>

void minimize_macosx () {
 NSApplication *application = [NSApplication sharedApplication];
 [application miniaturizeAll:application];
}

