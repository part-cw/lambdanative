#ifdef DEBUG
#define IOS_DEBUG 1
#endif

#ifdef IOS_DEBUG
#define DMSG(fmt...) (NSLog(@"DEBUG: " fmt))
#else
#define DMSG(fmt...)
#endif

#ifndef PrefixHeader_pch
#define PrefixHeader_pch

#ifdef __OBJC__
#import <UIKit/UIKit.h>
#import <Foundation/Foundation.h>
#endif

#import <os/log.h>

#define NSLog(FORMAT, ...) \
  os_log_with_type(OS_LOG_DEFAULT, OS_LOG_TYPE_DEFAULT, "%{public}s", \
                   [[NSString stringWithFormat:(FORMAT), ##__VA_ARGS__] UTF8String])

#endif /* PrefixHeader_pch */