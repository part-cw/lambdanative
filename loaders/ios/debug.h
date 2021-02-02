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

#import <os/object.h>
#import <os/activity.h>

/*
 *  System Versioning Preprocessor Macros
 */

#define SYSTEM_VERSION_EQUAL_TO(v)                  ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] == NSOrderedSame)
#define SYSTEM_VERSION_GREATER_THAN(v)              ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] == NSOrderedDescending)
#define SYSTEM_VERSION_GREATER_THAN_OR_EQUAL_TO(v)  ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] != NSOrderedAscending)
#define SYSTEM_VERSION_LESS_THAN(v)                 ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] == NSOrderedAscending)
#define SYSTEM_VERSION_LESS_THAN_OR_EQUAL_TO(v)     ([[[UIDevice currentDevice] systemVersion] compare:v options:NSNumericSearch] != NSOrderedDescending)

// os_log is only supported when compiling with Xcode 8.
// Check if iOS version > 10 and the _os_log_internal symbol exists,
// load it dynamically and call it.
// Definitions extracted from #import <os/log.h>

#if OS_OBJECT_SWIFT3
OS_OBJECT_DECL_SWIFT(os_log);
#elif OS_OBJECT_USE_OBJC
OS_OBJECT_DECL(os_log);
#else
typedef struct os_log_s *os_log_t;
#endif /* OS_OBJECT_USE_OBJC */

extern struct os_log_s _os_log_default;

extern __attribute__((weak)) void _os_log_internal(void *dso, os_log_t log, int type, const char *message, ...);

// In iOS 10 NSLog only shows in device log when debugging from Xcode:
#define NSLog(FORMAT, ...) \
if (SYSTEM_VERSION_GREATER_THAN_OR_EQUAL_TO(@"10.0")) {\
    void(*ptr_os_log_internal)(void *, __strong os_log_t, int, const char *, ...) = _os_log_internal;\
    if (ptr_os_log_internal != NULL) {\
        _Pragma("clang diagnostic push")\
        _Pragma("clang diagnostic error \"-Wformat\"")\
        _os_log_internal(&__dso_handle, OS_OBJECT_GLOBAL_OBJECT(os_log_t, _os_log_default), 0x00, [[NSString stringWithFormat:FORMAT, ##__VA_ARGS__] UTF8String]);\
        _Pragma("clang diagnostic pop")\
    } else {\
        NSLog(FORMAT, ##__VA_ARGS__);\
    }\
} else {\
    NSLog(FORMAT, ##__VA_ARGS__);\
}

#endif /* PrefixHeader_pch */

