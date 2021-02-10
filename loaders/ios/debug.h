
#ifdef DEBUG
#define IOS_DEBUG 1
#endif

#ifdef IOS_DEBUG
#define DMSG(fmt...) (NSLog(@"DEBUG: " fmt))
#else
#define DMSG(fmt...)
#endif

