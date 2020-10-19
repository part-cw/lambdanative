// lambdanative hook

#include <stdio.h>
#include <stdlib.h>

#include <LNCONFIG.h>

// ---------------
// lambdanative payload bootstrap

#ifndef PAYLOADONLY

#include <lambdanative.h>

void lambdanative_payload_setup();
void lambdanative_payload_cleanup();
void lambdanative_payload_event(int,int,int);

void lambdanative_exit(int code)
{
  lambdanative_payload_cleanup();
  exit(code);
}
#ifdef STANDALONE
// standalone setup
char **cmd_argv;
int cmd_argc=0;
int main(int argc, char *argv[])
{
  cmd_argc=argc; cmd_argv=argv;
  lambdanative_payload_setup();
  lambdanative_payload_cleanup();
  return 0;
}
#else
// event loop setup
#if defined(ANDROID) || defined(MACOSX) || defined(IOS) || defined(LINUX) || defined(OPENBSD) || defined(BB10) || defined(PLAYBOOK) || defined(NETBSD)
  #include <pthread.h>
  pthread_mutex_t ffi_event_lock;
  #define FFI_EVENT_INIT  pthread_mutex_init(&ffi_event_lock, 0);
  #define FFI_EVENT_LOCK  pthread_mutex_lock( &ffi_event_lock);
  #define FFI_EVENT_UNLOCK  pthread_mutex_unlock( &ffi_event_lock);
#else
  #ifdef WIN32
    #include <windows.h>
    CRITICAL_SECTION ffi_event_cs;
    #define FFI_EVENT_INIT  InitializeCriticalSection(&ffi_event_cs);
    #define FFI_EVENT_LOCK  EnterCriticalSection(&ffi_event_cs);
    #define FFI_EVENT_UNLOCK LeaveCriticalSection( &ffi_event_cs);
  #else
    static int ffi_event_lock;
    #define FFI_EVENT_INIT ffi_event_lock=0;
    #define FFI_EVENT_LOCK { while (ffi_event_lock) { }; ffi_event_lock=1; }
    #define FFI_EVENT_UNLOCK  ffi_event_lock=0;
  #endif
#endif
void ffi_event(int t, int x, int y)
{
  static int lambdanative_needsinit=1;
  if (lambdanative_needsinit) {
      FFI_EVENT_INIT
      lambdanative_needsinit=0;
      lambdanative_payload_setup();
  } 
  FFI_EVENT_LOCK
  if (!lambdanative_needsinit&&t) lambdanative_payload_event(t,x,y);
  if (t==EVENT_TERMINATE) { lambdanative_payload_cleanup(); exit(0); }
  FFI_EVENT_UNLOCK
}
#endif // STANDALONE

#endif // PAYLOADONLY

// eof
