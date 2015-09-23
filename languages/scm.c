// scheme payload bootstrap 

//#define DEBUG_BOOTSTRAP 1

#ifdef DEBUG_BOOTSTRAP
#define DMSG(fmt...) (fprintf(stderr,"DEBUG_BOOTSTRAP: " fmt),fprintf(stderr,"\n"))
#else
#define DMSG(fmt...)
#endif

#define ___VERSION 407009
#include <gambit.h>
#define LINKER ____20_@SCM_LINKER@
___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE
___setup_params_struct setup_params;
int debug_settings = ___DEBUG_SETTINGS_INITIAL;

void lambdanative_payload_setup(char *path)
{
  DMSG("lambdanative_payload_setup [scm]");
  ___setup_params_reset (&setup_params);
  setup_params.version = ___VERSION;
  setup_params.linker = LINKER;
  debug_settings = (debug_settings & ~___DEBUG_SETTINGS_REPL_MASK) |
  (___DEBUG_SETTINGS_REPL_STDIO << ___DEBUG_SETTINGS_REPL_SHIFT);
  setup_params.debug_settings = debug_settings;
  ___setup(&setup_params);
  #if defined(ANDROID)
  ___disable_heartbeat_interrupts();
  #endif
}

void lambdanative_payload_cleanup()
{
  DMSG("lambdanative_payload_cleanup [scm]");
  ___cleanup();
}

#ifndef STANDALONE

void scm_event(int,int,int);

void lambdanative_payload_event(int t, int x, int y)
{
  scm_event(t,x,y);
}
#endif

// eof
