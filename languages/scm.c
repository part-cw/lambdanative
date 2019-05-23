// scheme payload bootstrap 

//#define DEBUG_BOOTSTRAP 1

#ifdef DEBUG_BOOTSTRAP
#define DMSG(fmt...) (fprintf(stderr,"DEBUG_BOOTSTRAP: " fmt),fprintf(stderr,"\n"))
#else
#define DMSG(fmt...)
#endif

/*
 * Maybe we better fist #include <gambit.h> and then check that we've
 * got a known to be good version instead of unconditionally
 * overwriting it?
 */
#define USE_EXPERIMENTAL 0
#if (USE_EXPERIMENTAL = 0)
#define ___VERSION 407009
#else
#define ___VERSION 409002
#fi
#include <gambit.h>
#if (___VERSION < 409002 )
#define LINKER ____20_@SCM_LINKER@
#else
#define LINKER ___LNK_@SCM_LINKER@
#fi
___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE
___setup_params_struct setup_params;
int debug_settings = ___DEBUG_SETTINGS_INITIAL;

void system_init();

void lambdanative_payload_setup()
{
  DMSG("lambdanative_payload_setup [scm]");
  system_init();
  ___setup_params_reset (&setup_params);
  setup_params.version = ___VERSION;
  setup_params.linker = LINKER;
  debug_settings = (debug_settings & ~___DEBUG_SETTINGS_REPL_MASK) |
  (___DEBUG_SETTINGS_REPL_STDIO << ___DEBUG_SETTINGS_REPL_SHIFT);
  setup_params.debug_settings = debug_settings;
  ___setup(&setup_params);
  #if defined(ANDROID)
  #if (___VERSION < 409002 )
  ___disable_heartbeat_interrupts()
  #else
  ___cleanup_heartbeat_interrupt_handling();
  #endif
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
