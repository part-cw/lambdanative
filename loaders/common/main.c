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
// microgl based main loop

#include "LNCONFIG.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD)
#include <sys/types.h>
#include <fcntl.h>
#include <limits.h>
#endif

#if defined (OPENBSD) || defined (LINUX) || defined (WIN32) || defined(NETBSD) || defined(FREEBSD)
#include <signal.h>
#endif

#if defined(WIN32)
#include <windows.h>
#endif

static int run_flag=1;
static int int_flag=0;

// signal handler
void signal_hook(int sig)
{
  run_flag=0;
  int_flag=1;
}

#ifndef USECONSOLE

#ifdef MACOSX
#include <OpenGL/OpenGL.h>
#endif

#if defined(LINUX) || defined(OPENBSD) || defined(WIN32) || defined(NETBSD) || defined(FREEBSD)
#include <GL/gl.h>
#endif

// event hook
void microgl_hook(int t, int x, int y)
{
  switch (t) {
    case EVENT_CLOSE:
      ffi_event(EVENT_CLOSE,0,0);
      run_flag=0;
      break;
    default:
      ffi_event(t,x,y);
      break;
  }
}

#endif // USECONSOLE

int cmd_argc=0;
char **cmd_argv;

int main(int argc, char *argv[])
{
  int w=0,h=0;

  cmd_argc=argc; cmd_argv=argv;

// fork to release terminal (for starting processes on embedded systems)
#if defined(USECONSOLE) && defined(OPENBSD)
 signal(SIGHUP, SIG_IGN);
 signal(SIGTERM, SIG_IGN);
 signal(SIGCHLD, SIG_IGN);
 if(fork() != 0) return 0;
 chdir("/"); setsid(); umask(0);
 if(fork() != 0) return 0;
 { int fd = open("/tmp/stdin", O_RDONLY|O_CREAT);
   dup2(fd, STDIN_FILENO);
   fd = open("/tmp/stdout", O_WRONLY|O_CREAT);
   dup2(fd, STDOUT_FILENO);
   fd = open("/tmp/stderr", O_WRONLY|O_CREAT);
   dup2(fd, STDERR_FILENO);
   close(fd);
 }
#endif

#ifndef USECONSOLE

  // opengl preparations
  microgl_init();

  // determine width and height of display
  w=microgl_screenwidth(); h=microgl_screenheight();

#endif // USECONSOLE

  // allow payload to initialize
  ffi_event(EVENT_INIT,w,h);

  // handle signals
  #ifndef WIN32
    signal(SIGHUP,signal_hook);
  #endif
  signal(SIGTERM,signal_hook);
  signal(SIGINT,signal_hook);

  while (run_flag) {
    // check for application exit
    if (run_flag) run_flag=scm_runflag();

#ifndef USECONSOLE

    // check for events
    microgl_pollevents();

#else

   // generate a fake redraw event
    ffi_event(EVENT_REDRAW,0,0);

#endif // USECONSOLE

   }

  if (int_flag) ffi_event(EVENT_CLOSE,0,0);
  ffi_event(EVENT_TERMINATE,0,0);

#ifndef USECONSOLE
  microgl_close();
#endif

  return 1;
}
