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

#include "CONFIG.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#if defined(OPENBSD)
#include <sys/types.h>
#include <signal.h>
#include <fcntl.h>
#endif

#if defined(WIN32)
#include <windows.h>
#endif

static int run_flag=1;

#ifndef USECONSOLE

#if defined(MAEMO) || defined(MAEMOSIM)
#include <GLES/gl.h>
#include <GLES/egl.h>
#endif

#ifdef MACOSX
#include <OpenGL/OpenGL.h>
#endif

#if defined(LINUX) || defined(OPENBSD) || defined(WIN32)
#include <GL/gl.h>
#endif

// event hook
void microgl_hook(int t, int x, int y)
{
  switch (t) {
    case EVENT_REDRAW: 
      glClearColor(0.0, 0.0, 0.0, 0.0);
      glMatrixMode(GL_PROJECTION);	
      glLoadIdentity();
#if defined(MAEMO)  || defined (MAEMOSIM)
      glOrthof(0.,scm_width(),0.,scm_height(),-1.,1.);
#else
      glOrtho(0.,scm_width(),0.,scm_height(),-1.,1.);
#endif
// 20100322: subtle.. without GL_MODELVIEW maemo rendering is off
      glMatrixMode(GL_MODELVIEW);	
      glLoadIdentity();
      glClear(GL_COLOR_BUFFER_BIT);
      ffi_event(EVENT_REDRAW,0,0);
      microgl_swapbuffers();
      break;
    case EVENT_CLOSE: 
      ffi_event(EVENT_CLOSE,0,0);
      microgl_close();
      run_flag=0;
      break; 
    default:
      ffi_event(t,x,y);
      break; 
  }
}

#endif // USECONSOLE

#ifdef USECONSOLE
char *cmd_arg1=0;
#endif

int main(int argc, char *argv[])
{
  int w=0,h=0;

#ifdef USECONSOLE 
// 20101007: quick hack to get a command line argument through 
  if (argc>1) cmd_arg1=argv[1];
#endif

// fork to release terminal (for starting processes on embedded systems)
#if defined(USECONSOLE) && defined(OPENBSD)
 signal(SIGHUP,SIG_IGN);
 signal(SIGTERM,SIG_IGN);
 signal(SIGCHLD,SIG_IGN);
 if(fork() != 0) return 0;
 chdir("/"); setsid(); umask(0);
 if(fork() != 0) return 0;
 { int fd = open("/tmp/stdin",O_RDONLY|O_CREAT);
   dup2(fd,STDIN_FILENO);
   fd = open("/tmp/stdout",O_WRONLY|O_CREAT);
   dup2(fd,STDOUT_FILENO);
   fd = open("/tmp/stderr",O_WRONLY|O_CREAT);
   dup2(fd,STDERR_FILENO);
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

#ifndef USECONSOLE

  // open a window
  if (w==scm_width()&&h==scm_height()) {
    microgl_fullscreen(w,h);
  } else {
    microgl_window(scm_width(),scm_height());
  }
 
#endif // USECONSOLE

  while (run_flag) { 
    // check for application exit
    if (run_flag) run_flag=scm_runflag();

#ifndef USECONSOLE

    // check for events
    microgl_pollevents();
    // ask for a redraw
    microgl_refresh();

#else 
  
   // generate a fake redraw event
    ffi_event(EVENT_REDRAW,0,0);

#endif // USECONSOLE

#ifndef USECONSOLE
    // sleep 10ms...
#ifdef WIN32
    Sleep(10);  
#else
    usleep(10000);
#endif
#endif // USECONSOLE

   }

  ffi_event(EVENT_TERMINATE,0,0);
  return 1;
}

