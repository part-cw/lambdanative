/*
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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

// lambdanative system configuration

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(FREEBSD) || defined(NETBSD)
#include <sys/param.h>
#endif

#ifdef WIN32
#include <windows.h>
#endif

#include "LNCONFIG.h"

#if defined(MACOSX) || defined(IOS)
#include <CoreFoundation/CoreFoundation.h>
#endif

#ifndef USECONSOLE

#if  defined(IOS)
extern char* iphone_directory;
#endif

#endif // !USECONSOLE

#ifdef USECONSOLE
extern 
#endif
char **cmd_argv;

#ifdef USECONSOLE
extern 
#endif
int cmd_argc;

static char *sys_appdir=0;
static char *sys_dir=0;

static void find_directories()
{
#if defined(LINUX)
  char buf[1024];
  int len=readlink("/proc/self/exe", buf, 1024);
  if (len>0) {
    int i;
    for (i=len-1;i>0;i--){
      if (buf[i]=='/') {
        buf[i]='\0';
        break;
      }
    }
    sys_appdir=strdup(buf);
    sys_dir=strdup(buf);
  }
#endif
#if defined(WIN32)
  char buf[1024];
  int len=GetModuleFileName(NULL,buf,1024);
  if (len>0) {
    int i;
    for (i=len-1;i>0;i--){
      if (buf[i]=='\\'){
        buf[i]='\0';
        break;
      }
    }
    sys_appdir=strdup(buf);
    sys_dir=strdup(buf);
  }
#endif
#if defined(OPENBSD) || defined(NETBSD) || defined(FREEBSD)
  char buf[PATH_MAX];
  if (realpath(cmd_argv[0],buf)) {
    int i = strlen(buf)-1;
    while (i>0&&buf[i]!='/') {i--;}
    if (i>0) buf[i]=0;
    // check if directory exists?
    sys_appdir=strdup(buf);
    sys_dir=strdup(buf);
  }
#endif
#if defined(IOS) || defined(MACOSX)
  char path[1024];
  CFBundleRef mainBundle = CFBundleGetMainBundle();
  CFURLRef mainBundleURL = CFBundleCopyBundleURL( mainBundle);
  CFStringRef cfStringRef = CFURLCopyFileSystemPath( mainBundleURL, kCFURLPOSIXPathStyle);
  CFStringGetCString( cfStringRef, path, 1024, kCFStringEncodingASCII);
  CFRelease( mainBundleURL);
  CFRelease( cfStringRef);
  sys_appdir=strdup(path);
#ifdef IOS
  // check for jail break
  FILE *fd = fopen("/var/mobile/tmp.341231","a");
  if (fd) {
    fclose(fd);
    remove("/var/mobile/tmp.341231");
    sys_dir= strdup(path);
  } else {
    // point to the folder that is shared through iTunes
    sys_dir= strdup(iphone_directory);
  }
#else
  sys_dir=strdup(path);
#endif
#endif
#if defined(ANDROID)
// we put files on the sdcard, that's the only sane place (?)
  char path[1024];
  sprintf(path,"/sdcard/%s", SYS_APPNAME);
  sys_appdir=strdup(path);
  sys_dir=strdup(path);
#endif
#if defined(BB10) || defined(PLAYBOOK)
  char path[1024], cwd[1024];
  getcwd(cwd,1023);
  sprintf(path,"%s/app/native", cwd);
  sys_appdir=strdup(path);
  sprintf(path,"%s/data", cwd);
  sys_dir=strdup(path);
#endif
}

static unsigned int sys_buildepoch;
static char *sys_platform, *sys_appname, *sys_appversion, *sys_cmdarg;
static char *sys_buildhash;
static char *sys_repository, *sys_repositorydate;

void system_init(void)
{
  find_directories();
  sys_platform=strdup(SYS_PLATFORM);
  sys_appname=strdup(SYS_APPNAME);
  sys_appversion=strdup(SYS_APPVERSION);
  sys_buildhash=strdup(SYS_BUILDHASH);
  sys_buildepoch=SYS_BUILDEPOCH;
}

// report the path separator
#ifdef WIN32
#define PATH_SEPARATOR '\\'
#else
#define PATH_SEPARATOR '/'
#endif
char system_pathseparator(void) { return PATH_SEPARATOR; }

// report the system info
char *system_dir(void) { return sys_dir; }
char *system_appdir(void) { return sys_appdir; }
char *system_platform(void) { return sys_platform; }
char *system_appname(void) { return sys_appname; }
char *system_appversion(void) { return sys_appversion; }
char *system_buildhash(void) { return sys_buildhash; }
unsigned int system_buildepoch(void) { return sys_buildepoch; }

char *system_cmdargv(int n) { char *res=0; if (n<cmd_argc) res=cmd_argv[n]; return res; }
int system_cmdargc() { return cmd_argc; }

// eof
