#ifndef __lambdanative_h__
#define __lambdanative_h__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// system.c
char *system_dir(void);
char *system_appdir(void);
char *system_platform(void);
char *system_appname(void);
char *system_appversion(void);
char *system_buildhash(void);
unsigned int system_buildepoch(void);
char *system_cmdargv(int n);
int system_cmdargc();
char system_pathseparator(void);
void system_init();

// hook.c
void lambdanative_exit(int code);

#endif // __lambdanative_h__

