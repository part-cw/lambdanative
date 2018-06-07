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
// simple X11 loader
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#if defined(OPENBSD)||defined(LINUX)||defined(NETBSD) || defined(FREEBSD)
#define USE_GLX 1
#endif

#ifdef USE_GLX
#include <GL/glx.h>
#include <GL/gl.h>
//#include <GL/glu.h>
#endif

#ifdef USE_EGL
//#include <GLES2/gl2.h>
#include <GLES/gl.h>
#include <GLES/egl.h>
#endif

#include <LNCONFIG.h>

static Display *Dpy=0;
static int Scrn=0;

// window data structure
typedef struct microglWindow {
  Window Win;
#ifdef USE_GLX
  GLXContext CX;
  XVisualInfo *VI;
#endif
#ifdef USE_EGL
  EGLDisplay egl_display;
  EGLContext egl_context;
  EGLSurface egl_surface;
#endif
  Atom WMDeleteWindow;
  int Scrn;
  int  w,h;
  int  mouse_x, mouse_y; 
  int KeyboardGrabbed, PointerGrabbed;
} microglWindow;

static microglWindow win;

void microgl_swapbuffers()
{
#ifdef USE_GLX
  glXSwapBuffers( Dpy, win.Win );
#endif
#ifdef USE_EGL
  eglSwapBuffers(win.egl_display,win.egl_surface);
#endif
}

void microgl_refresh()
{
  XEvent event;
  event.type       = Expose;
  event.xany.window     = win.Win;
  XSendEvent(Dpy, win.Win, False, Expose, &event);
}

int _microgl_key( XKeyEvent *event )
{
  KeySym keysym;
  XLookupString( event, NULL, 0, &keysym, NULL );
  switch (keysym) {
    case XK_Delete:       return EVENT_KEYDELETE;
    case XK_Escape:       return EVENT_KEYESCAPE;
    case XK_Tab:          return EVENT_KEYTAB;
    case XK_BackSpace:    return EVENT_KEYBACKSPACE;
    case XK_Return:       return EVENT_KEYENTER;
    case XK_Home:         return EVENT_KEYHOME;
    case XK_End:          return EVENT_KEYEND;
    case XK_KP_Left:
    case XK_Left:         return EVENT_KEYLEFT;
    case XK_KP_Right:
    case XK_Right:        return EVENT_KEYRIGHT;
    case XK_KP_Down:
    case XK_Down:         return EVENT_KEYDOWN;
    case XK_KP_Up:
    case XK_Up:           return EVENT_KEYUP;
  }
  if( (keysym >= 0x0020 && keysym <= 0x007e) || 
      (keysym >= 0x00a0 && keysym <= 0x00ff) ) return keysym;
  return 0;
}

void microgl_pollevents(void)
{
  XEvent event;
  int expose=0;
  int motion=0;

  while( XPending( Dpy ) ) {
    XNextEvent( Dpy, &event );
    switch( event.type ) {
      case KeyPress:
        microgl_hook(EVENT_KEYPRESS, _microgl_key( &event.xkey),0);
        break;
      case KeyRelease:
        microgl_hook(EVENT_KEYRELEASE, _microgl_key( &event.xkey),0);
        break;
      case ButtonPress:
        switch (event.xbutton.button) {
          case Button1:
           win.mouse_x = event.xbutton.x; 
           win.mouse_y = win.h - event.xbutton.y - 1;
           microgl_hook(EVENT_BUTTON1DOWN,win.mouse_x, win.mouse_y); 
           break;
          case Button2:
           win.mouse_x = event.xbutton.x; 
           win.mouse_y = win.h - event.xbutton.y - 1;
           microgl_hook(EVENT_BUTTON2DOWN, win.mouse_x, win.mouse_y); 
             break;
          case Button3:
           win.mouse_x = event.xbutton.x; 
           win.mouse_y = win.h - event.xbutton.y - 1;
           microgl_hook(EVENT_BUTTON3DOWN, win.mouse_x, win.mouse_y); 
             break;
        }
        break;
      case ButtonRelease:
        switch (event.xbutton.button) {
          case Button1:
           win.mouse_x = event.xbutton.x; 
           win.mouse_y = win.h - event.xbutton.y - 1;
           microgl_hook(EVENT_BUTTON1UP, win.mouse_x, win.mouse_y); 
           break;
          case Button2:
           win.mouse_x = event.xbutton.x; 
           win.mouse_y = win.h - event.xbutton.y - 1;
           microgl_hook(EVENT_BUTTON2UP, win.mouse_x, win.mouse_y); 
           break;
          case Button3:
           win.mouse_x = event.xbutton.x; 
           win.mouse_y = win.h - event.xbutton.y - 1;
           microgl_hook(EVENT_BUTTON3UP, win.mouse_x, win.mouse_y); 
           break;
        }
        break;
      case MotionNotify:
        win.mouse_x = event.xbutton.x;
        win.mouse_y = win.h - event.xbutton.y - 1;
        motion=1;
        break;
      case Expose:
        expose=1;
        break;
      case ClientMessage:  
        if( (Atom) event.xclient.data.l[ 0 ] == win.WMDeleteWindow )
        microgl_hook(EVENT_CLOSE,0,0); 
        return; 
      case DestroyNotify:
        microgl_hook(EVENT_CLOSE,0,0); 
        return;
      case ConfigureNotify:
        if( event.xconfigure.width != win.w || event.xconfigure.height != win.h )
          XResizeWindow( Dpy, win.Win, win.w, win.h);
        break;
    } 
  }  // Xpending

  if (expose) {  // process expose events
     microgl_hook(EVENT_REDRAW,0,0); 
  }

  if (motion) { // process motion events
    microgl_hook(EVENT_MOTION, win.mouse_x, win.mouse_y);
  }

}


Bool _microglWaitForMapNotify( Display *d, XEvent *e, char *arg )
{
  return (e->type == MapNotify) && (e->xmap.window == (Window)arg);
}

int microgl_open(int w, int h, int fs)
{
  XEvent event;
  XSetWindowAttributes wa;
//  Colormap    cmap;

  win.w = w; win.h = h;

// step 1: create the window
  wa.event_mask=ExposureMask|KeyPressMask|KeyReleaseMask|ButtonPressMask|ButtonReleaseMask;
  if (!fs) wa.event_mask|= PointerMotionMask | StructureNotifyMask| ExposureMask | FocusChangeMask | VisibilityChangeMask; 
  win.Win=XCreateWindow(Dpy,DefaultRootWindow(Dpy),
    0,0,w,h,0,CopyFromParent,InputOutput,
    CopyFromParent,CWEventMask,&wa);

// step 2: fullscreen tweaks
    if (fs) { 
      int success=0;
      Atom atom;

/*  window manager specific, leave out for now
      atom=XInternAtom(Dpy,"_MOTIF_WM_HINTS",True);
      if (atom!=None) {
        struct {
            unsigned long flags;
            unsigned long functions;
            unsigned long decorations;
                     long input_mode;
            unsigned long status;
        } MWMHints = { MWM_HINTS_DECORATIONS, 0, 0, 0, 0 };
        XChangeProperty( Dpy, win.Win, atom, atom, 32, 
          PropModeReplace, (unsigned char *)&MWMHints, sizeof(MWMHints)/4 );
        sucess=1;
      }

      atom = XInternAtom( Dpy, "KWM_WIN_DECORATION", True );
      if ( atom!= None ) {
        long KWMHints = KDE_tinyDecoration;
        XChangeProperty( Dpy, win.Win, atom, atom, 32, 
            PropModeReplace, (unsigned char *)&KWMHints, sizeof(KWMHints)/4 );
        success= 1;
      }
*/

      atom= XInternAtom(Dpy,"_WIN_HINTS",True);
      if ( atom != None ) {
        long GNOMEHints = 0;
        XChangeProperty( Dpy, win.Win, atom, atom, 32, 
          PropModeReplace, (unsigned char *)&GNOMEHints, sizeof(GNOMEHints)/4 );
        success = 1;
      }
 
      atom = XInternAtom( Dpy, "_NET_WM_WINDOW_TYPE", True );
      if ( atom != None ) {
        Atom NET_WMHints[2];
        NET_WMHints[0] = XInternAtom( Dpy, "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE", True );
        NET_WMHints[1] = XInternAtom( Dpy, "_NET_WM_WINDOW_TYPE_NORMAL", True );
        XChangeProperty( Dpy, win.Win, atom, XA_ATOM, 32, 
          PropModeReplace, (unsigned char *)&NET_WMHints, 2 );
        success = 1;
      }

      atom=XInternAtom(Dpy,"_NET_WM_STATE",True); 
      if (atom!=None) {
        Atom NET_WMHints[1];
        NET_WMHints[0] = XInternAtom( Dpy, "_NET_WM_STATE_FULLSCREEN",True);
        XChangeProperty(Dpy,win.Win,atom, XA_ATOM, 32, 
          PropModeReplace, (unsigned char*)&NET_WMHints, 1);
        success=1;
      }

      atom=XInternAtom(Dpy,"_HILDON_NON_COMPOSITED_WINDOW",True);
      if (atom!=None) {
        int one=1;
        XChangeProperty(Dpy,win.Win,atom, XA_INTEGER, 32, 
          PropModeReplace, (unsigned char*)&one, 1); 
        success=1;
      }

      // Hildon run in landscape mode by default
      // this is opposite of other mobiles !
/* 
     atom=XInternAtom(Dpy,"_HILDON_PORTRAIT_MODE_SUPPORT",True);
      if (atom!=None) {
        long one=1;
        XChangeProperty(Dpy,win.Win,atom, XA_CARDINAL, 32,
          PropModeReplace, (unsigned char*)&one, 1);
      }     
      atom=XInternAtom(Dpy,"_HILDON_PORTRAIT_MODE_REQUEST",True);
      if (atom!=None) {                                          
        long one=1;
        XChangeProperty(Dpy,win.Win,atom, XA_CARDINAL, 32,
          PropModeReplace, (unsigned char*)&one, 1);
      }  
*/

      if (success) {
        XSetTransientForHint(Dpy,win.Win,DefaultRootWindow(Dpy));
        XUnmapWindow(Dpy,win.Win);
        XMapWindow(Dpy,win.Win);
      }

      {
        XSetWindowAttributes xattr;
        xattr.override_redirect=True;
        XChangeWindowAttributes(Dpy,win.Win,CWOverrideRedirect,&xattr);
      }

    }

// step 3: glx / egl setup
#ifdef USE_GLX
  {
    GLint att16[] = { GLX_RGBA, GLX_DEPTH_SIZE, 16, GLX_DOUBLEBUFFER, None };
    GLint att24[] = { GLX_RGBA, GLX_DEPTH_SIZE, 24, GLX_DOUBLEBUFFER, None };
    //XXXX Scrn is not initialized!!!
    win.VI = glXChooseVisual(Dpy, win.Scrn, att16);
    if (win.VI == NULL) win.VI = glXChooseVisual(Dpy, win.Scrn, att24);
    if (win.VI == NULL) return 0; // failed to find a visual
    win.CX = glXCreateContext( Dpy, win.VI, 0, GL_TRUE );
    glXMakeCurrent( Dpy, win.Win, win.CX );
  }
#endif

#ifdef USE_EGL
  { 
    EGLConfig ecfg;
    EGLint num_config;
 //   EGLint attr[]={EGL_BUFFER_SIZE,16,EGL_RENDERABLE_TYPE,EGL_OPENGL_ES2_BIT, EGL_NONE};
    EGLint attr[]={EGL_BUFFER_SIZE,16,EGL_RENDERABLE_TYPE,EGL_OPENGL_ES_BIT, EGL_NONE};
 //   EGLint ctxattr[]={EGL_CONTEXT_CLIENT_VERSION,2,EGL_NONE};
    EGLint ctxattr[]={EGL_CONTEXT_CLIENT_VERSION,1,EGL_NONE};
    win.egl_display=eglGetDisplay((EGLNativeDisplayType) Dpy);
    if ( win.egl_display == EGL_NO_DISPLAY ) { printf("ERROR 1\n"); return 0; }
    if (!eglInitialize(win.egl_display,NULL,NULL)) { printf("ERROR 2\n"); return 0; }
    if (!eglChooseConfig(win.egl_display,attr,&ecfg,1,&num_config)) { printf("ERROR 3\n"); return 0; }
    if (num_config!=1) { printf("ERROR 4\n"); return 0; }
    win.egl_surface=eglCreateWindowSurface(win.egl_display,ecfg,(void*)win.Win,NULL);
    if (win.egl_surface==EGL_NO_SURFACE) { printf("ERROR 5\n"); return 0; }
    win.egl_context=eglCreateContext(win.egl_display, ecfg, EGL_NO_CONTEXT, ctxattr);
    if (win.egl_context==EGL_NO_CONTEXT) { printf("ERROR 6\n"); return 0; }
    eglMakeCurrent(win.egl_display,win.egl_surface,win.egl_surface,win.egl_context); 
  }
#endif

  win.WMDeleteWindow = XInternAtom( Dpy, "WM_DELETE_WINDOW", False );
  XSetWMProtocols( Dpy, win.Win, &(win.WMDeleteWindow),1);

// show the window
  XMapWindow( Dpy, win.Win );
  if (!fs)  XIfEvent( Dpy, &event, _microglWaitForMapNotify, (char*)win.Win );
  XRaiseWindow( Dpy, win.Win );  // is this needed??

  win.KeyboardGrabbed = win.PointerGrabbed = 0;
  if (fs) {
    // fullscreen specific stuff
    XMoveWindow(Dpy,win.Win,0,0);
    XResizeWindow(Dpy,win.Win,w,h);
    if (XGrabKeyboard(Dpy, win.Win, True,
      GrabModeAsync, GrabModeAsync, CurrentTime ) == GrabSuccess)
      win.KeyboardGrabbed = 1;
    if (XGrabPointer( Dpy, win.Win, True,
      ButtonPressMask | ButtonReleaseMask |
      PointerMotionMask, GrabModeAsync, GrabModeAsync,
      win.Win, None, CurrentTime ) == GrabSuccess)
      win.PointerGrabbed = 1;
    XWarpPointer(Dpy,None,win.Win,0,0,0,0,0,0);
    XWarpPointer(Dpy,None,win.Win,0,0,0,0,w/2,h/2);
  }

  XStoreName( Dpy, win.Win, SYS_APPNAME);
  XSetIconName( Dpy, win.Win, SYS_APPNAME);

// clear garbage fast?
  glViewport(0,0,w,h);
  glClearColor(0,0,0,0);
  glClear( GL_COLOR_BUFFER_BIT );
  microgl_swapbuffers();
 
  return 1;
}

int microgl_window(int w, int h) { return microgl_open(w,h,0); }
int microgl_fullscreen(int w, int h) { return microgl_open(w,h,1); }

void microgl_close()
{
#ifdef USE_GLX
  glXMakeCurrent( Dpy, None, NULL );
  glXDestroyContext( Dpy, win.CX );
#endif
#ifdef USE_EGL
  eglDestroyContext(win.egl_display, win.egl_context);
  eglDestroySurface(win.egl_display, win.egl_surface);
  eglTerminate(win.egl_display);
#endif
  if (win.KeyboardGrabbed) {
    XUngrabKeyboard(Dpy,CurrentTime);
  } 
  if (win.PointerGrabbed) {
    XUngrabPointer(Dpy, CurrentTime);
  }
  XUnmapWindow( Dpy, win.Win );
  XDestroyWindow( Dpy, win.Win );
  XCloseDisplay(Dpy);
}

static int screen_width, screen_height;

void microgl_init(void)
{
  XWindowAttributes gwa;
  Dpy = XOpenDisplay(0);
  XGetWindowAttributes(Dpy,DefaultRootWindow(Dpy),&gwa);
  screen_width=gwa.width;
  screen_height=gwa.height;
}

int microgl_screenwidth(void) { return screen_width; }
int microgl_screenheight() { return screen_height; }

Display* microgl_getDisplay(){ return Dpy; }
Window microgl_getWindow(){ return win.Win; }
// eof
