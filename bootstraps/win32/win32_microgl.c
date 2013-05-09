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
// simple win32 GL window interface

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <windows.h>

#include <GL/gl.h>
//#include <GL/glu.h>

#include <CONFIG.h>

static const char microglClassName[]= "MicroglClass";

#define AppIcon 101

static HMODULE microglInstance; 

// window data structure
typedef struct microglWindow {
  HWND Wnd;
  HDC  DC; 
  HGLRC RC;
  int  w,h;
  int full_w, full_h;     // compensated for border decorations
  int  mouse_x, mouse_y; 
} microglWindow;

static microglWindow win;

// translate keyboard input
static int _microgl_key(WPARAM wParam, LPARAM lParam, int action)
{
  BYTE  keyboard_state[ 256 ];
  UCHAR char_buf[ 10 ];
  UINT  scan_code;
  int   num_chars,res;

  switch( wParam )
  {
     case VK_RETURN: return EVENT_KEYENTER;
     case VK_ESCAPE: return EVENT_KEYESCAPE;
     case VK_TAB:    return EVENT_KEYTAB;
     case VK_BACK:   return EVENT_KEYBACKSPACE;
     case VK_LEFT:   return EVENT_KEYLEFT;
     case VK_UP:     return EVENT_KEYUP;
     case VK_RIGHT:  return EVENT_KEYRIGHT;
     case VK_DOWN:   return EVENT_KEYDOWN;
     default:
       GetKeyboardState( keyboard_state );
       scan_code = (lParam & 0x01ff0000) >> 16;
       if( action == EVENT_KEYRELEASE ) { scan_code |= 0x8000000; }
       num_chars = ToAscii( wParam, scan_code,        
            keyboard_state,   (LPWORD) char_buf, 0);                 
       res = (int)char_buf[0];
       if( (res >=  32 && res <= 126) || (res >= 160 && res <= 255) ) { return res; }
       return 0;
  }
}

// window event callback
static LRESULT CALLBACK _microgl_callback( HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam )
{
  static int move_flag=0, new_x, new_y, key;
 // int new_x, new_y;
  // we get callbacks before CreateWindowEx returns!!
  //  if (!win) return DefWindowProc( hWnd, uMsg, wParam, lParam );
  switch( uMsg ) {
    case WM_PAINT:  
      if (move_flag) {  microgl_hook(EVENT_MOTION, new_x, new_y); move_flag=0; }
      microgl_hook(EVENT_REDRAW,0,0); 
      break;   // no return here!
    case WM_DESTROY: 
      PostQuitMessage( WM_QUIT ); return 0;
    case WM_CLOSE: 
      microgl_hook(EVENT_CLOSE,0,0);
      PostQuitMessage( WM_QUIT ); return 0;    // 0?
    case WM_QUIT:
      microgl_hook(EVENT_CLOSE,0,0);
      break; 
    case WM_KEYDOWN:
    case WM_SYSKEYDOWN:
      key = _microgl_key(wParam,lParam,EVENT_KEYPRESS);
      if (key) microgl_hook(EVENT_KEYPRESS, key, 0);
      return 0;
    case WM_KEYUP:
    case WM_SYSKEYUP:
      key = _microgl_key(wParam,lParam,EVENT_KEYRELEASE);
      if (key) microgl_hook(EVENT_KEYRELEASE, key, 0);
      return 0;
    case WM_LBUTTONDOWN:
      win.mouse_x= (int)((short)LOWORD(lParam));
      win.mouse_y= (int)((short)HIWORD(lParam));
      microgl_hook(EVENT_BUTTON1DOWN, win.mouse_x, win.h-win.mouse_y);
      return 0;
    case WM_RBUTTONDOWN:
      win.mouse_x= (int)((short)LOWORD(lParam));
      win.mouse_y= (int)((short)HIWORD(lParam));
      microgl_hook(EVENT_BUTTON2DOWN, win.mouse_x, win.h-win.mouse_y);
      return 0;
    case WM_MBUTTONDOWN:
      win.mouse_x= (int)((short)LOWORD(lParam));
      win.mouse_y= (int)((short)HIWORD(lParam));
      microgl_hook(EVENT_BUTTON3DOWN, win.mouse_x, win.h-win.mouse_y);
      return 0;
    case WM_LBUTTONUP:
      win.mouse_x= (int)((short)LOWORD(lParam));
      win.mouse_y= (int)((short)HIWORD(lParam));
      microgl_hook(EVENT_BUTTON1UP, win.mouse_x, win.h-win.mouse_y);
      return 0;
    case WM_RBUTTONUP:
      win.mouse_x= (int)((short)LOWORD(lParam));
      win.mouse_y= (int)((short)HIWORD(lParam));
      microgl_hook(EVENT_BUTTON2UP, win.mouse_x, win.h-win.mouse_y);
      return 0;
    case WM_MBUTTONUP:
      win.mouse_x= (int)((short)LOWORD(lParam));
      win.mouse_y= (int)((short)HIWORD(lParam));
      microgl_hook(EVENT_BUTTON3UP, win.mouse_x, win.h-win.mouse_y);
      return 0;
    case WM_MOUSEMOVE:
// Dec 2009: we don't use motion events, and they can eat up a lot of resources
// disabled for now
      new_x= (int)((short)LOWORD(lParam));
      new_y= win.h - (int)((short)HIWORD(lParam)) - 1;
      if  ( new_x != win.mouse_x || new_y != win.mouse_y ) {
        win.mouse_x=new_x; win.mouse_y=new_y;
        move_flag=1;
      }
  //      microgl_hook(EVENT_MOTION, new_x, new_y);
  //    }
      return 0;
    case WM_SIZE:
      if ( (int)LOWORD(lParam)!=win.full_w || (int)HIWORD(lParam)!= win.full_h)
        SetWindowPos( win.Wnd, HWND_TOP, 0, 0, win.full_w, win.full_h,
                      SWP_NOOWNERZORDER | SWP_NOMOVE | SWP_NOZORDER );
      return 0;
  }
  // default handler
  return DefWindowProc( hWnd, uMsg, wParam, lParam );
}

// initialization
void microgl_init(void)
{
//  char tmpfile[1024];
  WNDCLASSEX    wc;
  microglInstance  = GetModuleHandle( NULL ); 
  wc.cbSize = sizeof(WNDCLASSEX);
  wc.style         = CS_HREDRAW | CS_VREDRAW | CS_OWNDC; 
  wc.lpfnWndProc   = (WNDPROC)_microgl_callback;  
  wc.cbClsExtra    = 0;                             
  wc.cbWndExtra    = 0;                             
  wc.hInstance     = microglInstance;         
  wc.hCursor       = LoadCursor( NULL, IDC_ARROW ); 

//  wc.hIcon = 0;
//  wc.hIconSm = 0;
//  snprintf(tmpfile,1024,"%s%cbin%cicon.ico",
//      system_directory(), system_pathseparator(),system_pathseparator());
//  wc.hIcon = LoadImage(NULL,tmpfile,IMAGE_ICON, 32, 32, LR_LOADFROMFILE);
//  wc.hIconSm = LoadImage(NULL,tmpfile,IMAGE_ICON, 16,16,LR_LOADFROMFILE);
//  if( !wc.hIcon ) { wc.hIcon = LoadIcon( NULL, IDI_APPLICATION); }
//  if( !wc.hIconSm ) { wc.hIconSm = LoadIcon( NULL, IDI_APPLICATION); }

  // 20100729: we need to fix this icon business...
  // 20100729: note that the app dies mysteriously without the icon???
  wc.hIcon = LoadIcon(microglInstance, MAKEINTRESOURCE(AppIcon));
  wc.hIconSm = LoadImage(microglInstance,MAKEINTRESOURCE(AppIcon),IMAGE_ICON,16,16, LR_DEFAULTCOLOR);

//  wc.hIcon = LoadIcon( NULL, IDI_APPLICATION);
//  wc.hIconSm = LoadIcon( NULL, IDI_APPLICATION);

  wc.lpszClassName = microglClassName;
  wc.hbrBackground = NULL;                          
  wc.lpszMenuName  = NULL;                          
  if (!RegisterClassEx( &wc )) 
    printf("Error: microgl: RegisterClassEx() failed [%i]\n", 
       (int)GetLastError());
}

void microgl_refresh()
{
  InvalidateRect(win.Wnd, NULL, FALSE);
}

void microgl_swapbuffers()
{
  SwapBuffers( win.DC );
}

int microgl_open(int w, int h, int fs)
{
  PIXELFORMATDESCRIPTOR pfd;
  int pf;
  DWORD  dwStyle, dwExStyle;
  RECT rect,wa;
  POINT  pos;

//  printf(">> open %i %i\n",w,h); 
  win.w = w; win.h = h;
  // full screen
  if (fs) {
    DEVMODE dmScreenSettings;			
    memset(&dmScreenSettings,0,sizeof(dmScreenSettings));	
    dmScreenSettings.dmSize=sizeof(dmScreenSettings);
    dmScreenSettings.dmPelsWidth	= w;		
    dmScreenSettings.dmPelsHeight	= h;	
    dmScreenSettings.dmBitsPerPel	= 16;
    dmScreenSettings.dmFields=DM_BITSPERPEL|DM_PELSWIDTH|DM_PELSHEIGHT;
    ChangeDisplaySettings(&dmScreenSettings,CDS_FULLSCREEN);
    dwStyle = WS_POPUP;
    dwExStyle = WS_EX_APPWINDOW;
  } else {
    dwStyle=WS_OVERLAPPEDWINDOW;
    dwExStyle = WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;
  }
  rect.left   = (long)0; rect.right  = (long)w-1;
  rect.top    = (long)0; rect.bottom = (long)h-1;
  AdjustWindowRectEx( &rect, dwStyle, FALSE, dwExStyle );
  win.full_w= rect.right-rect.left+1;
  win.full_h= rect.bottom-rect.top+1;
  SystemParametersInfo( SPI_GETWORKAREA, 0, &wa, 0 );  
  win.Wnd = CreateWindowEx(
        dwExStyle,        // Extended style
        microglClassName,        // Class name
        SYS_APPNAME,        // Window title
        WS_CLIPSIBLINGS | WS_CLIPCHILDREN | dwStyle,          // Defined window style
        0,0,
        win.full_w,                // Decorated window width
        win.full_h,               // Decorated window height
        NULL,                      // No parent window
        NULL,                      // No menu
        microglInstance,     // Instance
        NULL );                    // Nothing to WM_CREATE
  win.DC = GetDC( win.Wnd );
  pfd.nSize           = sizeof(PIXELFORMATDESCRIPTOR);
  pfd.nVersion        = 1;
  pfd.dwFlags         = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
  pfd.iPixelType      = PFD_TYPE_RGBA;       // Request an RGBA format
  pfd.cColorBits      = 16;  // Color bits (ex. alpha)
  pfd.cAlphaBits      = 0;    // Alpha bits
  pfd.cAccumBits      = 0; // Accum. bits ????
  pfd.cDepthBits      = 16; // Depth buffer bits
  pfd.cStencilBits    = 0;
  pfd.cAuxBuffers     = 0;
  pfd.iLayerType      = PFD_MAIN_PLANE;      // Drawing layer: main
  pf=ChoosePixelFormat( win.DC, &pfd);
  if (!pf) {
    printf("ChoosePixelFormat() failed, error code %i\n", 
            (int)GetLastError());
  } else {
    SetPixelFormat(win.DC,pf,&pfd);
  }
  win.RC = wglCreateContext( win.DC );
  wglMakeCurrent( win.DC, win.RC );
  GetCursorPos( &pos );
  ScreenToClient( win.Wnd, &pos );
  win.mouse_x=pos.x; win.mouse_y=pos.y;
  ShowWindow( win.Wnd, SW_SHOW );        // do we need this?
  SetForegroundWindow(win.Wnd);
  SetFocus(win.Wnd);
  glClear( GL_COLOR_BUFFER_BIT );
  microgl_swapbuffers();
  return 1;
}

int microgl_window(int w, int h) { return microgl_open(w,h,0); } 
int microgl_fullscreen(int w, int h) { return microgl_open(w,h,1); } 

// close window
void microgl_close()
{
  wglMakeCurrent( NULL, NULL );
  wglDeleteContext( win.RC);
  ReleaseDC(win.Wnd, win.DC );
  DestroyWindow( win.Wnd );
}

// process events
void microgl_pollevents(void)
{ 
  MSG Msg;
  while( PeekMessage( &Msg, NULL, 0, 0, PM_REMOVE ) ) { DispatchMessage( &Msg ); }
//  if (GetMessage(&Msg, NULL, 0, 0) > 0) {
//    TranslateMessage(&Msg); DispatchMessage(&Msg);
//  }
}

int microgl_screenwidth()
{
  return (int)GetSystemMetrics(SM_CXSCREEN);
}

int microgl_screenheight()
{
  return (int)GetSystemMetrics(SM_CYSCREEN);
}

// eof
