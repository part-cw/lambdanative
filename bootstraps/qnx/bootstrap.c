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

#include <stdio.h>

#include <bps/screen.h>
#include <bps/bps.h>
#include <bps/event.h>
#include <bps/virtualkeyboard.h>

#include <GLES/gl.h>
#include <glview/glview.h>

#include <screen/screen.h>
#include <sys/keycodes.h>

#include <CONFIG.h>

static void init_cb(void *p)
{
// virtualkeyboard_request_events(0);
  int screen_height, screen_width;
  glview_get_size(&screen_width, &screen_height);
  glViewport(0, 0, screen_width, screen_height);
  glClearColor(0,0,0,0);
  glClear( GL_COLOR_BUFFER_BIT );
  ffi_event(EVENT_INIT,screen_width,screen_height);
}

static void display_cb(void *p)
{
  ffi_event(EVENT_REDRAW,0,0);
}

static void resume_cb(void *p)
{
  ffi_event(EVENT_RESUME,0,0);
}

static void suspend_cb(void *p)
{
  ffi_event(EVENT_SUSPEND,0,0);
}

static void close_cb(void *p)
{
  ffi_event(EVENT_CLOSE,0,0);
  ffi_event(EVENT_TERMINATE,0,0);
}

static void event_cb(bps_event_t *event, int domain, int code, void *p)
{
  int screen_val, key_val, sym_val;
  int pos[2];
  int pointerid;
  
  if (screen_get_domain() == domain) {
    screen_event_t screen_event = screen_event_get_event(event);
    screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_TYPE, &screen_val);
    switch (screen_val) {

      case SCREEN_EVENT_MTOUCH_TOUCH:
        screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_TOUCH_ID, &pointerid);
        screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_SOURCE_POSITION, pos);
#ifdef USE_MULTITOUCH
        ffi_event(EVENT_MULTITOUCH,(unsigned int)pointerid,0);
#endif
        ffi_event(EVENT_BUTTON1DOWN,pos[0],pos[1]);
        break;

      case SCREEN_EVENT_MTOUCH_RELEASE:
        screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_TOUCH_ID, &pointerid);
        screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_SOURCE_POSITION, pos);
#ifdef USE_MULTITOUCH
        ffi_event(EVENT_MULTITOUCH,(unsigned int)pointerid,0);
#endif
        ffi_event(EVENT_BUTTON1UP,pos[0],pos[1]);
        break;

      case SCREEN_EVENT_MTOUCH_MOVE:
        screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_TOUCH_ID, &pointerid);
        screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_SOURCE_POSITION, pos);
#ifdef USE_MULTITOUCH
        ffi_event(EVENT_MULTITOUCH,(unsigned int)pointerid,0);
#endif
        ffi_event(EVENT_MOTION,pos[0],pos[1]);
        break;

      case SCREEN_EVENT_KEYBOARD:
        screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_KEY_FLAGS, &key_val);
        if (key_val & KEY_DOWN) {
          screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_KEY_SYM,&sym_val);
          if (sym_val>32&&sym_val<127) {
            ffi_event(EVENT_KEYPRESS, sym_val,0);
          }
        } else {
          screen_get_event_property_iv(screen_event, SCREEN_PROPERTY_KEY_SYM,&sym_val);
          if (sym_val>32&&sym_val<127) {
            ffi_event(EVENT_KEYRELEASE, sym_val,0);
          }
        }
        break;
     }
  }
}

int main(int argc, char **argv)
{
  glview_initialize(GLVIEW_API_OPENGLES_11, &display_cb);
  glview_register_initialize_callback(&init_cb);
  glview_register_event_callback(&event_cb);
  glview_register_foreground_callback(&resume_cb);
  glview_register_background_callback(&suspend_cb);
  glview_register_finalize_callback(&close_cb);
  glview_loop();
  return 0;
}

