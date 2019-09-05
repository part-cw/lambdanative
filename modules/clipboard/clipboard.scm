#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2018, University of British Columbia
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
|#

(c-declare  #<<end-of-c-declare

#ifdef IOS
  extern int ios_clipboard_copy(char* str);
  extern int ios_clipboard_hascontent();
  extern char *ios_clipboard_paste();
#endif
#ifdef MACOSX
  extern int macosx_clipboard_copy(char* str);
  extern int macosx_clipboard_hascontent();
  extern int macosx_clipboard_clear();
  extern char *macosx_clipboard_paste();
#endif
#ifdef ANDROID
  extern int android_clipboard_copy(char *str);
  extern char *android_clipboard_paste();
  extern int android_clipboard_clear();
  extern int android_clipboard_hascontent();
  extern void android_clipboard_release();
#endif
#ifdef LINUX
  #include <X11/Xlib.h>
  extern Display* microgl_getDisplay();
  extern Window microgl_getWindow();
  extern void log_c(char *);
  extern void microgl_setCopiedString(char *, int len);
  extern void *microgl_getCopiedString(char **);
#endif

// Clipboard copying
int clipboard_copy(char *str, int len){
#ifdef MACOSX
  return macosx_clipboard_copy(str);
#endif
#ifdef WIN32
  if (!OpenClipboard(NULL)){
    return 0;
  }
  EmptyClipboard();
  HGLOBAL hglbCopy = GlobalAlloc(GMEM_MOVEABLE, (len+1) * sizeof(TCHAR));
  if (hglbCopy == NULL) {
    CloseClipboard();
    return 0;
  }
  LPTSTR lptstrCopy = GlobalLock(hglbCopy);
  memcpy(lptstrCopy, str, len * sizeof(TCHAR));
  lptstrCopy[len] = (TCHAR) 0;
  GlobalUnlock(hglbCopy);
  SetClipboardData(CF_TEXT, hglbCopy);
  CloseClipboard();
  return 1;
#endif
#ifdef ANDROID
  return android_clipboard_copy(str);
#endif
#ifdef IOS
  return ios_clipboard_copy(str);
#endif
#ifdef LINUX
  Display *display = microgl_getDisplay();
  Window window = microgl_getWindow();
  Atom selection = XInternAtom(display, "CLIPBOARD", 0);
  XSetSelectionOwner (display, selection, window, 0);
  if (XGetSelectionOwner (display, selection) != window)
    return 0;
  microgl_setCopiedString(str, len);
  return 0;
#endif
  return 0;
}

// Clipboard pasting
char *clipboard_paste(){
#ifdef ANDROID
  return android_clipboard_paste();
#endif
#ifdef IOS
  return ios_clipboard_paste();
#endif
#ifdef WIN32
  char *str = NULL;
  if (!IsClipboardFormatAvailable(CF_TEXT)){
    return "";
  }
  if (!OpenClipboard(NULL)){
    return "";
  }
  HGLOBAL hglb = GetClipboardData(CF_TEXT);
  if (hglb != NULL) {
    LPTSTR lptstr = GlobalLock(hglb);
    if (lptstr != NULL){
      // Figure out if Unicode
      if (sizeof(TCHAR) == sizeof(char)){
        size_t size = strlen(lptstr);
        str = malloc(size+1);
        strcpy(str, lptstr);
      } else {
        size_t size = wcstombs(NULL, lptstr, 0);
        str = malloc(size+1);
        wcstombs(str, lptstr, size+1);
      }
      GlobalUnlock(hglb);
    }
  }
  CloseClipboard();
  return str;
#endif
#ifdef MACOSX
  return macosx_clipboard_paste();
#endif
#ifdef LINUX
  char *str = NULL;
  Display *display = microgl_getDisplay();
  Window window = microgl_getWindow();
  Atom selection = XInternAtom(display, "CLIPBOARD", 0);
  Window owner = XGetSelectionOwner(display, selection);
  if (owner == window) {
    microgl_getCopiedString(&str);
    return str;
  }

  Atom type = XInternAtom(display, "STRING", 0);
  Atom propid = XInternAtom(display, "XSEL_DATA", 0);
  Atom incrid = XInternAtom(display, "INCR", 0);
  XEvent event;

  XConvertSelection(display, selection, type, propid, window, CurrentTime);
  do {
    XNextEvent(display, &event);
  } while (event.type != SelectionNotify || event.xselection.selection != selection);

  if (event.xselection.property){
    unsigned long ressize, restail;
    int resbits;
    XGetWindowProperty(display, window, propid, 0, LONG_MAX/4, 0, AnyPropertyType,
        &type, &resbits, &ressize, &restail, (unsigned char**)&str);

    if (type == incrid){
      log_c("Buffer is too large");
      str="";
    }
  }else{
    str="";
  }
  return str;
#endif
  char* buf="";
  return buf;
}

// Releases reference to string fetched during paste
void clipboard_release(){
#ifdef ANDROID
  android_clipboard_release();
#endif
}

// Clipboard Clearing
int clipboard_clear(){
#ifdef WIN32
  if (!OpenClipboard(NULL)){
    return 0;
  }
  int ret=EmptyClipboard();
  CloseClipboard();
  return ret;
#endif
#ifdef ANDROID
  return android_clipboard_clear();
#endif
#ifdef MACOSX
  return macosx_clipboard_clear();
#endif

  return 0;
}

// Clipboard content checking
int clipboard_hascontent(){
#ifdef ANDROID
  return android_clipboard_hascontent();
#endif
#ifdef IOS
  return ios_clipboard_hascontent();
#endif
#ifdef MACOSX
  return macosx_clipboard_hascontent();
#endif

  /* FIXME: Actually check win32 and linux.  However it's bettert to
   * provide a false positive claiming content (thus suggesting to the
   * user to paste is even though that would fail) than falsely claim
   * nothing available thus preventing avail content to be pasted.
   */

  return 1;
}

end-of-c-declare
)

;; Function prototypes/bindings
(define clipboard-clear (c-lambda (char-string) bool "clipboard_clear"))
(define (clipboard-copy str)
  ((c-lambda (char-string int) bool "___result=
    clipboard_copy(___arg1,___arg2);")
    str (string-length str)))
(define (clipboard-paste)
  (let ((str ((c-lambda () char-string "___result=clipboard_paste();"))))
    ((c-lambda () void "clipboard_release();"))
    str))
(define clipboard-hascontent (c-lambda () bool "___result=clipboard_hascontent();"))

;; eof
