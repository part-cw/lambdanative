#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2016, University of British Columbia
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

(c-define (c-heartbeat) () void "scm_heartbeat" "" (##thread-heartbeat!))

(c-declare  #<<end-of-c-declare

#ifdef MACOSX
void minimize_macosx();
#endif

void minimize() {
  static int do_mini=0;
#ifdef MACOSX
  if (do_mini) minimize_macosx();
#endif
  do_mini=0;
}

#if defined(ANDROID) || defined(IOS) 
int scm_width() { return 0; }
int scm_height() { return 0; }
int scm_screenwidth() { return 0; }
int scm_screenheight() { return 0; }
void scm_event(int t, int x, int y) { scm_heartbeat(); }
#endif

end-of-c-declare
)

;; fall back to launching into the default host browser 
(if (not (member (system-platform) '("android" "ios"))) (begin
  (launch-url "http://127.0.0.1:8080")
  (let ((gui #f))
    ((eval 'main)
      (lambda (w h)
        ((eval 'make-window) 320 32)
        ((eval 'glgui-orientation-set!) (eval 'GUI_PORTRAIT))
        (set! gui ((eval 'make-glgui)))
      )
     (lambda (t x y)
       ((c-lambda () void "minimize"))
       (if (= t (eval 'EVENT_KEYPRESS)) (begin
         (if (= x (eval 'EVENT_KEYESCAPE)) ((eval 'terminate)))))
       ((eval 'glgui-event) gui t x y))
     (lambda () #t)
     (lambda () ((eval 'glgui-suspend)))
     (lambda () ((eval 'glgui-resume)))
    )
  )
))

;; eof
