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

(c-declare  #<<end-of-c-declare

#ifdef ANDROID
  void android_show_keypad(int orientation);
  void android_hide_keypad();
#endif

#ifdef IOS
  void ios_show_keypad(int orientation);
  void ios_hide_keypad();
#endif

void native_keypad_show(int orientation){
#ifdef IOS
  ios_show_keypad(orientation);
#elif ANDROID
  android_show_keypad(orientation);
#else
   return;
#endif
}
void native_keypad_hide(void){
#ifdef IOS
  ios_hide_keypad();
#elif ANDROID
  android_hide_keypad();
#else
   return;
#endif
}

end-of-c-declare
)

(define glgui-native-keypad-hide (c-lambda () void "native_keypad_hide"))
(define (glgui-native-keypad-show)
  ((c-lambda (int) void "native_keypad_show(___arg1);") glgui:rotate)
)
;; Widget functions
(define (glgui:native-keypad-draw g wgt)
  (let ((x (glgui-widget-get-dyn g wgt 'x))
        (y (glgui-widget-get-dyn g wgt 'y))
        (w (glgui-widget-get-dyn g wgt 'w))
        (h (glgui-widget-get-dyn g wgt 'h)))
    (glgui:draw-box x y w h DarkGrey)
  ))

(define (glgui:native-keypad-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
    (if inside (glgui-native-keypad-show))
    inside
  ))

(define (glgui:native-keypad-update g w id val)
  (if (eq? id 'hidden)
    (if val
      (glgui-native-keypad-hide)
      (glgui-native-keypad-show)
    )
  ))

(define (glgui-native-keypad g x y w h . keypad)
  (glgui-native-keypad-show)
  (glgui-widget-add g
     'x 0
     'y 0
     'w (glgui-width-get)
     'h (- 216 30)
     'hidden #f
     'draw-handle  glgui:native-keypad-draw
     'input-handle glgui:native-keypad-input
     'update-handle glgui:native-keypad-update
  ))
;; eof
