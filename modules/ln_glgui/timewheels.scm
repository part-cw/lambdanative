#|
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
|#

;; time entry widget consisting of two vertical wheels for hours and minutes

;; Valid hours and minutes, as strings to show 2 characters at all times
(define glgui:timewheel_hours `("00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" 
                                "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23"))
(define glgui:timewheel_minutes `("00" "01" "02" "03" "04" "05" "06" "07" "08" "09" 
                                  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" 
                                  "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" 
                                  "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" 
                                  "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" 
                                  "50" "51" "52" "53" "54" "55" "56" "57" "58" "59"))

(define (glgui:timewheels-callback widget hwheel mwheel)
  ;; The wgt parameter in the lambda is the specific wheel
  (lambda (g wgt . x)
    ;; Build a date string from the hour and minute
    (let* ((hour (fix (glgui-widget-get g hwheel 'value)))
           (minute (fix (glgui-widget-get g mwheel 'value))))

       ;; Get the time specified in the wheels as a string
       (let* ((timestr (string-append "1/1/1970 " (number->string hour) ":" (number->string minute) ":00" ))
              (newvalue (string->seconds timestr "%m/%d/%Y %H:%M:%S")))
         ;; Set the current time
         (glgui-widget-set! g widget 'value newvalue))
    )
    ;; Then call the callback for this component - if there is one
    (let ((cb (glgui-widget-get g widget 'callback)))
       (if cb (cb g widget)))
  ))

(define (glgui:timewheels-get-hour secs)
  (string->number (seconds->string secs "%H"))
)
(define (glgui:timewheels-get-minute secs)
  (string->number (seconds->string secs "%M"))
)
(define (glgui:timewheel-update g wgt id val)
  (let ((hwheel (glgui-widget-get g wgt 'hourwheel))
        (mwheel (glgui-widget-get g wgt 'minutewheel)))
    (cond 
     ;; Directly update all subwheels for some parameters
      ((or (eqv? id 'y) (eqv? id 'hidden) (eqv? id 'topdown)  (eqv? id 'colorvalue) (eqv? id 'colorshade))
        (glgui-widget-set! g hwheel id val)
        (glgui-widget-set! g mwheel id val))
      ;; Change in value - update wheels
      ((eqv? id 'value)
        (glgui-widget-set! g hwheel 'value (glgui:timewheels-get-hour val))
        (glgui-widget-set! g mwheel 'value (glgui:timewheels-get-minute val)))
      ;; Update x
      ((eqv? id 'x)
        (glgui-widget-set! g hwheel 'x val)
        (glgui-widget-set! g mwheel 'x (+ val 46)))
      ;; Update fonts
      ((eqv? id 'bigfnt)
        (glgui-widget-set! g hwheel 'bigfnt val)
        (glgui-widget-set! g mwheel 'bigfnt val))
      ((eqv? id 'smlfnt)
        (glgui-widget-set! g hwheel 'smlfnt val)
        (glgui-widget-set! g mwheel 'smlfnt val))))
)

;; This procedure sets the time displayed on the wheels
(define (glgui-timewheels-set-time g w hours minutes)
  (glgui-widget-set! g w 'value 
     (string->seconds (string-append "01/01/1970 " hours ":" minutes ":00") "%m/%d/%Y %H:%M:%S"))
)

;; Create this time widget
(define (glgui-timewheels g x y w h colorvalue colorshade selfont unselfont)
  (let* ((time ##now)
        ;; Create the two vertical wheels
        (hwheel (glgui-verticalnumberwheel g x y 45 144 #f #f #f #f colorvalue colorshade
                 selfont unselfont #f glgui:timewheel_hours))
        (mwheel (glgui-verticalnumberwheel g (+ x 46) y 45 144 #f #f #f #f colorvalue colorshade selfont 
                 unselfont #f glgui:timewheel_minutes))
        (widget (glgui-widget-add g
          'x x
          'y y
          'w w
          'h h
          'callback #f
          'update-handle glgui:timewheel-update
          'hidden #f
          'value time
          'colorvalue colorvalue
          'colorshade colorshade
          'bigfnt selfont
          'smlfnt unselfont
          ;; Topdown can be set to false to make the latest time be displayed at the top instead of the bottom
          'topdown #t
          ;; The wheels that make up this widget
          'hourwheel hwheel
          'minutewheel mwheel)))
    ;; The wheels can roll through from :23 or :59 over to :00
    (glgui-widget-set! g hwheel 'cycle #t)
    (glgui-widget-set! g mwheel 'cycle #t)
    ;; Hook into the callback of the wheels
    (glgui-widget-set! g hwheel 'callback (glgui:timewheels-callback widget hwheel mwheel))
    (glgui-widget-set! g mwheel 'callback (glgui:timewheels-callback widget hwheel mwheel))
    ; Set topdown and values for the wheels
    (glgui:timewheel-update g widget 'topdown #t)
    (glgui:timewheel-update g widget 'value time)
    ;; Make sure to still return the widget
    widget))
;; eof