#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
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

;; time entry widget consisting of two vertical value pickers for hours and minutes

;; Valid hours and minutes, as strings to show 2 characters at all times
(define glgui:timepicker_hours `("00" "01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" 
                                "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23"))
(define glgui:timepicker_minutes `("00" "01" "02" "03" "04" "05" "06" "07" "08" "09" 
                                  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" 
                                  "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" 
                                  "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" 
                                  "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" 
                                  "50" "51" "52" "53" "54" "55" "56" "57" "58" "59"))

(define (glgui:timepicker-callback widget hpicker mpicker)
  ;; The wgt parameter in the lambda is the specific picker
  (lambda (g wgt . x)
    ;; Build a date string from the hour and minute
    (let* ((hour (fix (glgui-widget-get g hpicker 'value)))
           (minute (fix (glgui-widget-get g mpicker 'value))))

       ;; Get the time specified in the pickers as a string
       (let* ((timestr (string-append "1/1/1970 " (number->string hour) ":" (number->string minute) ":00" ))
              (newvalue (string->seconds timestr "%m/%d/%Y %H:%M:%S")))
         ;; Set the current time
         (glgui-widget-set! g widget 'value newvalue))
    )
    ;; Then call the callback for this component - if there is one
    (let ((cb (glgui-widget-get g widget 'callback)))
       (if cb (cb g widget)))
  ))

(define (glgui:timepicker-get-hour secs)
  (string->number (seconds->string secs "%H"))
)

(define (glgui:timepicker-get-minute secs)
  (string->number (seconds->string secs "%M"))
)

;; When any parameters of the widget are updated, potentially update the individual pickers for hours and minutes
(define (glgui:timepicker-update g wgt id val)
  (let ((hpicker (glgui-widget-get g wgt 'hourpicker))
        (mpicker (glgui-widget-get g wgt 'minutepicker))
        (clabel (glgui-widget-get g wgt 'clabel)))
    (cond 
      ;; Directly update all widgets for some parameters
      ((or (eqv? id 'y) (eqv? id 'h) (eqv? id 'hidden) (eqv? id 'font))
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val)
        (glgui-widget-set! g clabel id val))
      ((eqv? id 'colorvalue)
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val)
        (glgui-widget-set! g clabel 'color val))
      ((eqv? id 'colorbg)
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val)
        (glgui-widget-set! g clabel 'bgcolor val))
     ;; Directly update the minute and hour pickers for some parameters
      ((or (eqv? id 'topdown) (eqv? id 'colorarrows) (eqv? id 'colorhighlight))
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val))
      ;; Change in value - update pickers
      ((eqv? id 'value)
        (glgui-widget-set! g hpicker 'value (glgui:timepicker-get-hour val))
        (glgui-widget-set! g mpicker 'value (glgui:timepicker-get-minute val)))
      ;; Update x or w
      ((or (eqv? id 'x) (eqv? id 'w))
        (let* ((w (glgui-widget-get g wgt 'w))
               (x (glgui-widget-get g wgt 'x))
               (dx (fix (/ (- w 2) 2))))
          (glgui-widget-set! g hpicker 'x x)
          (glgui-widget-set! g hpicker 'w dx)
          (glgui-widget-set! g mpicker 'x (+ x dx))
          (glgui-widget-set! g mpicker 'w dx)
          (glgui-widget-set! g clabel 'x (- (+ x dx) 5))))))
)

;; Create this time widget
(define (glgui-timepicker g x y w h colorarrows colorhighlight colorvalue colorbg font)
  (let* ((time ##now)
         (dx (fix (/ (- w 2) 2)))
         ;; Create the two vertical pickers for hours and minutes and the colon in between
         (hpicker (glgui-verticalvaluepicker g x y dx h #f #f colorarrows colorhighlight colorvalue colorbg font glgui:timepicker_hours))
         (mpicker (glgui-verticalvaluepicker g (+ x dx) y dx h #f #f colorarrows colorhighlight colorvalue colorbg font glgui:timepicker_minutes))
         (clabel (glgui-label g (- (+ x dx) 5) y 10 h ":" font colorvalue colorbg))
         (widget (glgui-widget-add g
          'x x
          'y y
          'w w
          'h h
          'callback #f
          'update-handle glgui:timepicker-update
          'hidden #f
          'value time
          'colorarrows colorarrows
          'colorhighlight colorhighlight
          'colorvalue colorvalue
          'colorbg colorbg
          'font font
          ;; Topdown can be set to true to reverse the order of times (down arrow to get to later times instead of up arrow)
          'topdown #f
          ;; The pickers that make up this widget
          'hourpicker hpicker
          'minutepicker mpicker
          'clabel clabel)))
    ;; The pickers can roll through from :23 or :59 over to :00
    (glgui-widget-set! g hpicker 'cycle #t)
    (glgui-widget-set! g mpicker 'cycle #t)
    ;; Hook into the callback of the pickers
    (glgui-widget-set! g hpicker 'callback (glgui:timepicker-callback widget hpicker mpicker))
    (glgui-widget-set! g mpicker 'callback (glgui:timepicker-callback widget hpicker mpicker))
    ; Set topdown and values for the pickers
    (glgui:timepicker-update g widget 'topdown #f)
    (glgui:timepicker-update g widget 'value time)
    ;; Return the widget
    widget))
;; eof