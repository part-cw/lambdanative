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
(define glgui:timepicker_hours_ampm `("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12"))

(define (glgui:timepicker-callback widget hpicker mpicker ampmbutton)
  ;; The wgt parameter in the lambda is the specific picker
  (lambda (g wgt . x)
    ;; Build a date string from the hour and minute
    (let* ((oldvalue (glgui-widget-get g widget 'value))
           (hourrange (glgui-widget-get g hpicker 'vallist))
           (oldhourindex (glgui:timepicker-get-hour hourrange oldvalue ampmbutton))
           (hourindex (fix (glgui-widget-get g hpicker 'value)))
           (hour (if (fx< hourindex (length hourrange)) (list-ref hourrange hourindex) (car hourrange)))
           (minuterange (glgui-widget-get g mpicker 'vallist))
           (minuteindex (fix (glgui-widget-get g mpicker 'value)))
           (minute (if (fx< minuteindex (length minuterange)) (list-ref minuterange minuteindex) (car minuterange)))
           (oldampm (glgui:timepicker-get-ampm oldvalue))
           ;; Update AM/PM when going from 11->12 or 12->11, or when AM/PM button pressed
           (newampm (if (or (and (fx= oldhourindex 10) (fx= hourindex 11)) (and (fx= oldhourindex 11) (fx= hourindex 10)) (eqv? wgt ampmbutton))
                      (if (string=? oldampm "AM") " PM" " AM")
                      (string-append " " oldampm))))
       ;; Get the time specified in the pickers as a string
       (let* ((timestr (string-append "1/1/1970 " hour ":" minute ":00" (if ampmbutton newampm "")))
              (newvalue (string->seconds timestr (string-append "%m/%d/%Y " (if ampmbutton "%I:%M:%S %p" "%H:%M:%S")))))
         ;; Set the current time
         (glgui-widget-set! g widget 'value newvalue))
    )
    ;; Then call the callback for this component - if there is one
    (let ((cb (glgui-widget-get g widget 'callback)))
       (if cb (cb g widget)))
))

(define (glgui:timepicker-get-hour range secs ampm)
  (let* ((hourstr (seconds->string secs (if ampm "%I" "%H")))
         (index (list-pos range hourstr)))
    (if index index 0 )))

(define (glgui:timepicker-get-minute range secs)
  (let* ((minutestr (seconds->string secs "%M"))
         (index (list-pos range minutestr)))
    (if index index 0))
)

(define (glgui:timepicker-get-ampm secs)
  (seconds->string secs "%p")
)


;; When any parameters of the widget are updated, potentially update the individual pickers for hours and minutes
(define (glgui:timepicker-update g wgt id val)
  (let* ((hpicker (glgui-widget-get g wgt 'hourpicker))
         (mpicker (glgui-widget-get g wgt 'minutepicker))
         (ampmbutton (glgui-widget-get g wgt 'ampmbutton))
         (bg (glgui-widget-get g wgt 'bg))
         (clabel (glgui-widget-get g wgt 'clabel)))
    (cond 
      ;; Directly update all widgets for some parameters
      ((or (eqv? id 'hidden) (eqv? id 'modal))
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val)
        (if ampmbutton (glgui-widget-set! g ampmbutton id val))
        (glgui-widget-set! g bg id val)
        (glgui-widget-set! g clabel id val))
      ((eqv? id 'font)
        (glgui-widget-set! g hpicker 'fnt val)
        (glgui-widget-set! g mpicker 'fnt val)
        (if ampmbutton (glgui-widget-set! g ampmbutton id val))
        (glgui-widget-set! g clabel id val))
      ((eqv? id 'y)
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val)
        (if ampmbutton (let ((ampm-h (glgui-widget-get g wgt 'h)))
          (glgui-widget-set! g ampmbutton id (+ val (/ ampm-h 3)))))
        (glgui-widget-set! g bg id val)
        (glgui-widget-set! g clabel id val))
      ((eqv? id 'h)
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val)
        (if ampmbutton (let ((ampm-y (glgui-widget-get g wgt 'y)))
          (glgui-widget-set! g ampmbutton 'y (+ ampm-y (/ val 3)))
          (glgui-widget-set! g ampmbutton id (/ val 3))))
        (glgui-widget-set! g bg id val)
        (glgui-widget-set! g clabel id val))
      ((eqv? id 'colorvalue)
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val)
        (if ampmbutton (glgui-widget-set! g ampmbutton 'color val))
        (glgui-widget-set! g clabel 'color val))
      ((eqv? id 'colorbg)
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val)
        (glgui-widget-set! g bg 'color val)
        (glgui-widget-set! g clabel 'bgcolor val))
     ;; Directly update the minute and hour pickers for some parameters
      ((or (eqv? id 'topdown) (eqv? id 'colorarrows) (eqv? id 'colorhighlight) (eqv? id 'scalearrows))
        (glgui-widget-set! g hpicker id val)
        (glgui-widget-set! g mpicker id val))
      ;; Directly update the ampm button for some parameters
      ((and ampmbutton (eqv? id 'button-normal-color))
        (glgui-widget-set! g ampmbutton id val))
      ;; Change in value - update pickers
      ((eqv? id 'value)
        (glgui-widget-set! g hpicker 'value (glgui:timepicker-get-hour (glgui-widget-get g hpicker 'vallist) val ampmbutton))
        (glgui-widget-set! g mpicker 'value (glgui:timepicker-get-minute (glgui-widget-get g mpicker 'vallist) val))
        (if ampmbutton
          (let ((ampm-str (glgui:timepicker-get-ampm val)))
            (glgui-widget-set! g ampmbutton 'value (if (string=? ampm-str "AM") 0 1))
            (glgui-widget-set! g ampmbutton 'image (list ampm-str)))))
      ;; Update x or w
      ((or (eqv? id 'x) (eqv? id 'w))
        (let* ((w (glgui-widget-get g wgt 'w))
               (x (glgui-widget-get g wgt 'x))
               (dx (if ampmbutton (fix (/ (- w 3) 3)) (fix (/ (- w 2) 2)))))
          (glgui-widget-set! g hpicker 'x x)
          (glgui-widget-set! g hpicker 'w dx)
          (glgui-widget-set! g mpicker 'x (+ x dx))
          (glgui-widget-set! g mpicker 'w dx)
          (glgui-widget-set! g bg 'x x)
          (glgui-widget-set! g bg 'w w)
          (glgui-widget-set! g clabel 'x (- (+ x dx) 5))
          (if ampmbutton
            (begin
              (glgui-widget-set! g ampmbutton 'x (+ x dx dx))
              (glgui-widget-set! g ampmbutton 'w dx)))))
      ;; Update the limits
      ((or (eqv? id 'hourmax) (eqv? id 'hourmin))
         (let* ((max (glgui-widget-get g wgt 'hourmax))
                (min (glgui-widget-get g wgt 'hourmin))
                (wrapped (fx< max min))
                (lastindex (if wrapped (- 23 (- min max 1)) (- max min)))
                (newrange (if wrapped
                            (append (list-tail glgui:timepicker_hours min) (list-head glgui:timepicker_hours (+ max 1)))
                            (list-head (list-tail glgui:timepicker_hours min) (+ lastindex 1)))))
           (glgui-widget-set! g hpicker 'vallist newrange)
           (glgui-widget-set! g hpicker 'valmax lastindex)
           ;; Now reset value
           (glgui:timepicker-update g wgt 'value (glgui-widget-get g wgt 'value))))
      ;; Switch from 24hr to AM/PM and vice versa
      ((eqv? id 'ampm)
       (let* ((w (glgui-widget-get g wgt 'w))
              (x (glgui-widget-get g wgt 'x))
              (h (glgui-widget-get g wgt 'h))
              (y (glgui-widget-get g wgt 'y))
              (font (glgui-widget-get g wgt 'font))
              (dx (if val (fix (/ (- w 3) 3)) (fix (/ (- w 2) 2))))
              (value (glgui-widget-get g wgt 'value))
              (hourlist (if val glgui:timepicker_hours_ampm glgui:timepicker_hours))
              (hourupdated (glgui:timepicker-get-hour hourlist value val)))
         (glgui-widget-set! g hpicker 'vallist hourlist)
         (glgui-widget-set! g hpicker 'value hourupdated)
         (glgui-widget-set! g hpicker 'w dx)
         (glgui-widget-set! g hpicker 'valmax (- (length hourlist) 1))
         (glgui-widget-set! g clabel 'x (- (+ x dx) 5))
         (glgui-widget-set! g mpicker 'x (+ x dx))
         (glgui-widget-set! g mpicker 'w dx)
         (if (not val) (glgui-widget-delete g ampmbutton))
         (let ((ampmbutton (if val (glgui-button-string g (+ x dx dx) (+ y (/ h 3)) dx (/ h 3) "AM" font #f) #f)))
           (glgui-widget-set! g wgt 'ampmbutton ampmbutton)
           (if ampmbutton
             (begin
               (glgui:timepicker-color-ampm-button g wgt ampmbutton)
               (glgui-widget-set! g ampmbutton 'callback (glgui:timepicker-callback wgt hpicker mpicker ampmbutton))))
           (glgui-widget-set! g hpicker 'callback (glgui:timepicker-callback wgt hpicker mpicker ampmbutton))
           (glgui-widget-set! g mpicker 'callback (glgui:timepicker-callback wgt hpicker mpicker ampmbutton))
           (glgui:timepicker-update g wgt 'value value))))
  ))
)

(define (glgui:timepicker-color-ampm-button g wgt ampmbutton)
  (glgui-widget-set! g ampmbutton 'button-normal-color (glgui-widget-get g wgt 'button-normal-color))
  (glgui-widget-set! g ampmbutton 'color (glgui-widget-get g wgt 'colorvalue))
  (glgui-widget-set! g ampmbutton 'button-selected-color (glgui-widget-get g wgt 'colorhighlight))
  (glgui-widget-set! g ampmbutton 'solid-color #t)
)

;; Create this time widget
(define (glgui-timepicker g x y w h colorarrows colorhighlight colorvalue colorbg font . ampmtime)
  (let* ((time ##now)
         (ampm (if (fx= (length ampmtime) 1) (car ampmtime) #f))
         (dx (if ampm (fix (/ (- w 3) 3)) (fix (/ (- w 2) 2))))
         ;; Create the two vertical pickers for hours and minutes and the colon in between
         (bg (glgui-box g x y w h colorbg))
         (hpicker (glgui-verticalvaluepicker g x y dx h #f #f colorarrows colorhighlight colorvalue colorbg font (if ampm glgui:timepicker_hours_ampm glgui:timepicker_hours)))
         (mpicker (glgui-verticalvaluepicker g (+ x dx) y dx h #f #f colorarrows colorhighlight colorvalue colorbg font glgui:timepicker_minutes))
         ;; Create AM/PM button if user specifies
         (ampmbutton (if ampm (glgui-button-string g (+ x dx dx) (+ y (/ h 3)) dx (/ h 3) "AM" font #f) #f))
         (clabel (glgui-label g (- (+ x dx) 5) y 10 h ":" font colorvalue colorbg))
         (widget (glgui-widget-add g
          'x x
          'y y
          'w w
          'h h
          'ampm ampm
          'callback #f
          'update-handle glgui:timepicker-update
          'hidden #f
          'value time
          ;; Maximum and minimum hour selectable, can wrap around (ex. 20 and 6 for range 20:00-6:59)
          'hourmin 0
          'hourmax 23
          'colorarrows colorarrows
          'colorhighlight colorhighlight
          'colorvalue colorvalue
          'colorbg colorbg
          'button-normal-color colorbg
          'font font
          ;; Topdown can be set to true to reverse the order of times (down arrow to get to later times instead of up arrow)
          'topdown #f
          'scalearrows #f
          ;; The pickers that make up this widget
          'hourpicker hpicker
          'minutepicker mpicker
          'ampmbutton ampmbutton
          'bg bg
          'clabel clabel
          )))
    ;; Set colours of AM/PM button
    (if ampm (glgui:timepicker-color-ampm-button g widget ampmbutton))
    ;; The pickers can roll through from :23 or :59 over to :00
    (glgui-widget-set! g hpicker 'cycle #t)
    (glgui-widget-set! g mpicker 'cycle #t)
    ;; Hook into the callback of the pickers
    (glgui-widget-set! g hpicker 'callback (glgui:timepicker-callback widget hpicker mpicker ampmbutton))
    (glgui-widget-set! g mpicker 'callback (glgui:timepicker-callback widget hpicker mpicker ampmbutton))
    (if ampm (glgui-widget-set! g ampmbutton 'callback (glgui:timepicker-callback widget hpicker mpicker ampmbutton)))
    ;; Set topdown and values for the pickers
    (glgui:timepicker-update g widget 'topdown #f)
    (glgui:timepicker-update g widget 'value time)
    ;; Return the widget
    widget))
;; eof
