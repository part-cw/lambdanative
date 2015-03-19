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

;; date entry widget consisting of displayed year, month and day and arrow buttons to change the values

;; Months of the year
(define glgui:datepicker_months `("Jan" "Feb"  "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
(define glgui:datepicker_month_lengths `(31 28 31 30 31 30 31 31 30 31 30 31))

;; Constants for the order of the date elements
(define GUI_YEAR_MONTH_DAY 1)
(define GUI_DAY_MONTH_YEAR 2)
(define GUI_MONTH_DAY_YEAR 2)

;; Allows an app to change the month strings into another language. This will be for all the date pickers in the app.
(define (glgui-datepicker-update-months strlist)
  (set! glgui:datepicker_months strlist))

;; Public shortcut procedure to call the one below to enforce the limits
(define (glgui-datepicker-update-limits gui wgt)
  (let ((mpicker (glgui-widget-get g wgt 'monthpicker))
        (dpicker (glgui-widget-get g wgt 'datepicker))
        (ypicker (glgui-widget-get g wgt 'yearpicker)))
    (glgui-widget-set! gui wgt 'unlimited #f)
    (glgui:datepicker-update-picker-limits gui wgt mpicker dpicker ypicker #t #t)))

;; Updates the limits of the month, day and year pickers based on a new value or date limits.
(define (glgui:datepicker-update-picker-limits gui widget mpicker dpicker ypicker limitday? limitmonth?)
  ;; Get the month, day and year as numbers
  (let* ((month (+ (fix (glgui-widget-get gui mpicker 'value)) 1))
         (day (fix (glgui-widget-get gui dpicker 'value)))
         (year (fix (glgui-widget-get gui ypicker 'value)))
         (dmin (glgui-widget-get gui widget 'datemin))
         (eyear (fix (string->number (seconds->string dmin "%Y"))))
         (dmax (glgui-widget-get gui widget 'datemax))
         (lyear (fix (string->number (seconds->string dmax "%Y"))))
         (oldvalue (glgui-widget-get gui widget 'value)))
    
         ;; Check if we are in the earliest year - ie. need to limit the other pickers (depending on limitday and limitmonth)
         (if (fx= year eyear)
           (let ((emonth (fix (string->number (seconds->string dmin "%m")))))
             ;; Make sure month isn't earlier than allowed
             (if (and limitmonth? (fx< month emonth)) (set! month emonth))
             ;; Limit available months
             (if limitmonth?
               (glgui-widget-set! gui mpicker 'valmin (- emonth 1)))
             ;; Check if we are in the earliest month (of the earliest year)
             (if (= month emonth)
               (let ((eday (fix (string->number (seconds->string dmin "%d")))))
                 ;; Make sure day isn't earlier than allowed
                 (if (and limitday? (fx< day eday)) (set! day eday))
                 ;; Limit available days
                 (if limitday?
                   (glgui-widget-set! gui dpicker 'valmin eday)))
               ;; Otherwise no limit on earliest day of the month
               (glgui-widget-set! gui dpicker 'valmin 1)))
           ;; Otherwise no limit on earliest month or day of the month
           (begin 
             (glgui-widget-set! gui mpicker 'valmin 0)
             (glgui-widget-set! gui dpicker 'valmin 1)))
         ;; Check if we are in the latest year - ie. need to limit the other pickers
         (if (fx= year lyear)
           (let ((lmonth (fix (string->number (seconds->string dmax "%m")))))
             ;; Make sure month isn't later than allowed
             (if (and limitmonth? (fx> month lmonth)) (set! month lmonth))
             ;; Limit available months
             (if limitmonth?
               (glgui-widget-set! gui mpicker 'valmax (- lmonth 1)))
             ;; Check if we are in the latest month (of the latest year)
             (if (= month lmonth)
               (let ((lday (fix (string->number (seconds->string dmax "%d")))))
                  ;; Make sure day isn't later than allowed
                  (if (and limitday? (fx> day lday)) (set! day lday))
                  ;; Limit available days
                  (if limitday?
                    (glgui-widget-set! gui dpicker 'valmax lday)))
               ;; Otherwise no limit on latest day of the month - except the number of days in the month
               (let ((maxday
                        (if (fx= month 2)
                          ;; If February, check if leap year, leap year is divisible by 4, but not by 100, unless by 400. ie. 1896, 2000, 2004 are leap years. 1900, 1901, 1999 are not
                          (if (fx= (modulo year 4) 0) (if (fx= (modulo year 100) 0) (if (fx= (modulo year 400) 0) 29 28) 29) 28)
                          ;; Normally just use lengths from list
                          (list-ref glgui:datepicker_month_lengths (fx- month 1)))))
                   (if (and limitday? (fx> day maxday)) (set! day maxday))
                   (if limitday?
                     (glgui-widget-set! gui dpicker 'valmax maxday)))))
           ;; Otherwise no limit on latest month or day of the month
           (begin 
             (glgui-widget-set! gui mpicker 'valmax 11)
             (let ((maxday
                     (if (fx= month 2)
                       ;; If February, check if leap year, leap year is divisible by 4, but not by 100, unless by 400. ie. 1896, 2000, 2004 are leap years. 1900, 1901, 1999 are not
                       (if (fx= (modulo year 4) 0) (if (fx= (modulo year 100) 0) (if (fx= (modulo year 400) 0) 29 28) 29) 28)
                       ;; Normally just use lengths from list
                       (list-ref glgui:datepicker_month_lengths (fx- month 1)))))
                  (if (and limitday? (fx> day maxday)) (set! day maxday))
                  (if limitday?
                    (glgui-widget-set! gui dpicker 'valmax maxday)))))
         ;; Get the date specified in the pickers as a string and set it as the value if it is different than the original
         (let* ((datestr (string-append (number->string month) "/" (number->string day) "/" (number->string year) " 00:00:00" ))
                ;; Get the new value in seconds
                (newvalue (string->seconds datestr "%m/%d/%Y %H:%M:%S")))
           (if (or (not oldvalue) (not (= oldvalue newvalue)))
             (glgui-widget-set! gui widget 'value newvalue))))
)

(define (glgui:datepicker-callback widget mpicker dpicker ypicker)
  ;; The wgt parameter in the lambda is the specific picker
  (lambda (g wgt . x)
          ;; Limit the date picker values if the month or year pickers just changed
    (let ((limitday? (or (eqv? wgt mpicker) (eqv? wgt ypicker)))
          ;; Limit the month picker values if the year picker just changed
          (limitmonth? (eqv? wgt ypicker)))
       ;; Update the value of the date pickers by building it from the individual pickers while possibly modifying the limits of the displayed pickers
       (glgui:datepicker-update-picker-limits g widget mpicker dpicker ypicker limitday? limitmonth?))
    ;; Then call the callback for this component - if there is one
    (let ((cb (glgui-widget-get g widget 'callback)))
       (if cb (cb g widget)))))

(define (glgui:datepicker-get-day secs)
  (string->number (seconds->string secs "%d"))
)
(define (glgui:datepicker-get-month secs)
  ;; Subtract 1 to match indices, January is 1st month, but becomes 0
  (- (string->number (seconds->string secs "%m")) 1)
)
(define (glgui:datepicker-get-year secs)
  (string->number (seconds->string secs "%Y"))
)
(define (glgui:datepicker-get-end-of-next-year secs)
  (let ((year (+ (string->number (seconds->string secs "%Y")) 1)))
    (string->seconds (string-append "12/31/" (number->string year) " 00:00:00" ) "%m/%d/%Y %H:%M:%S"))
)

(define (glgui:datepicker-update g wgt id val)
  (let ((mpicker (glgui-widget-get g wgt 'monthpicker))
        (dpicker (glgui-widget-get g wgt 'datepicker))
        (ypicker (glgui-widget-get g wgt 'yearpicker)))
    (cond 
      ;; Directly update all pickers for some parameters
      ((or (eqv? id 'y) (eqv? id 'h) (eqv? id 'hidden) (eqv? id 'topdown) (eqv? id 'colorarrows) (eqv? id 'colorhighlight) (eqv? id 'colorvalue) (eqv? id 'colorbg) (eqv? id 'modal))
        (glgui-widget-set! g mpicker id val)
        (glgui-widget-set! g dpicker id val)
        (glgui-widget-set! g ypicker id val))
      ;; Change in value - update pickers
      ((eqv? id 'value)
        (if val
          (let ((enforcelimits (not (glgui-widget-get g wgt 'unlimited))))
            (glgui-widget-set! g mpicker 'value (glgui:datepicker-get-month val))
            (glgui-widget-set! g dpicker 'value (glgui:datepicker-get-day val))
            (glgui-widget-set! g ypicker 'value (glgui:datepicker-get-year val))
            ;; Then update the limits of the pickers based on the new values
            (glgui:datepicker-update-picker-limits g wgt mpicker dpicker ypicker enforcelimits enforcelimits))))
      ;; Update picker limits if max or min have changed, possibly update value too
      ((eqv? id 'datemin)
         (glgui-widget-set! g ypicker 'valmin (fix (string->number (seconds->string val "%Y"))))
         (let ((curvalue (glgui-widget-get g wgt 'value)))
           (if (and curvalue (< curvalue val))
             ;; If value is less than new minimum, update it (will trigger a call to this procedure recursively)
             (glgui-widget-set! g wgt 'value val) 
             ;; Otherwise just update the limits of the pickers
             (let ((enforcelimits (not (glgui-widget-get g wgt 'unlimited))))
               (glgui:datepicker-update-picker-limits g wgt mpicker dpicker ypicker enforcelimits enforcelimits)))))
      ((eqv? id 'datemax)
         (glgui-widget-set! g ypicker 'valmax (fix (string->number (seconds->string val "%Y"))))
         (let ((curvalue (glgui-widget-get g wgt 'value)))
           (if (and curvalue (> curvalue val))
             ;; If value is more than new maximum, update it (will trigger a call to this procedure recursively)
             (glgui-widget-set! g wgt 'value val)   
             ;; Otherwise just update the limits of the pickers
             (let ((enforcelimits (not (glgui-widget-get g wgt 'unlimited))))
               (glgui:datepicker-update-picker-limits g wgt mpicker dpicker ypicker enforcelimits enforcelimits)))))
      ;; Update x or w or the order of the pickers
      ((or (eqv? id 'x) (eqv? id 'w) (eqv? id 'displayorder))
        (let* ((order (glgui-widget-get g wgt 'displayorder))
               (w (glgui-widget-get g wgt 'w))
               (x (glgui-widget-get g wgt 'x))
               (dx (fix (/ (- w 2) 3)))
               ;; Determine which picker is in which order
               (first (cond 
                        ((fx= order GUI_DAY_MONTH_YEAR) dpicker)
                        ((fx= order GUI_YEAR_MONTH_DAY) ypicker)
                        ((fx= order GUI_MONTH_DAY_YEAR) mpicker)))
               (second (cond 
                        ((fx= order GUI_DAY_MONTH_YEAR) mpicker)
                        ((fx= order GUI_YEAR_MONTH_DAY) mpicker)
                        ((fx= order GUI_MONTH_DAY_YEAR) dpicker)))
               (third (cond 
                        ((fx= order GUI_DAY_MONTH_YEAR) ypicker)
                        ((fx= order GUI_YEAR_MONTH_DAY) dpicker)
                        ((fx= order GUI_MONTH_DAY_YEAR) ypicker))))
          (glgui-widget-set! g first 'w dx)
          (glgui-widget-set! g first 'x x)
          (glgui-widget-set! g second 'w dx)
          (glgui-widget-set! g second 'x (+ x dx 1))
          (glgui-widget-set! g third 'w dx)
          (glgui-widget-set! g third 'x (+ x (* 2 dx) 2))))
      ;; Update fonts
      ((eqv? id 'numfnt)
        (glgui-widget-set! g dpicker 'fnt val)
        (glgui-widget-set! g ypicker 'fnt val))
      ((eqv? id 'monthfnt)
        (glgui-widget-set! g mpicker 'fnt val))))
)

;; Make the date value picker go from 1-31 and the month picker go from Jan-Dec regardless of the year picker value or datemin or datemax
;; Also, the month and day pickers will only have limits applied if the pickers representing larger periods are modified or until
;; the glgui-datepicker-update-limits is called to set unlimited back to false
(define (glgui-datepicker-unlimited g wgt)
  (let ((mpicker (glgui-widget-get g wgt 'monthpicker))
        (dpicker (glgui-widget-get g wgt 'datepicker)))
    (glgui-widget-set! g mpicker 'valmin 0)
    (glgui-widget-set! g mpicker 'valmax 11)
    (glgui-widget-set! g dpicker 'valmin 1)
    (glgui-widget-set! g dpicker 'valmax 31)
    (glgui-widget-set! g wgt 'unlimited #t)))
  
;; Create a set of three value pickers for entering a date.
(define (glgui-datepicker g x y w h datemin datemax colorarrows colorhighlight colorvalue colorbg numfont monthfont)
  ;; Determine earliest and latest date
  (let* (;; if time on the device messed up, less than 31,000,000 (in 1970 or before) - then instead use 2012
         (now (if (> ##now 31000000) ##now 1329515282))
         (dmin (if datemin datemin 0))
         ;; Do not let datemax be larger than 32-bit latest date of Jan 19th, 2038
         (dmax (if datemax (if (> datemax 2147483600) 2147483600 datemax)
           ;; If no datemax set, use current time (unless messed up)
            (glgui:datepicker-get-end-of-next-year now)
         ))
         (current (if (and (>= now dmin) (<= now dmax)) now dmax))
         ;; Create the three vertical value pickers
         (dx (fix (/ (- w 2) 3)))
         (ypicker (glgui-verticalvaluepicker g x y dx h (glgui:datepicker-get-year dmin)
            (glgui:datepicker-get-year dmax) colorarrows colorhighlight colorvalue colorbg numfont 1))
         (mpicker (glgui-verticalvaluepicker g (+ x dx 1) y dx h #f #f colorarrows colorhighlight colorvalue colorbg
            monthfont glgui:datepicker_months))
         (dpicker (glgui-verticalvaluepicker g (+ x (* dx 2) 2) y dx h 1 31 colorarrows colorhighlight colorvalue colorbg
            numfont 1))
         ;; Create this date widget
         (widget (glgui-widget-add g
           'x x
           'y y
           'w w
           'h h
           'callback #f
           'update-handle glgui:datepicker-update
           'hidden #f
           'datemin dmin
           ;; Set value to current date, unless not within the range
           'value current
           'defaultvalue current
           'datemax dmax
           'colorarrows colorarrows
           'colorhighlight colorhighlight
           'colorvalue colorvalue
           'colorbg colorbg
           'numfnt numfont
           'monthfnt monthfont
           ;; Topdown can be set to true to reverse the order of dates (down arrow to get to later dates instead of up arrow)
           'topdown #f
           ;; Display order sets the order of the pickers from left to right
           'displayorder GUI_YEAR_MONTH_DAY
           'unlimited #f
           'datepicker dpicker
           'monthpicker mpicker
           'yearpicker ypicker
          )))
      ;; The pickers can cycle through from December to January and from 1 to 28/29/30/31
      (glgui-widget-set! g dpicker 'cycle #t)
      (glgui-widget-set! g mpicker 'cycle #t)
      ;; Set topdown and values for all pickers
      (glgui:datepicker-update g widget 'topdown #f)
      (glgui:datepicker-update g widget 'value current)
      ;; Hook into the callback of the pickers
      (glgui-widget-set! g mpicker 'callback (glgui:datepicker-callback widget mpicker dpicker ypicker))
      (glgui-widget-set! g dpicker 'callback (glgui:datepicker-callback widget mpicker dpicker ypicker))
      (glgui-widget-set! g ypicker 'callback (glgui:datepicker-callback widget mpicker dpicker ypicker))
      ;; Call the callback in order to restrict pickers if near the end or start of the date range
      ((glgui:datepicker-callback widget mpicker dpicker ypicker) g mpicker)
   ;; Make sure to still return the widget
    widget))
;; eof