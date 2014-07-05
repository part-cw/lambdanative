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

;; date entry widget consisting of three vertical wheels for month, date (of month), and year

;; Months of the year
(define glgui:datewheel_months `("Jan" "Feb"  "Mar" "Apr" "May" "June" "July" "Aug" "Sept" "Oct" "Nov" "Dec"))
(define glgui:datewheel_month_lengths `(31 28 31 30 31 30 31 31 30 31 30 31))

;; Constants for the order of the date wheels
(define GUI_YEAR_MONTH_DAY 1)
(define GUI_DAY_MONTH_YEAR 2)
(define GUI_MONTH_DAY_YEAR 2)

;; Allows an app to change the month strings into another language. This will be for all the date wheels in the app.
(define (glgui-datewheel-update-months strlist)
  (set! glgui:datewheel_months strlist))

;; Public shortcut procedure to call the one below to enforce the limits
(define (glgui-datawheel-update-limits gui wgt)
  (let ((mwheel (glgui-widget-get g wgt 'monthwheel))
        (dwheel (glgui-widget-get g wgt 'datewheel))
        (ywheel (glgui-widget-get g wgt 'yearwheel)))
    (glgui-widget-set! gui wgt 'unlimited #f)
    (glgui:datewheel-update-wheel-limits gui wgt mwheel dwheel ywheel #t #t)))

;; Updates the limits of the month, day and year wheels based on a new value or date limits
(define (glgui:datewheel-update-wheel-limits gui widget mwheel dwheel ywheel limitday? limitmonth?)
  ;; Get the month, day and year as numbers
  (let* ((month (+ (fix (glgui-widget-get gui mwheel 'value)) 1))
         (day (fix (glgui-widget-get gui dwheel 'value)))
         (year (fix (glgui-widget-get gui ywheel 'value)))
         (dmin (glgui-widget-get gui widget 'datemin))
         (eyear (fix (string->number (seconds->string dmin "%Y"))))
         (dmax (glgui-widget-get gui widget 'datemax))
         (lyear (fix (string->number (seconds->string dmax "%Y"))))
         (oldvalue (glgui-widget-get gui widget 'value)))
    
         ;; Check if we are in the earliest year - ie. need to limit the other wheels (depending on limitday and limitmonth)
         (if (fx= year eyear)
           (let ((emonth (fix (string->number (seconds->string dmin "%m")))))
             ;; Make sure month isn't earlier than allowed
             (if (and limitmonth? (fx< month emonth)) (set! month emonth))
             ;; Limit available months
             (if limitmonth?
               (glgui-widget-set! gui mwheel 'valmin (- emonth 1)))
             ;; Check if we are in the earliest month (of the earliest year)
             (if (= month emonth)
               (let ((eday (fix (string->number (seconds->string dmin "%d")))))
                 ;; Make sure day isn't earlier than allowed
                 (if (and limitday? (fx< day eday)) (set! day eday))
                 ;; Limit available days
                 (if limitday?
                   (glgui-widget-set! gui dwheel 'valmin eday)))
               ;; Otherwise no limit on earliest day of the month
               (glgui-widget-set! gui dwheel 'valmin 1)))
           ;; Otherwise no limit on earliest month or day of the month
           (begin 
             (glgui-widget-set! gui mwheel 'valmin 0)
             (glgui-widget-set! gui dwheel 'valmin 1)))
         ;; Check if we are in the latest year - ie. need to limit the other wheels
         (if (fx= year lyear)
           (let ((lmonth (fix (string->number (seconds->string dmax "%m")))))
             ;; Make sure month isn't later than allowed
             (if (and limitmonth? (fx> month lmonth)) (set! month lmonth))
             ;; Limit available months
             (if limitmonth?
               (glgui-widget-set! gui mwheel 'valmax (- lmonth 1)))
             ;; Check if we are in the latest month (of the latest year)
             (if (= month lmonth)
               (let ((lday (fix (string->number (seconds->string dmax "%d")))))
                  ;; Make sure day isn't later than allowed
                  (if (and limitday? (fx> day lday)) (set! day lday))
                  ;; Limit available days
                  (if limitday?
                    (glgui-widget-set! gui dwheel 'valmax lday)))
               ;; Otherwise no limit on latest day of the month - except the number of days in the month
               (let ((maxday
                        (if (fx= month 2)
                          ;; If February, check if leap year, leap year is divisible by 4, but not by 100, unless by 400. ie. 1896, 2000, 2004 are leap years. 1900, 1901, 1999 are not
                          (if (fx= (modulo year 4) 0) (if (fx= (modulo year 100) 0) (if (fx= (modulo year 400) 0) 29 28) 29) 28)
                          ;; Normally just use lengths from list
                          (list-ref glgui:datewheel_month_lengths (fx- month 1)))))
                   (if (and limitday? (fx> day maxday)) (set! day maxday))
                   (if limitday?
                     (glgui-widget-set! gui dwheel 'valmax maxday)))))
           ;; Otherwise no limit on latest month or day of the month
           (begin 
             (glgui-widget-set! gui mwheel 'valmax 11)
             (let ((maxday
                     (if (fx= month 2)
                       ;; If February, check if leap year, leap year is divisible by 4, but not by 100, unless by 400. ie. 1896, 2000, 2004 are leap years. 1900, 1901, 1999 are not
                       (if (fx= (modulo year 4) 0) (if (fx= (modulo year 100) 0) (if (fx= (modulo year 400) 0) 29 28) 29) 28)
                       ;; Normally just use lengths from list
                       (list-ref glgui:datewheel_month_lengths (fx- month 1)))))
                  (if (and limitday? (fx> day maxday)) (set! day maxday))
                  (if limitday?
                    (glgui-widget-set! gui dwheel 'valmax maxday)))))
         ;; Get the date specified in the wheels as a string and set it as the value if it is different than the original
         (let* ((datestr (string-append (number->string month) "/" (number->string day) "/" (number->string year) " 00:00:00" ))
                ;; Get the new value in seconds
                (newvalue (string->seconds datestr "%m/%d/%Y %H:%M:%S")))
            (if (or (not oldvalue) (not (= oldvalue newvalue)))
              (glgui-widget-set! gui widget 'value newvalue))))
)

(define (glgui:datewheels-callback widget mwheel dwheel ywheel)
  ;; The wgt parameter in the lambda is the specific wheel
  (lambda (g wgt . x)
          ;; Limit the date wheel values if the month or year wheels just changed
    (let ((limitday? (or (eqv? wgt mwheel) (eqv? wgt ywheel)))
          ;; Limit the month wheel values if the year wheel just changed
          (limitmonth? (eqv? wgt ywheel)))
       ;; Update the value of the date wheels by building it from the individual wheels while possibly modifying the limits of the displayed wheels
       (glgui:datewheel-update-wheel-limits g widget mwheel dwheel ywheel limitday? limitmonth?))
    ;; Then call the callback for this component - if there is one
    (let ((cb (glgui-widget-get g widget 'callback)))
       (if cb (cb g widget)))))

(define (glgui:datewheels-get-day secs)
  (string->number (seconds->string secs "%d"))
)
(define (glgui:datewheels-get-month secs)
  ;; Subtract 1 to match indices, January is 1st month, but becomes 0
  (- (string->number (seconds->string secs "%m")) 1)
)
(define (glgui:datewheels-get-year secs)
  (string->number (seconds->string secs "%Y"))
)
(define (glgui:datewheels-get-end-of-next-year secs)
  (let ((year (+ (string->number (seconds->string secs "%Y")) 1)))
    (string->seconds (string-append "12/31/" (number->string year) " 00:00:00" ) "%m/%d/%Y %H:%M:%S"))
)

(define (glgui:datewheel-update g wgt id val)
  (let ((mwheel (glgui-widget-get g wgt 'monthwheel))
        (dwheel (glgui-widget-get g wgt 'datewheel))
        (ywheel (glgui-widget-get g wgt 'yearwheel)))
    (cond 
      ;; Directly update all subwheels for some parameters
      ((or (eqv? id 'y) (eqv? id 'hidden) (eqv? id 'topdown)  (eqv? id 'colorvalue) (eqv? id 'colorshade))
        (glgui-widget-set! g mwheel id val)
        (glgui-widget-set! g dwheel id val)
        (glgui-widget-set! g ywheel id val))
      ;; Change in value - update wheels
      ((eqv? id 'value)
        (if val
          (let ((enforcelimits (not (glgui-widget-get g wgt 'unlimited))))
            (glgui-widget-set! g mwheel 'value (glgui:datewheels-get-month val))
            (glgui-widget-set! g dwheel 'value (glgui:datewheels-get-day val))
            (glgui-widget-set! g ywheel 'value (glgui:datewheels-get-year val))
            ;; Then update the limits of the wheels based on the new values
            (glgui:datewheel-update-wheel-limits g wgt mwheel dwheel ywheel enforcelimits enforcelimits))))
      ;; Update wheel limits if max or min have changed, possibly update value too
      ((eqv? id 'datemin)
         (glgui-widget-set! g ywheel 'valmin (fix (string->number (seconds->string val "%Y"))))
         (let ((curvalue (glgui-widget-get g wgt 'value)))
           (if (and curvalue (< curvalue val))
             ;; If value is less than new minimum, update it (will trigger a call to this procedure recursively)
             (glgui-widget-set! g wgt 'value val) 
             ;; Otherwise just update the limits of the wheels
             (let ((enforcelimits (not (glgui-widget-get g wgt 'unlimited))))
               (glgui:datewheel-update-wheel-limits g wgt mwheel dwheel ywheel enforcelimits enforcelimits)))))
      ((eqv? id 'datemax)
         (glgui-widget-set! g ywheel 'valmax (fix (string->number (seconds->string val "%Y"))))
         (let ((curvalue (glgui-widget-get g wgt 'value)))
           (if (and curvalue (> curvalue val))
             ;; If value is more than the new maximum, update it (will trigger a call to this procedure recursively)
             (glgui-widget-set! g wgt 'value val)   
             ;; Otherwise just update the limits of the wheels
             (let ((enforcelimits (not (glgui-widget-get g wgt 'unlimited))))
               (glgui:datewheel-update-wheel-limits g wgt mwheel dwheel ywheel enforcelimits enforcelimits)))))
      ;; Update x or w or the order of the wheels
      ((or (eqv? id 'x) (eqv? id 'w) (eqv? id 'displayorder))
        (let* ((order (glgui-widget-get g wgt 'displayorder))
               (w (glgui-widget-get g wgt 'w))
               (x (glgui-widget-get g wgt 'x))
               (dx (fix (/ (- w 2) 3)))
               ;; Determine which wheel is in which order
               (first (cond 
                        ((fx= order GUI_DAY_MONTH_YEAR) dwheel)
                        ((fx= order GUI_YEAR_MONTH_DAY) ywheel)
                        ((fx= order GUI_MONTH_DAY_YEAR) mwheel)))
               (second (cond 
                        ((fx= order GUI_DAY_MONTH_YEAR) mwheel)
                        ((fx= order GUI_YEAR_MONTH_DAY) mwheel)
                        ((fx= order GUI_MONTH_DAY_YEAR) dwheel)))
               (third (cond 
                        ((fx= order GUI_DAY_MONTH_YEAR) ywheel)
                        ((fx= order GUI_YEAR_MONTH_DAY) dwheel)
                        ((fx= order GUI_MONTH_DAY_YEAR) ywheel))))
          (glgui-widget-set! g first 'w dx)
          (glgui-widget-set! g first 'x x)
          (glgui-widget-set! g second 'w dx)
          (glgui-widget-set! g second 'x (+ x dx 1))
          (glgui-widget-set! g third 'w dx)
          (glgui-widget-set! g third 'x (+ x (* 2 dx) 2))))
      ;; Update fonts
      ((eqv? id 'bignumfnt)
        (glgui-widget-set! g dwheel 'bigfnt val)
        (glgui-widget-set! g ywheel 'bigfnt val))
      ((eqv? id 'smlnumfnt)
        (glgui-widget-set! g dwheel 'smlfnt val)
        (glgui-widget-set! g ywheel 'smlfnt val))
      ((eqv? id 'bigmonthfnt)
        (glgui-widget-set! g mwheel 'bigfnt val))
      ((eqv? id 'smlmonthfnt)
        (glgui-widget-set! g mwheel 'smlfnt val))))
)

;; Make the date wheels go from 1-31 and the month wheel go from Jan-Dec regardless of the year wheel value or datemin or datemax
;; ;; Also, the month and day wheels will only have limits applied if the wheels representing larger periods are modified or until
;; the glgui-datewheel-update-limits is called to set unlimited back to false
(define (glgui-datewheels-unlimited g wgt)
  (let ((mwheel (glgui-widget-get g wgt 'monthwheel))
        (dwheel (glgui-widget-get g wgt 'datewheel)))
    (glgui-widget-set! g mwheel 'valmin 0)
    (glgui-widget-set! g mwheel 'valmax 11)
    (glgui-widget-set! g dwheel 'valmin 1)
    (glgui-widget-set! g dwheel 'valmax 31)
    (glgui-widget-set! g wgt 'unlimited #t)))
  
;; Create a set of three wheels for entering a date.
(define (glgui-datewheels g x y w h datemin datemax colorvalue colorshade numfont1 numfont2 monthfont1 monthfont2)
  ;; Determine earliest and latest date
  (let* (;; if time on the device messed up, less than 31,000,000 (in 1970 or before) - then instead use 2012
         (now (if (> ##now 31000000) ##now 1329515282))
         (dmin (if datemin datemin 0))
         ;; Do not let datemax be larger than 32-bit latest date of Jan 19th, 2038
         (dmax (if datemax (if (> datemax 2147483600) 2147483600 datemax)
           ;; If no datemax set, use current time (unless messed up)
            (glgui:datewheels-get-end-of-next-year now)
         ))
         (current (if (and (>= now dmin) (<= now dmax)) now dmax))
         ;; Create the three vertical wheels
         (dx (fix (/ (- w 2) 3)))
         (ywheel (glgui-verticalnumberwheel g x y dx 144 (glgui:datewheels-get-year dmin)
            (glgui:datewheels-get-year dmax) #f #f colorvalue colorshade numfont1 numfont2 #f 1))
         (mwheel (glgui-verticalnumberwheel g (+ x dx 1) y dx 144 #f #f #f #f colorvalue colorshade
            monthfont1 monthfont2 #f glgui:datewheel_months))
         (dwheel (glgui-verticalnumberwheel g (+ x (* dx 2) 2) y dx 144 1 31 #f #f colorvalue colorshade
            numfont1 numfont2 #f 1))
         ;; Create this date widget
         (widget (glgui-widget-add g
           'x x
           'y y
           'w w
           'h h
           'callback #f
           'update-handle glgui:datewheel-update
           'hidden #f
           'datemin dmin
           ;; Set value to current date, unless not within the range
           'value current
           'defaultvalue current
           'datemax dmax
           'colorvalue colorvalue
           'colorshade colorshade
           'bignumfnt numfont1
           'smlnumfnt numfont2
           'bigmonthfnt monthfont1
           'smlmonthfnt monthfont2
           ;; Topdown can be set to false to make the latest date be displayed at the top instead of the bottom
           'topdown #t
           ;; Display order sets the order of the wheels from left to right
           'displayorder GUI_YEAR_MONTH_DAY
           'unlimited #f
           'monthwheel mwheel
           'datewheel dwheel
           'yearwheel ywheel
          )))
      ;; The wheels can roll through from December to January and from 1 to 28/29/30/31
      (glgui-widget-set! g dwheel 'cycle #t)
      (glgui-widget-set! g mwheel 'cycle #t)
      ;; Set topdown and values for all wheels
      (glgui:datewheel-update g widget 'topdown #t)
      (glgui:datewheel-update g widget 'value current)
      ;; Hook into the callback of the wheels
      (glgui-widget-set! g mwheel 'callback (glgui:datewheels-callback widget mwheel dwheel ywheel))
      (glgui-widget-set! g dwheel 'callback (glgui:datewheels-callback widget mwheel dwheel ywheel))
      (glgui-widget-set! g ywheel 'callback (glgui:datewheels-callback widget mwheel dwheel ywheel))
      ;; Call the callback in order to restrict wheels if near the end or start of the date range
      ((glgui:datewheels-callback widget mwheel dwheel ywheel) g mwheel)
   ;; Make sure to still return the widget
    widget))
;; eof