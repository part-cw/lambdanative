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
;; time related functions

(c-declare #<<end-of-c-declare

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <math.h>

double timezone_hours(unsigned long int t) {
  double tz=0;
  long dtime = 0;
  time_t tm1, tm2;
  struct tm *t1, *t2;
  tzset();
  tm1 = (time_t) t;
  t2 = gmtime(&tm1);
  tm2 = mktime(t2);
  t1 = localtime(&tm1);
  dtime = (long)(tm1 - tm2);
  tz = (dtime>=0?1.:-1.)*abs(dtime)/3600.;
#ifndef WIN32
  // adjust for daylight saving
  tz+=(t1->tm_isdst?1.:0.);
#endif
  return tz;
}

end-of-c-declare
)

(define timezone-hours (c-lambda (unsigned-long) double "timezone_hours"))

;; SRFI-19: Time Data Types and Procedures.
;;
;; Copyright (C) I/NET, Inc. (2000, 2002, 2003). All Rights Reserved.
;;
;; This document and translations of it may be copied and furnished to others,
;; and derivative works that comment on or otherwise explain it or assist in its
;; implementation may be prepared, copied, published and distributed, in whole or
;; in part, without restriction of any kind, provided that the above copyright
;; notice and this paragraph are included on all such copies and derivative works.
;; However, this document itself may not be modified in any way, such as by
;; removing the copyright notice or references to the Scheme Request For
;; Implementation process or editors, except as needed for the purpose of
;; developing SRFIs in which case the procedures for copyrights defined in the SRFI
;; process must be followed, or as required to translate it into languages other
;; than English.
;;
;; The limited permissions granted above are perpetual and will not be revoked
;; by the authors or their successors or assigns.
;;
;; This document and the information contained herein is provided on an "AS IS"
;; basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL WARRANTIES, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTY THAT THE USE OF THE
;; INFORMATION HEREIN WILL NOT INFRINGE ANY RIGHTS OR ANY IMPLIED WARRANTIES OF
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

(define (:optional args fallback)
  (if (= (length args) 0) fallback (car args)))

(define time-tai 'time-tai)
(define time-utc 'time-utc)
(define time-monotonic 'time-monotonic)
(define time-thread 'time-thread)
(define time-process 'time-process)
(define time-duration 'time-duration)

;;-- LOCALE dependent constants
(define tm:locale-number-separator ".")

(define tm:locale-abbr-weekday-vector (vector "Sun" "Mon" "Tue" "Wed"
                                              "Thu" "Fri" "Sat"))
(define tm:locale-long-weekday-vector (vector "Sunday" "Monday"
                                              "Tuesday" "Wednesday"
                                              "Thursday" "Friday"
                                              "Saturday"))
;; note empty string in 0th place.
(define tm:locale-abbr-month-vector   (vector "" "Jan" "Feb" "Mar"
                                              "Apr" "May" "Jun" "Jul"
                                              "Aug" "Sep" "Oct" "Nov"
                                              "Dec"))
(define tm:locale-long-month-vector   (vector "" "January" "February"
                                              "March" "April" "May"
                                              "June" "July" "August"
                                              "September" "October"
                                              "November" "December"))

(define tm:locale-pm "PM")
(define tm:locale-am "AM")
(define tm:locale-ampm-vector (vector tm:locale-am tm:locale-pm))

;; See date->string
(define tm:locale-date-time-format "~a ~b ~d ~H:~M:~S~z ~Y")
(define tm:locale-short-date-format "~m/~d/~y")
(define tm:locale-time-format "~H:~M:~S")
(define tm:iso-8601-date-time-format "~Y-~m-~dT~H:~M:~S~z")
;;-- Miscellaneous Constants.
;;-- only the tm:tai-epoch-in-jd might need changing if
;;   a different epoch is used.

(define tm:nano (expt 10 9))
(define tm:sid  86400)    ; seconds in a day
(define tm:sihd 43200)    ; seconds in a half day
(define tm:tai-epoch-in-jd 4881175/2) ; julian day number for 'the epoch'


;;; A Very simple Error system for the time procedures
;;;
(define tm:time-error-types
  '(invalid-clock-type
    unsupported-clock-type
    incompatible-time-types
    not-duration
    dates-are-immutable
    bad-date-format-string
    bad-date-template-string
    invalid-month-specification
    ))

(define (tm:time-error caller type value)
  (if (member type tm:time-error-types)
      (if value
	  (error caller "TIME-ERROR type ~S: ~S" type value)
	  (error caller "TIME-ERROR type ~S" type))
      (error caller "TIME-ERROR unsupported error type ~S" type)))


;; A table of leap seconds
;; See ftp://maia.usno.navy.mil/ser7/tai-utc.dat
;; and update as necessary.
;; this procedures reads the file in the abover
;; format and creates the leap second table
;; it also calls the almost standard, but not R5 procedures read-line
;; & open-input-string
;; ie (set! tm:leap-second-table (tm:read-tai-utc-date "tai-utc.dat"))

(define (tm:read-tai-utc-data filename)
  (define (convert-jd jd)
    (* (- (inexact->exact jd) tm:tai-epoch-in-jd) tm:sid))
  (define (convert-sec sec)
    (inexact->exact sec))
  (let ( (port (open-input-file filename))
	 (table '()) )
    (let loop ((line (read-line port)))
      (if (not (eof-object? line))
	  (begin
	    (let* ( (data (read (open-input-string (string-append "(" line ")"))))
		    (year (car data))
		    (jd   (cadddr (cdr data)))
		    (secs (cadddr (cdddr data))) )
	      (if (>= year 1972)
		  (set! table (cons (cons (convert-jd jd) (convert-sec secs)) table)))
	      (loop (read-line port))))))
    table))

;; each entry is ( utc seconds since epoch . # seconds to add for tai )
;; note they go higher to lower, and end in 1972.
(define tm:leap-second-table
 '((1483228800 . 37)
  (1435708800 . 36)
  (1341100800 . 35)
  (1230768000 . 34)
  (1136073600 . 33)
  (915148800 . 32)
  (867715200 . 31)
  (820454400 . 30)
  (773020800 . 29)
  (741484800 . 28)
  (709948800 . 27)
  (662688000 . 26)
  (631152000 . 25)
  (567993600 . 24)
  (489024000 . 23)
  (425865600 . 22)
  (394329600 . 21)
  (362793600 . 20)
  (315532800 . 19)
  (283996800 . 18)
  (252460800 . 17)
  (220924800 . 16)
  (189302400 . 15)
  (157766400 . 14)
  (126230400 . 13)
  (94694400 . 12)
  (78796800 . 11)
  (63072000 . 10)))

(define (read-leap-second-table filename)
  (set! tm:leap-second-table (tm:read-tai-utc-data filename))
  (values))


(define (tm:leap-second-delta utc-seconds)
  (letrec ( (lsd (lambda (table)
		   (cond
		    ((>= utc-seconds (caar table))
		     (cdar table))
		    (else (lsd (cdr table)))))) )
    (if (< utc-seconds  (* (- 1972 1970) 365 tm:sid)) 0
	(lsd  tm:leap-second-table))))

;; going from tai seconds to utc seconds ...
(define (tm:leap-second-neg-delta tai-seconds)
  (letrec ( (lsd (lambda (table)
		   (cond ((null? table) 0)
			 ((<= (cdar table) (- tai-seconds (caar table)))
			  (cdar table))
			 (else (lsd (cdr table)))))) )
    (if (< tai-seconds  (* (- 1972 1970) 365 tm:sid)) 0
	(lsd  tm:leap-second-table))))


;;; the time structure; creates the accessors, too.
;;; wf: changed to match srfi documentation. uses mzscheme structures & inspectors

(define-type srfi19:time type nanosecond second)

;; thanks, Martin Gasbichler ...

(define (copy-time time)
  (make-srfi19:time (srfi19:time-type time)
	     (srfi19:time-second time)
	     (srfi19:time-nanosecond time)))


;;; current-time

;;; specific time getters.
;;; these should be rewritten to be os specific.
;;
;; -- using gnu gettimeofday() would be useful here -- gets
;;    second + millisecond
;;    let's pretend we do, using mzscheme's current-seconds & current-milliseconds
;;    this is supposed to return utc.
;;


(define (current-seconds)
  (inexact->exact (floor (time->seconds (current-time)))))

(define (current-milliseconds)
  (inexact->exact (floor (* 1000 (time->seconds (current-time))))))

(define (tm:get-time-of-day)
  (values (current-seconds)
	  (abs (remainder (current-milliseconds) 1000))))

(define (tm:current-time-utc)
  (receive (seconds ms) (tm:get-time-of-day)
	   (make-srfi19:time  time-utc (* ms 10000) seconds )))

(define (tm:current-time-tai)
  (receive (seconds ms) (tm:get-time-of-day)
	   (make-srfi19:time time-tai
		      (* ms 10000)
		      (+ seconds (tm:leap-second-delta seconds))
		      )))



(define (tm:current-time-ms-time time-type proc)
  (let ((current-ms (proc)))
    (make-srfi19:time time-type
	       (* (remainder current-ms 1000) 10000)
	       (quotient current-ms 10000)
	       )))

;; -- we define it to be the same as tai.
;;    a different implemation of current-time-montonic
;;    will require rewriting all of the time-monotonic converters,
;;    of course.

(define (tm:current-time-monotonic)
  (receive (seconds ms) (tm:get-time-of-day)
	   (make-srfi19:time time-monotonic
		      (* ms 10000)
		      (+ seconds (tm:leap-second-delta seconds))
		      )))


(define (tm:current-time-thread)
  ;;(tm:current-time-ms-time time-process current-process-milliseconds))
  (error "Unimplemented"))

(define (tm:current-time-process)
  ;;(tm:current-time-ms-time time-process current-process-milliseconds))
  (error "Unimplemented"))

(define (current-time-tc . clock-type)
  (let ( (clock-type (:optional clock-type time-utc)) )
    (cond
      ((eq? clock-type time-tai) (tm:current-time-tai))
      ((eq? clock-type time-utc) (tm:current-time-utc))
      ((eq? clock-type time-monotonic) (tm:current-time-monotonic))
      ((eq? clock-type time-thread) (tm:current-time-thread))
      ((eq? clock-type time-process) (tm:current-time-process))
      (else (tm:time-error 'current-time 'invalid-clock-type clock-type)))))



;; -- time resolution
;; this is the resolution of the clock in nanoseconds.
;; this will be implementation specific.

(define (time-resolution . clock-type)
  (let ((clock-type (:optional clock-type time-utc)))
    (cond
      ((eq? clock-type time-tai) 10000)
      ((eq? clock-type time-utc) 10000)
      ((eq? clock-type time-monotonic) 10000)
      ((eq? clock-type time-thread) 10000)
      ((eq? clock-type time-process) 10000)
      (else (tm:time-error 'time-resolution 'invalid-clock-type clock-type)))))

;; -- time comparisons

(define (tm:time-compare-check time1 time2 caller)
  (if (or (not (and (srfi19:time? time1) (srfi19:time? time2)))
	  (not (eq? (srfi19:time-type time1) (srfi19:time-type time2))))
      (tm:time-error caller 'incompatible-time-types #f)
      #t))

(define (time=? time1 time2)
  (tm:time-compare-check time1 time2 'time=?)
  (and (= (srfi19:time-second time1) (srfi19:time-second time2))
       (= (srfi19:time-nanosecond time1) (srfi19:time-nanosecond time2))))

(define (time>? time1 time2)
  (tm:time-compare-check time1 time2 'time>?)
  (or (> (srfi19:time-second time1) (srfi19:time-second time2))
      (and (= (srfi19:time-second time1) (srfi19:time-second time2))
	   (> (srfi19:time-nanosecond time1) (srfi19:time-nanosecond time2)))))

(define (time<? time1 time2)
  (tm:time-compare-check time1 time2 'time<?)
  (or (< (srfi19:time-second time1) (srfi19:time-second time2))
      (and (= (srfi19:time-second time1) (srfi19:time-second time2))
	   (< (srfi19:time-nanosecond time1) (srfi19:time-nanosecond time2)))))

(define (time>=? time1 time2)
  (tm:time-compare-check time1 time2 'time>=?)
  (or (>= (srfi19:time-second time1) (srfi19:time-second time2))
      (and (= (srfi19:time-second time1) (srfi19:time-second time2))
	   (>= (srfi19:time-nanosecond time1) (srfi19:time-nanosecond time2)))))

(define (time<=? time1 time2)
  (tm:time-compare-check time1 time2 'time<=?)
  (or (<= (srfi19:time-second time1) (srfi19:time-second time2))
      (and (= (srfi19:time-second time1) (srfi19:time-second time2))
	   (<= (srfi19:time-nanosecond time1) (srfi19:time-nanosecond time2)))))

;; -- time arithmetic

(define (tm:time->nanoseconds time)
  (define (sign1 n)
    (if (negative? n) -1 1))
  (+ (* (srfi19:time-second time) tm:nano)
      (srfi19:time-nanosecond time)))

(define (tm:nanoseconds->time time-type nanoseconds)
  (make-srfi19:time time-type
             (remainder nanoseconds tm:nano)
             (quotient nanoseconds tm:nano)))

(define (tm:nanoseconds->values nanoseconds)
  (values (abs (remainder nanoseconds tm:nano))
          (quotient nanoseconds tm:nano)))

(define (tm:time-difference time1 time2 time3)
  (if (or (not (and (srfi19:time? time1) (srfi19:time? time2)))
	  (not (eq? (srfi19:time-type time1) (srfi19:time-type time2))))
      (tm:time-error 'time-difference 'incompatible-time-types #f))
  (srfi19:time-type-set! time3 time-duration)
  (if (time=? time1 time2)
      (begin
	(srfi19:time-second-set! time3 0)
	(srfi19:time-nanosecond-set! time3 0))
      (receive
       (nanos secs)
       (tm:nanoseconds->values (- (tm:time->nanoseconds time1)
                                  (tm:time->nanoseconds time2)))
       (srfi19:time-second-set! time3 secs)
       (srfi19:time-nanosecond-set! time3 nanos)))
  time3)

(define (time-difference time1 time2)
  (tm:time-difference time1 time2 (make-srfi19:time #f #f #f)))

(define (time-difference! time1 time2)
  (tm:time-difference time1 time2 time1))

(define (tm:add-duration time1 duration time3)
  (if (not (and (srfi19:time? time1) (srfi19:time? duration)))
      (tm:time-error 'add-duration 'incompatible-time-types #f))
  (if (not (eq? (srfi19:time-type duration) time-duration))
      (tm:time-error 'add-duration 'not-duration duration)
      (let ( (sec-plus (+ (srfi19:time-second time1) (srfi19:time-second duration)))
	     (nsec-plus (+ (srfi19:time-nanosecond time1) (srfi19:time-nanosecond duration))) )
	(let ((r (remainder nsec-plus tm:nano))
	      (q (quotient nsec-plus tm:nano)))
          ; (set-time-type! time3 (srfi19:time-type time1))
	  (if (negative? r)
	      (begin
		(srfi19:time-second-set! time3 (+ sec-plus q -1))
		(srfi19:time-nanosecond-set! time3 (+ tm:nano r)))
	      (begin
		(srfi19:time-second-set! time3 (+ sec-plus q))
		(srfi19:time-nanosecond-set! time3 r)))
	  time3))))

(define (add-duration time1 duration)
  (tm:add-duration time1 duration (make-srfi19:time (srfi19:time-type time1) #f #f)))

(define (add-duration! time1 duration)
  (tm:add-duration time1 duration time1))

(define (tm:subtract-duration time1 duration time3)
  (if (not (and (srfi19:time? time1) (srfi19:time? duration)))
      (tm:time-error 'add-duration 'incompatible-time-types #f))
  (if (not (eq? (srfi19:time-type duration) time-duration))
      (tm:time-error 'tm:subtract-duration 'not-duration duration)
      (let ( (sec-minus  (- (srfi19:time-second time1) (srfi19:time-second duration)))
	     (nsec-minus (- (srfi19:time-nanosecond time1) (srfi19:time-nanosecond duration))) )
	(let ((r (remainder nsec-minus tm:nano))
	      (q (quotient nsec-minus tm:nano)))
	  (if (negative? r)
	      (begin
		(srfi19:time-second-set! time3 (- sec-minus q 1))
		(srfi19:time-nanosecond-set! time3 (+ tm:nano r)))
	      (begin
		(srfi19:time-second-set! time3 (- sec-minus q))
		(srfi19:time-nanosecond-set! time3 r)))
	  time3))))

(define (subtract-duration time1 duration)
  (tm:subtract-duration time1 duration (make-srfi19:time (srfi19:time-type time1) #f #f)))

(define (subtract-duration! time1 duration)
  (tm:subtract-duration time1 duration time1))


;; -- converters between types.

(define (tm:time-tai->time-utc! time-in time-out caller)
  (if (not (eq? (srfi19:time-type time-in) time-tai))
      (tm:time-error caller 'incompatible-time-types time-in))
  (srfi19:time-type-set! time-out time-utc)
  (srfi19:time-nanosecond-set! time-out (srfi19:time-nanosecond time-in))
  (srfi19:time-second-set!     time-out (- (srfi19:time-second time-in)
				    (tm:leap-second-neg-delta
				     (srfi19:time-second time-in))))
  time-out)

(define (time-tai->time-utc time-in)
  (tm:time-tai->time-utc! time-in (make-srfi19:time #f #f #f) 'time-tai->time-utc))


(define (time-tai->time-utc! time-in)
  (tm:time-tai->time-utc! time-in time-in 'time-tai->time-utc!))


(define (tm:time-utc->time-tai! time-in time-out caller)
  (if (not (eq? (srfi19:time-type time-in) time-utc))
      (tm:time-error caller 'incompatible-time-types time-in))
  (srfi19:time-type-set! time-out time-tai)
  (srfi19:time-nanosecond-set! time-out (srfi19:time-nanosecond time-in))
  (srfi19:time-second-set!     time-out (+ (srfi19:time-second time-in)
				    (tm:leap-second-delta
				     (srfi19:time-second time-in))))
  time-out)


(define (time-utc->time-tai time-in)
  (tm:time-utc->time-tai! time-in (make-srfi19:time #f #f #f) 'time-utc->time-tai))

(define (time-utc->time-tai! time-in)
  (tm:time-utc->time-tai! time-in time-in 'time-utc->time-tai!))

;; -- these depend on time-monotonic having the same definition as time-tai!
(define (time-monotonic->time-utc time-in)
  (if (not (eq? (srfi19:time-type time-in) time-monotonic))
      (tm:time-error 'time-monotonic->time-utc 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (srfi19:time-type-set! ntime time-tai)
    (tm:time-tai->time-utc! ntime ntime 'time-monotonic->time-utc)))

(define (time-monotonic->time-utc! time-in)
  (if (not (eq? (srfi19:time-type time-in) time-monotonic))
      (tm:time-error 'time-monotonic->time-utc! 'incompatible-time-types time-in))
  (srfi19:time-type-set! time-in time-tai)
  (tm:time-tai->time-utc! time-in time-in 'time-monotonic->time-utc))

(define (time-monotonic->time-tai time-in)
  (if (not (eq? (srfi19:time-type time-in) time-monotonic))
      (tm:time-error 'time-monotonic->time-tai 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (srfi19:time-type-set! ntime time-tai)
    ntime))

(define (time-monotonic->time-tai! time-in)
  (if (not (eq? (srfi19:time-type time-in) time-monotonic))
      (tm:time-error 'time-monotonic->time-tai! 'incompatible-time-types time-in))
  (srfi19:time-type-set! time-in time-tai)
  time-in)

(define (time-utc->time-monotonic time-in)
  (if (not (eq? (srfi19:time-type time-in) time-utc))
      (tm:time-error 'time-utc->time-monotonic 'incompatible-time-types time-in))
  (let ((ntime (tm:time-utc->time-tai! time-in (make-srfi19:time #f #f #f)
				       'time-utc->time-monotonic)))
    (srfi19:time-type-set! ntime time-monotonic)
    ntime))


(define (time-utc->time-monotonic! time-in)
  (if (not (eq? (srfi19:time-type time-in) time-utc))
      (tm:time-error 'time-utc->time-montonic! 'incompatible-time-types time-in))
  (let ((ntime (tm:time-utc->time-tai! time-in time-in
				       'time-utc->time-monotonic!)))
    (srfi19:time-type-set! ntime time-monotonic)
    ntime))


(define (time-tai->time-monotonic time-in)
  (if (not (eq? (srfi19:time-type time-in) time-tai))
      (tm:time-error 'time-tai->time-monotonic 'incompatible-time-types time-in))
  (let ((ntime (copy-time time-in)))
    (srfi19:time-type-set! ntime time-monotonic)
    ntime))

(define (time-tai->time-monotonic! time-in)
  (if (not (eq? (srfi19:time-type time-in) time-tai))
      (tm:time-error 'time-tai->time-monotonic!  'incompatible-time-types time-in))
  (srfi19:time-type-set! time-in time-monotonic)
  time-in)


;; -- date structures

(define-type date nanosecond second minute hour day month year zone-offset)

;; redefine setters

(define tm:set-date-nanosecond! date-nanosecond-set!)
(define tm:set-date-second! date-second-set!)
(define tm:set-date-minute! date-minute-set!)
(define tm:set-date-hour! date-hour-set!)
(define tm:set-date-day! date-day-set!)
(define tm:set-date-month! date-month-set!)
(define tm:set-date-year! date-year-set!)
(define tm:set-date-zone-offset! date-zone-offset-set!)

;; gives the julian day which starts at noon.
(define (tm:encode-julian-day-number day month year)
  (let* ((a (quotient (- 14 month) 12))
	 (y (- (- (+ year 4800) a) (if (negative? year) -1 0)))
	 (m (- (+ month (* 12 a)) 3)))
    (+ day
       (quotient (+ (* 153 m) 2) 5)
       (* 365 y)
       (quotient y 4)
       (- (quotient y 100))
       (quotient y 400)
       -32045)))

(define (tm:char-pos char str index len)
  (cond
   ((>= index len) #f)
   ((char=? (string-ref str index) char)
    index)
   (else
    (tm:char-pos char str (+ index 1) len))))


(define (tm:fractional-part r)
  (if (integer? r) "0"
      (let ((str (number->string (exact->inexact r))))
	(let ((ppos (tm:char-pos #\. str 0 (string-length str))))
	  (substring str  (+ ppos 1) (string-length str))))))


;; gives the seconds/date/month/year
(define (tm:decode-julian-day-number jdn)
  (let* ((days (truncate jdn))
	 (a (+ days 32044))
	 (b (quotient (+ (* 4 a) 3) 146097))
	 (c (- a (quotient (* 146097 b) 4)))
	 (d (quotient (+ (* 4 c) 3) 1461))
	 (e (- c (quotient (* 1461 d) 4)))
	 (m (quotient (+ (* 5 e) 2) 153))
	 (y (+ (* 100 b) d -4800 (quotient m 10))))
    (values ; seconds date month year
     (* (- jdn days) tm:sid)
     (+ e (- (quotient (+ (* 153 m) 2) 5)) 1)
     (+ m 3 (* -12 (quotient m 10)))
     (if (>= 0 y) (- y 1) y))
    ))


;; relies on the fact that we named our time zone accessor
;; differently from MzScheme's....
;; This should be written to be OS specific.

(define (tm:local-tz-offset) 0)

;;  (error "Not implemented")
;;  #;(date-time-zone-offset ;(seconds->date
;;                          (current-seconds)
;;                         ; )
;;                         ))

;; special thing -- ignores nanos
(define (tm:time->julian-day-number seconds tz-offset)
  (+ (/ (+ seconds
	   tz-offset
	   tm:sihd)
	tm:sid)
     tm:tai-epoch-in-jd))

(define (tm:find proc l)
  (if (null? l)
      #f
      (if (proc (car l))
	  #t
	  (tm:find proc (cdr l)))))

(define (tm:tai-before-leap-second? second)
  (tm:find (lambda (x)
	     (= second (- (+ (car x) (cdr x)) 1)))
	   tm:leap-second-table))

(define (tm:time->date time tz-offset ttype)
  (if (not (eq? (srfi19:time-type time) ttype))
      (tm:time-error 'time->date 'incompatible-time-types  time))
  (let* ( (offset (:optional tz-offset (tm:local-tz-offset))) )
    (receive (secs date month year)
	     (tm:decode-julian-day-number
	      (tm:time->julian-day-number (srfi19:time-second time) offset))
	     (let* ( (hours    (quotient secs (* 60 60)))
		     (rem      (remainder secs (* 60 60)))
		     (minutes  (quotient rem 60))
		     (seconds  (remainder rem 60)) )
	       (make-date (srfi19:time-nanosecond time)
			  seconds
			  minutes
			  hours
			  date
			  month
			  year
		  offset)))))

(define (time-tai->date time . tz-offset)
  (if (tm:tai-before-leap-second? (srfi19:time-second time))
      ;; if it's *right* before the leap, we need to pretend to subtract a second ...
      (let ((d (tm:time->date (subtract-duration! (time-tai->time-utc time) (make-srfi19:time time-duration 0 1)) tz-offset time-utc)))
	(tm:set-date-second! d 60)
	d)
      (tm:time->date (time-tai->time-utc time) tz-offset time-utc)))

(define (time-utc->date time . tz-offset)
  (tm:time->date time tz-offset time-utc))

;; again, time-monotonic is the same as time tai
(define (time-monotonic->date time . tz-offset)
  (tm:time->date time tz-offset time-monotonic))

(define (date->time-utc date)
  (let ( (nanosecond (date-nanosecond date))
	 (second (date-second date))
	 (minute (date-minute date))
	 (hour (date-hour date))
	 (day (date-day date))
	 (month (date-month date))
	 (year (date-year date))
	 (offset (date-zone-offset date)) )
    (let ( (jdays (- (tm:encode-julian-day-number day month year)
		     tm:tai-epoch-in-jd)) )
      (make-srfi19:time
       time-utc
       nanosecond
       (+ (* (- jdays 1/2) 24 60 60)
	  (* hour 60 60)
	  (* minute 60)
	  second
	  (- offset))
       ))))

(define (date->time-tai d)
  (if (= (date-second d) 60)
      (subtract-duration! (time-utc->time-tai! (date->time-utc d)) (make-srfi19:time time-duration 0 1))
      (time-utc->time-tai! (date->time-utc d))))

(define (date->time-monotonic date)
  (time-utc->time-monotonic! (date->time-utc date)))


(define (tm:leap-year? year)
  (or (= (modulo year 400) 0)
      (and (= (modulo year 4) 0) (not (= (modulo year 100) 0)))))

(define (leap-year? date)
  (tm:leap-year? (date-year date)))

;; tm:year-day fixed: adding wrong number of days.
(define  tm:month-assoc '((0 . 0) (1 . 31)  (2 . 59)   (3 . 90)   (4 . 120)
			  (5 . 151) (6 . 181)  (7 . 212)  (8 . 243)
			  (9 . 273) (10 . 304) (11 . 334)))

(define (tm:year-day day month year)
  (let ((days-pr (assoc (- month 1) tm:month-assoc)))
    (if (not days-pr)
	(tm:time-error 'date-year-day 'invalid-month-specification month))
    (if (and (tm:leap-year? year) (> month 2))
	(+ day (cdr days-pr) 1)
	(+ day (cdr days-pr)))))

(define (date-year-day date)
  (tm:year-day (date-day date) (date-month date) (date-year date)))

;; from calendar faq
(define (tm:week-day day month year)
  (let* ((a (quotient (- 14 month) 12))
	 (y (- year a))
	 (m (+ month (* 12 a) -2)))
    (modulo (+ day y (quotient y 4) (- (quotient y 100))
	       (quotient y 400) (quotient (* 31 m) 12))
	    7)))

(define (date-week-day date)
  (tm:week-day (date-day date) (date-month date) (date-year date)))

(define (tm:days-before-first-week date day-of-week-starting-week)
  (let* ( (first-day (make-date 0 0 0 0
                                1
                                1
                                (date-year date)
                                #f))
          (fdweek-day (date-week-day first-day))  )
    (modulo (- day-of-week-starting-week fdweek-day)
            7)))

(define (date-week-number date day-of-week-starting-week)
  (quotient (- (date-year-day date)
	       (tm:days-before-first-week  date day-of-week-starting-week))
	    7))

(define (current-date . tz-offset)
  (time-utc->date (current-time-tc time-utc)
		  (:optional tz-offset (tm:local-tz-offset))))

;; given a 'two digit' number, find the year within 50 years +/-
(define (tm:natural-year n)
  (let* ( (current-year (date-year (current-date)))
	  (current-century (* (quotient current-year 100) 100)) )
    (cond
      ((>= n 100) n)
      ((<  n 0) n)
      ((<=  (- (+ current-century n) current-year) 50)
       (+ current-century n))
      (else
       (+ (- current-century 100) n)))))

(define (date->julian-day date)
  (let ( (nanosecond (date-nanosecond date))
	 (second (date-second date))
	 (minute (date-minute date))
	 (hour (date-hour date))
	 (day (date-day date))
	 (month (date-month date))
	 (year (date-year date))
	 (offset (date-zone-offset date)) )
    (+ (tm:encode-julian-day-number day month year)
       (- 1/2)
       (+ (/ (/ (+ (* hour 60 60)
		   (* minute 60) second (/ nanosecond tm:nano)) tm:sid)
	     (- offset))))))

(define (date->modified-julian-day date)
  (- (date->julian-day date)
     4800001/2))


(define (time-utc->julian-day time)
  (if (not (eq? (srfi19:time-type time) time-utc))
      (tm:time-error 'time-utc->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (srfi19:time-second time) (/ (srfi19:time-nanosecond time) tm:nano))
	tm:sid)
     tm:tai-epoch-in-jd))

(define (time-utc->modified-julian-day time)
  (- (time-utc->julian-day time)
     4800001/2))

(define (time-tai->julian-day time)
  (if (not (eq? (srfi19:time-type time) time-tai))
      (tm:time-error 'time-tai->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (- (srfi19:time-second time)
	      (tm:leap-second-delta (srfi19:time-second time)))
	   (/ (srfi19:time-nanosecond time) tm:nano))
	tm:sid)
     tm:tai-epoch-in-jd))

(define (time-tai->modified-julian-day time)
  (- (time-tai->julian-day time)
     4800001/2))

;; this is the same as time-tai->julian-day
(define (time-monotonic->julian-day time)
  (if (not (eq? (srfi19:time-type time) time-monotonic))
      (tm:time-error 'time-monotonic->julian-day 'incompatible-time-types  time))
  (+ (/ (+ (- (srfi19:time-second time)
	      (tm:leap-second-delta (srfi19:time-second time)))
	   (/ (srfi19:time-nanosecond time) tm:nano))
	tm:sid)
     tm:tai-epoch-in-jd))


(define (time-monotonic->modified-julian-day time)
  (- (time-monotonic->julian-day time)
     4800001/2))


(define (julian-day->time-utc jdn)
  (let ( (nanosecs (* tm:nano tm:sid (- jdn tm:tai-epoch-in-jd))) )
    (make-srfi19:time time-utc
	       (remainder nanosecs tm:nano)
	       (floor (/ nanosecs tm:nano)))))

(define (julian-day->time-tai jdn)
  (time-utc->time-tai! (julian-day->time-utc jdn)))

(define (julian-day->time-monotonic jdn)
  (time-utc->time-monotonic! (julian-day->time-utc jdn)))

(define (julian-day->date jdn . tz-offset)
  (let ((offset (:optional tz-offset (tm:local-tz-offset))))
    (time-utc->date (julian-day->time-utc jdn) offset)))

(define (modified-julian-day->date jdn . tz-offset)
  (let ((offset (:optional tz-offset (tm:local-tz-offset))))
    (julian-day->date (+ jdn 4800001/2) offset)))

(define (modified-julian-day->time-utc jdn)
  (julian-day->time-utc (+ jdn 4800001/2)))

(define (modified-julian-day->time-tai jdn)
  (julian-day->time-tai (+ jdn 4800001/2)))

(define (modified-julian-day->time-monotonic jdn)
  (julian-day->time-monotonic (+ jdn 4800001/2)))

(define (current-julian-day)
  (time-utc->julian-day (current-time-tc time-utc)))

(define (current-modified-julian-day)
  (time-utc->modified-julian-day (current-time-tc time-utc)))

;; returns a string rep. of number N, of minimum LENGTH,
;; padded with character PAD-WITH. If PAD-WITH if #f,
;; no padding is done, and it's as if number->string was used.
;; if string is longer than LENGTH, it's as if number->string was used.

(define (tm:padding n pad-with length)
  (let* ( (str (number->string n))
	  (str-len (string-length str)) )
    (if (or (> str-len length)
            (not pad-with))
	str
	(let* ( (new-str (make-string length pad-with))
		(new-str-offset (- (string-length new-str)
				   str-len)) )
	  (do ((i 0 (+ i 1)))
            ((>= i (string-length str)))
            (string-set! new-str (+ new-str-offset i)
                         (string-ref str i)))
	  new-str))))

(define (tm:last-n-digits i n)
  (abs (remainder i (expt 10 n))))

(define (tm:locale-abbr-weekday n)
  (vector-ref tm:locale-abbr-weekday-vector n))

(define (tm:locale-long-weekday n)
  (vector-ref tm:locale-long-weekday-vector n))

(define (tm:locale-abbr-month n)
  (vector-ref tm:locale-abbr-month-vector n))

(define (tm:locale-long-month n)
  (vector-ref tm:locale-long-month-vector n))

(define (tm:vector-find needle haystack comparator)
  (let ((len (vector-length haystack)))
    (define (tm:vector-find-int index)
      (cond
        ((>= index len) #f)
        ((comparator needle (vector-ref haystack index)) index)
        (else (tm:vector-find-int (+ index 1)))))
    (tm:vector-find-int 0)))

(define (tm:locale-abbr-weekday->index string)
  (tm:vector-find string tm:locale-abbr-weekday-vector string=?))

(define (tm:locale-long-weekday->index string)
  (tm:vector-find string tm:locale-long-weekday-vector string=?))

(define (tm:locale-abbr-month->index string)
  (tm:vector-find string tm:locale-abbr-month-vector string=?))

(define (tm:locale-long-month->index string)
  (tm:vector-find string tm:locale-long-month-vector string=?))

(define (tm:locale-reader-ampm->index string)
  (tm:vector-find string tm:locale-ampm-vector string-ci=?))

;; do nothing.
;; Your implementation might want to do something...
;;
(define (tm:locale-print-time-zone date port)
  (values))

;; Again, locale specific.
(define (tm:locale-am/pm hr)
  (if (> hr 11) tm:locale-pm tm:locale-am))

(define (tm:tz-printer offset port)
  (cond
    ((= offset 0) (display "Z" port))
    ((negative? offset) (display "-" port))
    (else (display "+" port)))
  (if (not (= offset 0))
      (let ( (hours   (abs (quotient offset (* 60 60))))
	     (minutes (abs (quotient (remainder offset (* 60 60)) 60))) )
	(display (tm:padding hours #\0 2) port)
	(display (tm:padding minutes #\0 2) port))))

;; A table of output formatting directives.
;; the first time is the format char.
;; the second is a procedure that takes the date, a padding character
;; (which might be #f), and the output port.
;;
(define tm:directives
  (list
   (cons #\~ (lambda (date pad-with port) (display #\~ port)))

   (cons #\a (lambda (date pad-with port)
	       (display (tm:locale-abbr-weekday (date-week-day date))
			port)))
   (cons #\A (lambda (date pad-with port)
	       (display (tm:locale-long-weekday (date-week-day date))
			port)))
   (cons #\b (lambda (date pad-with port)
	       (display (tm:locale-abbr-month (date-month date))
			port)))
   (cons #\B (lambda (date pad-with port)
	       (display (tm:locale-long-month (date-month date))
			port)))
   (cons #\c (lambda (date pad-with port)
	       (display (date->string date tm:locale-date-time-format) port)))
   (cons #\d (lambda (date pad-with port)
	       (display (tm:padding (date-day date)
				    #\0 2)
                        port)))
   (cons #\D (lambda (date pad-with port)
	       (display (date->string date "~m/~d/~y") port)))
   (cons #\e (lambda (date pad-with port)
	       (display (tm:padding (date-day date)
				    #\space 2)
			port)))
   (cons #\f (lambda (date pad-with port)
	       (if (> (date-nanosecond date)
		      tm:nano)
		   (display (tm:padding (+ (date-second date) 1)
					pad-with 2)
			    port)
		   (display (tm:padding (date-second date)
					pad-with 2)
			    port))
	       (let* ((ns (tm:fractional-part (/
					       (date-nanosecond date)
					       tm:nano 1.0)))
		      (le (string-length ns)))
		 (if (> le 2)
		     (begin
		       (display tm:locale-number-separator port)
		       (display (substring ns 2 le) port))))))
   (cons #\h (lambda (date pad-with port)
	       (display (date->string date "~b") port)))
   (cons #\H (lambda (date pad-with port)
	       (display (tm:padding (date-hour date)
				    pad-with 2)
			port)))
   (cons #\I (lambda (date pad-with port)
	       (let ((hr (date-hour date)))
		 (cond
                   ((> hr 12) (display (tm:padding (- hr 12) pad-with 2) port))
		   ((= hr 0) (display (tm:padding 12 pad-with 2) port))
                   (else (display (tm:padding hr pad-with 2) port))
                  ))))
   (cons #\j (lambda (date pad-with port)
	       (display (tm:padding (date-year-day date)
				    pad-with 3)
			port)))
   (cons #\k (lambda (date pad-with port)
	       (display (tm:padding (date-hour date)
				    #\0 2)
                        port)))
   (cons #\l (lambda (date pad-with port)
	       (let ((hr (if (> (date-hour date) 12)
			     (- (date-hour date) 12) (date-hour date))))
		 (display (tm:padding hr  #\space 2)
			  port))))
   (cons #\m (lambda (date pad-with port)
	       (display (tm:padding (date-month date)
				    pad-with 2)
			port)))
   (cons #\M (lambda (date pad-with port)
	       (display (tm:padding (date-minute date)
				    pad-with 2)
			port)))
   (cons #\n (lambda (date pad-with port)
	       (newline port)))
   (cons #\N (lambda (date pad-with port)
	       (display (tm:padding (date-nanosecond date)
				    pad-with 9)
			port)))
   (cons #\p (lambda (date pad-with port)
	       (display (tm:locale-am/pm (date-hour date)) port)))
   (cons #\r (lambda (date pad-with port)
	       (display (date->string date "~I:~M:~S ~p") port)))
   (cons #\s (lambda (date pad-with port)
	       (display (srfi19:time-second (date->time-utc date)) port)))
   (cons #\S (lambda (date pad-with port)
	       (if (> (date-nanosecond date)
		      tm:nano)
                   (display (tm:padding (+ (date-second date) 1)
                                        pad-with 2)
                            port)
                   (display (tm:padding (date-second date)
                                        pad-with 2)
                            port))))
   (cons #\t (lambda (date pad-with port)
	       (display (integer->char 9) port)))
   (cons #\T (lambda (date pad-with port)
	       (display (date->string date "~H:~M:~S") port)))
   (cons #\U (lambda (date pad-with port)
	       (if (> (tm:days-before-first-week date 0) 0)
		   (display (tm:padding (+ (date-week-number date 0) 1)
					#\0 2) port)
		   (display (tm:padding (date-week-number date 0)
					#\0 2) port))))
   (cons #\V (lambda (date pad-with port)
	       (display (tm:padding (date-week-number date 1)
				    #\0 2) port)))
   (cons #\w (lambda (date pad-with port)
	       (display (date-week-day date) port)))
   (cons #\x (lambda (date pad-with port)
	       (display (date->string date tm:locale-short-date-format) port)))
   (cons #\X (lambda (date pad-with port)
	       (display (date->string date tm:locale-time-format) port)))
   (cons #\W (lambda (date pad-with port)
	       (if (> (tm:days-before-first-week date 1) 0)
		   (display (tm:padding (+ (date-week-number date 1) 1)
					#\0 2) port)
		   (display (tm:padding (date-week-number date 1)
					#\0 2) port))))
   (cons #\y (lambda (date pad-with port)
	       (display (tm:padding (tm:last-n-digits
				     (date-year date) 2)
				    pad-with
				    2)
			port)))
   (cons #\Y (lambda (date pad-with port)
	       (display (date-year date) port)))
   (cons #\z (lambda (date pad-with port)
	       (tm:tz-printer (date-zone-offset date) port)))
   (cons #\Z (lambda (date pad-with port)
	       (tm:locale-print-time-zone date port)))
   (cons #\1 (lambda (date pad-with port)
	       (display (date->string date "~Y-~m-~d") port)))
   (cons #\2 (lambda (date pad-with port)
	       (display (date->string date "~k:~M:~S~z") port)))
   (cons #\3 (lambda (date pad-with port)
	       (display (date->string date "~k:~M:~S") port)))
   (cons #\4 (lambda (date pad-with port)
	       (display (date->string date "~Y-~m-~dT~k:~M:~S~z") port)))
   (cons #\5 (lambda (date pad-with port)
	       (display (date->string date "~Y-~m-~dT~k:~M:~S") port)))
   ))


(define (tm:get-formatter char)
  (let ( (associated (assoc char tm:directives)) )
    (if associated (cdr associated) #f)))

(define (tm:date-printer date index format-string str-len port)
  (if (>= index str-len)
      (values)
      (let ( (current-char (string-ref format-string index)) )
	(if (not (char=? current-char #\~))
	    (begin
	      (display current-char port)
	      (tm:date-printer date (+ index 1) format-string str-len port))

	    (if (= (+ index 1) str-len) ; bad format string.
		(tm:time-error 'tm:date-printer 'bad-date-format-string
			       format-string)
                (let ( (pad-char? (string-ref format-string (+ index 1))) )
                  (cond
                    ((char=? pad-char? #\-)
                     (if (= (+ index 2) str-len) ; bad format string.
                         (tm:time-error 'tm:date-printer 'bad-date-format-string
                                        format-string)
                         (let ( (formatter (tm:get-formatter
                                            (string-ref format-string
                                                        (+ index 2)))) )
                           (if (not formatter)
                               (tm:time-error 'tm:date-printer 'bad-date-format-string
                                              format-string)
                               (begin
                                 (formatter date #f port)
                                 (tm:date-printer date (+ index 3)
                                                  format-string str-len port))))))

                    ((char=? pad-char? #\_)
                     (if (= (+ index 2) str-len) ; bad format string.
                         (tm:time-error 'tm:date-printer 'bad-date-format-string
                                        format-string)
                         (let ( (formatter (tm:get-formatter
                                            (string-ref format-string
                                                        (+ index 2)))) )
                           (if (not formatter)
                               (tm:time-error 'tm:date-printer 'bad-date-format-string
                                              format-string)
                               (begin
                                 (formatter date #\space port)
                                 (tm:date-printer date (+ index 3)
                                                  format-string str-len port))))))
                    (else
                     (let ( (formatter (tm:get-formatter
                                        (string-ref format-string
                                                    (+ index 1)))) )
                       (if (not formatter)
                           (tm:time-error 'tm:date-printer 'bad-date-format-string
                                          format-string)
                           (begin
                             (formatter date #\0 port)
                             (tm:date-printer date (+ index 2)
                                              format-string str-len port))))))))))))


(define (date->string date .  format-string)
  (let ( (str-port (open-output-string))
	 (fmt-str (:optional format-string "~c")) )
    (tm:date-printer date 0 fmt-str (string-length fmt-str) str-port)
    (get-output-string str-port)))

(define (tm:char->int ch)
  (cond
    ((char=? ch #\0) 0)
    ((char=? ch #\1) 1)
    ((char=? ch #\2) 2)
    ((char=? ch #\3) 3)
    ((char=? ch #\4) 4)
    ((char=? ch #\5) 5)
    ((char=? ch #\6) 6)
    ((char=? ch #\7) 7)
    ((char=? ch #\8) 8)
    ((char=? ch #\9) 9)
    (else (tm:time-error 'string->date 'bad-date-template-string
                         (list "Non-integer character" ch )))))

;; read an integer upto n characters long on port; upto -> #f if any length
(define (tm:integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
      (if (or (eof-object? ch)
              (not (char-numeric? ch))
              (and upto (>= nchars  upto )))
          accum
          (accum-int port (+ (* accum 10) (tm:char->int (read-char
                                                         port))) (+
                                                                  nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-integer-reader upto)
  (lambda (port)
    (tm:integer-reader upto port)))

;; read an fractional integer upto n characters long on port; upto -> #f if any length
;;
;; The return value is normalized to upto decimal places. For example, if upto is 9 and
;; the string read is "123", the return value is 123000000.
(define (tm:fractional-integer-reader upto port)
  (define (accum-int port accum nchars)
    (let ((ch (peek-char port)))
     (if (or (eof-object? ch)
     	(not (char-numeric? ch))
     	(and upto (>= nchars  upto )))
         (* accum (expt 10 (- upto nchars)))
         (accum-int port (+ (* accum 10) (tm:char->int (read-char port))) (+ nchars 1)))))
  (accum-int port 0 0))

(define (tm:make-fractional-integer-reader upto)
  (lambda (port)
    (tm:fractional-integer-reader upto port)))


;; read *exactly* n characters and convert to integer; could be padded
(define (tm:integer-reader-exact n port)
  (let ( (padding-ok #t) )
    (define (accum-int port accum nchars)
      (let ((ch (peek-char port)))
	(cond
          ((>= nchars n) accum)
          ((eof-object? ch)
           (tm:time-error 'string->date 'bad-date-template-string
                          "Premature ending to integer read."))
          ((char-numeric? ch)
           (set! padding-ok #f)
           (accum-int port (+ (* accum 10) (tm:char->int (read-char
                                                          port)))
                      (+ nchars 1)))
          (padding-ok
           (read-char port) ; consume padding
           (accum-int port accum (+ nchars 1)))
          (else ; padding where it shouldn't be
           (tm:time-error 'string->date 'bad-date-template-string
			  "Non-numeric characters in integer read.")))))
    (accum-int port 0 0)))


(define (tm:make-integer-exact-reader n)
  (lambda (port)
    (tm:integer-reader-exact n port)))

(define (tm:zone-reader port)
  (let ( (offset 0)
	 (positive? #f) )
    (let ( (ch (read-char port)) )
      (if (eof-object? ch)
	  (tm:time-error 'string->date 'bad-date-template-string
			 (list "Invalid time zone +/-" ch)))
      (if (or (char=? ch #\Z) (char=? ch #\z))
	  0
	  (begin
	    (cond
              ((char=? ch #\+) (set! positive? #t))
              ((char=? ch #\-) (set! positive? #f))
              (else
               (tm:time-error 'string->date 'bad-date-template-string
                              (list "Invalid time zone +/-" ch))))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
		  (tm:time-error 'string->date 'bad-date-template-string
                                 (list "Invalid time zone number" ch)))
	      (set! offset (* (tm:char->int ch)
			      10 60 60)))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
		  (tm:time-error 'string->date 'bad-date-template-string
				 (list "Invalid time zone number" ch)))
	      (set! offset (+ offset (* (tm:char->int ch)
					60 60))))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
		  (tm:time-error 'string->date 'bad-date-template-string
				 (list "Invalid time zone number" ch)))
	      (set! offset (+ offset (* (tm:char->int ch)
					10 60))))
	    (let ((ch (read-char port)))
	      (if (eof-object? ch)
		  (tm:time-error 'string->date 'bad-date-template-string
				 (list "Invalid time zone number" ch)))
	      (set! offset (+ offset (* (tm:char->int ch)
					60))))
	    (if positive? offset (- offset)))))))

;; looking at a char, read the char string, run thru indexer, return index
(define (tm:locale-reader port indexer)
  (let ( (string-port (open-output-string)) )
    (define (read-char-string)
      (let ((ch (peek-char port)))
	(if (char-alphabetic? ch)
	    (begin (write-char (read-char port) string-port)
		   (read-char-string))
	    (get-output-string string-port))))
    (let* ( (str (read-char-string))
	    (index (indexer str)) )
      (if index index (tm:time-error 'string->date
				     'bad-date-template-string
				     (list "Invalid string for " indexer))))))

(define (tm:make-locale-reader indexer)
  (lambda (port)
    (tm:locale-reader port indexer)))

(define (tm:make-char-id-reader char)
  (lambda (port)
    (if (char=? char (read-char port))
	char
	(tm:time-error 'string->date
		       'bad-date-template-string
		       "Invalid character match."))))

;; A List of formatted read directives.
;; Each entry is a list.
;; 1. the character directive;
;; a procedure, which takes a character as input & returns
;; 2. #t as soon as a character on the input port is acceptable
;; for input,
;; 3. a port reader procedure that knows how to read the current port
;; for a value. Its one parameter is the port.
;; 4. a action procedure, that takes the value (from 3.) and some
;; object (here, always the date) and (probably) side-effects it.
;; In some cases (e.g., ~A) the action is to do nothing

(define tm:read-directives
  (let ( (ireader4 (tm:make-integer-reader 4))
	 (ireader2 (tm:make-integer-reader 2))
	 (fireader9 (tm:make-fractional-integer-reader 9))
	 (ireaderf (tm:make-integer-reader #f))
	 (eireader2 (tm:make-integer-exact-reader 2))
	 (eireader4 (tm:make-integer-exact-reader 4))
	 (locale-reader-abbr-weekday (tm:make-locale-reader
				      tm:locale-abbr-weekday->index))
	 (locale-reader-long-weekday (tm:make-locale-reader
				      tm:locale-long-weekday->index))
	 (locale-reader-abbr-month   (tm:make-locale-reader
				      tm:locale-abbr-month->index))
	 (locale-reader-long-month   (tm:make-locale-reader
				      tm:locale-long-month->index))
   (locale-reader-ampm (tm:make-locale-reader
              tm:locale-reader-ampm->index))
	 (char-fail (lambda (ch) #t))
	 (do-nothing (lambda (val object) (values)))
	 )

    (list
     (list #\~ char-fail (tm:make-char-id-reader #\~) do-nothing)
     (list #\a char-alphabetic? locale-reader-abbr-weekday do-nothing)
     (list #\A char-alphabetic? locale-reader-long-weekday do-nothing)
     (list #\b char-alphabetic? locale-reader-abbr-month
           (lambda (val object)
             (tm:set-date-month! object val)))
     (list #\B char-alphabetic? locale-reader-long-month
           (lambda (val object)
             (tm:set-date-month! object val)))
     (list #\d char-numeric? ireader2 (lambda (val object)
                                        (tm:set-date-day!
                                         object val)))
     (list #\e char-fail eireader2 (lambda (val object)
                                     (tm:set-date-day! object val)))
     (list #\h char-alphabetic? locale-reader-abbr-month
           (lambda (val object)
             (tm:set-date-month! object val)))
     (list #\H char-numeric? ireader2 (lambda (val object)
                                        (tm:set-date-hour! object val)))
     (list #\I char-numeric? ireader2 (lambda (val object)
                                        (tm:set-date-hour! object (modulo val 12))))
     (list #\k char-fail eireader2 (lambda (val object)
                                     (tm:set-date-hour! object val)))
     (list #\m char-numeric? ireader2 (lambda (val object)
                                        (tm:set-date-month! object val)))
     (list #\M char-numeric? ireader2 (lambda (val object)
                                        (tm:set-date-minute!
                                         object val)))
     (list #\N char-numeric? fireader9 (lambda (val object)
					 (tm:set-date-nanosecond! object val)))
     (list #\p char-alphabetic? locale-reader-ampm (lambda (val object)
           (if (fx= val 1) (tm:set-date-hour! object (+ (date-hour object) 12)))))
     (list #\S char-numeric? ireader2 (lambda (val object)
                                        (tm:set-date-second! object val)))
     (list #\y char-fail eireader2
           (lambda (val object)
             (tm:set-date-year! object (tm:natural-year val))))
     (list #\Y char-numeric? ireader4 (lambda (val object)
                                        (tm:set-date-year! object val)))
     (list #\z (lambda (c)
                 (or (char=? c #\Z)
                     (char=? c #\z)
                     (char=? c #\+)
                     (char=? c #\-)))
           tm:zone-reader (lambda (val object)
                            (tm:set-date-zone-offset! object val)))
     )))

(define (tm:string->date date index format-string str-len port template-string)
  (define (skip-until port skipper)
    (let ((ch (peek-char port)))
      (if (eof-object? ch)
	  (tm:time-error 'string->date 'bad-date-format-string template-string)
	  (if (not (skipper ch))
	      (begin (read-char port) (skip-until port skipper))))))
  (if (>= index str-len)
      (begin
	(values))
      (let ( (current-char (string-ref format-string index)) )
	(if (not (char=? current-char #\~))
	    (let ((port-char (read-char port)))
	      (if (or (eof-object? port-char)
		      (not (char=? current-char port-char)))
		  (tm:time-error 'string->date 'bad-date-format-string template-string))
	      (tm:string->date date (+ index 1) format-string str-len port template-string))
	    ;; otherwise, it's an escape, we hope
	    (if (> (+ index 1) str-len)
		(tm:time-error 'string->date 'bad-date-format-string template-string)
		(let* ( (format-char (string-ref format-string (+ index 1)))
			(format-info (assoc format-char tm:read-directives)) )
		  (if (not format-info)
		      (tm:time-error 'string->date 'bad-date-format-string template-string)
		      (begin
			(let ((skipper (cadr format-info))
			      (reader  (caddr format-info))
			      (actor   (cadddr format-info)))
			  (skip-until port skipper)
			  (let ((val (reader port)))
			    (if (eof-object? val)
				(tm:time-error 'string->date 'bad-date-format-string template-string)
				(actor val date)))
			  (tm:string->date date (+ index 2) format-string  str-len port template-string))))))))))

(define (string->date input-string template-string)
  (define (tm:date-ok? date)
    (and (date-nanosecond date)
	 (date-second date)
	 (date-minute date)
	 (date-hour date)
	 (date-day date)
	 (date-month date)
	 (date-year date)
	 (date-zone-offset date)))
  (let* ((today (current-date))
         (newdate (make-date 0 0 0 0 (date-day today) (date-month today) (date-year today) 0)))
    (tm:string->date newdate
		     0
		     template-string
		     (string-length template-string)
		     (open-input-string input-string)
		     template-string)
    (if (tm:date-ok? newdate)
	newdate
	(tm:time-error 'string->date 'bad-date-format-string (list "Incomplete date read. " newdate template-string)))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; make format percent sign into tilde
(define (tm:tildify str)
  (let loop ((cs (string->list str))(res '()))
    (if (= (length cs) 0) (list->string res)
      (loop (cdr cs) (append res (list
        (if (char=? (car cs) #\%) #\~ (car cs))))))))

(define (string->seconds str fmt . tz0)
  (let* ((date (string->date str (tm:tildify fmt)))
         (t (date->time-monotonic date))
         (s (srfi19:time-second t))
         (ns (srfi19:time-nanosecond t))
         (t2 (- s (tm:leap-second-delta s)))
         (tz (if (= (length tz0) 1) (car tz0) (timezone-hours (fix t2))))
         (utc (+ 0.0 (* tz -3600.) t2 (* ns 1.0e-9)))
         ;; timezone-hours assumes t is UTC, this causes issues if one is DST but not both
         (tz2 (timezone-hours (fix utc)))
         (tzdiff (- tz tz2))
         (utc2 (if (= tzdiff 0) utc (+ utc (* tzdiff 3600.))))
        )
    utc2))

(define (seconds->string sec0 fmt . tz0)
  (let* ((tz (if (= (length tz0) 1) (car tz0) (timezone-hours (fix sec0))))
         (sec (+ sec0 (* tz 3600.)))
         (s (inexact->exact (floor sec)))
         (ns (inexact->exact (floor (* 1.0e9 (- sec s)))))
         (t (make-srfi19:time time-monotonic ns s))
         (d (time-monotonic->date t)))
    (date->string d (tm:tildify fmt))))

(define (localseconds->string sec fmt) (seconds->string sec fmt 0.))

(define (time->string t fmt) (seconds->string (time->seconds t) fmt))

(define (secondselapsed->string arg1 . arg2)
  (seconds->string arg1 (if (= (length arg2) 1) (car arg2) "%T") 0.))

(define (time->timestamp arg1) (time->string arg1 "%y%m%d-%H%M%S"))

(define (seconds->timestamp arg1) (seconds->string arg1 "%y%m%d-%H%M%S"))

(define (current-time-seconds) (time->seconds (current-time)))

;; eof
