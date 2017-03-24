#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2017, University of British Columbia
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

;; report battery level and charging state

(c-declare  #<<end-of-c-declare

#ifdef IOS
  int ios_battery_level(void);
  int ios_battery_charging(void);
#endif

#ifdef ANDROID
  int android_battery_level(void);
  int android_battery_charging(void);
#endif

static int battery_level(void) {
#ifdef IOS
  return ios_battery_level();
#elif ANDROID
  return android_battery_level();
#else
  return 0;
#endif
}

static int battery_charging(void) {
#ifdef IOS
  return ios_battery_charging();
#elif ANDROID
  return android_battery_charging();
#else
  return 0;
#endif
}

end-of-c-declare
)

(define battery:debuglevel 0)
(define (battery:log level . x)
   (if (>= battery:debuglevel level) (apply log-system (append (list "battery: ") x))))

(define battery-level (c-lambda () int "battery_level"))
(define battery-charging (c-lambda () int "battery_charging"))

;; inject battery events if appropriate
(if (procedure? (with-exception-catcher (lambda (e) #f) (lambda () (eval 'event-push))))
  (thread-start! (make-thread (lambda () 
    (let loop () 
      (battery:log 2 "level=" (battery-level) " charging=" (battery-charging))
      ((eval 'event-push) (eval 'EVENT_BATTERY) (battery-level) (battery-charging))
      (thread-sleep! 1.0)
      (loop))))))

;; eof
