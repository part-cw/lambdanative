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

;; share stores between processes using MQTT
;; this allows seamless integration of distributed data stores
;; 
;; example:
;; (store-mqtt-setup store 'host "127.0.0.1" 'port 1883 
;;   'subscribe '(("LocalTemp" "sensor/temperature" 2)("MyAccel "sensor/accelerometer" 0))
;;   'publish   '(("MyHR" "vitals/heartrate" 2 0) ("LocalSpO2" "vitals/spo2" 2 1))
;; )

(define mqtt-store:debuglevel 0)
(define (mqtt-store:log level . x)
   (if (>= mqtt-store:debuglevel level) (apply log-system (append (list "mqtt-store: ") x))))

(define mqtt-store:clearflag 'PLSCLEAR)
(define (mqtt-store:clearflag? x) (and (symbol? x) (eq? x 'PLSCLEAR)))

(define (mqtt-store:clear! store id)
  (mqtt-store:log 2 "mqtt-store:clear! " store id)
  (mqtt-store:set! store id mqtt-store:clearflag))

(define (mqtt-store:set! store id val . category)
  (mqtt-store:log 2 "mqtt-store:set! " store " " id " " val)
  (let ((mosq (store-ref store "mqtt:handle" #f)))
    (if mosq 
      (let* ((topic (store-ref store (string-append id ":topic") #f))
             (qos (if topic (store-ref store (string-append id ":qos") 0) #f))
             (retain (if topic (store-ref store (string-append id ":retain") 0) #f)))
        (if (and topic qos retain)
          (mqtt-publish mosq topic val qos retain))))))

(define (mqtt-store-setup store . x)
  (mqtt-store:log 1 "mqtt-store-setup " store " " x)
  (let ((m (apply mqtt-setup (append x (list 'handler (mqtt-store:callback store))))))
    (store-set! store "mqtt:handle" m)
    (let loop ((subs (table-ref m 'subscribe '())))
      (if (> (length subs) 0) 
        (let* ((entry (car subs))
               (localname (car entry))
               (topic (cadr entry))
               (qos (if (= (length entry) 3) (caddr entry) 0)))
          (table-set! m (string-append topic ":localname") localname)
          (mqtt-subscribe m topic qos)
          (loop (cdr subs)))))
    (let loop ((pubs (table-ref m 'publish '())))
      (if (> (length pubs) 0) 
        (let* ((entry (car pubs))
               (localname (car entry))
               (topic (cadr entry))
               (qos (if (>= (length entry) 3) (caddr entry) 0))
               (retain (if (= (length entry) 4) (cadddr entry) 0)))
           (store-set! store (string-append localname ":topic") topic)
           (store-set! store (string-append localname ":qos") qos)
           (store-set! store (string-append localname ":retain") retain)
           (loop (cdr pubs)))))
    (store-set-extern-handler! store mqtt-store:set!)
    (store-clear-extern-handler! store mqtt-store:clear!)
 ))

(define (mqtt-store:callback store)
  (lambda (topic msg)
    (mqtt-store:log 2 "mqtt-store:callback " store " " topic " " msg)
    (let* ((mqtt (store-ref store "mqtt:handle" #f))
           (k (if mqtt (table-ref mqtt (string-append topic ":localname") #f) #f))
           (v msg))
      (if k
        (let ((kl (string-length k)))
          (if (and (fx> kl 5) (string=? (substring k (fx- kl 5) kl) "Alarm")) (begin
            (mqtt-store:log 4 "mqtt-store:callback local alarm dispatch")
            (apply store-event-add (append (list store) (car v)))
          ) 
          (if (mqtt-store:clearflag? v)
            (begin 
              (mqtt-store:log 4 "mqtt-store:callback local clear dispatch")
              (store:clearlocal! store k)
            ) 
            (begin
              (mqtt-store:log 4 "mqtt-store:callback local set dispatch: " k "=" v)
              (store:setlocal! store k v)
            )
          )
        )) (mqtt-store:log 0 "mqtt-store:callback failed on topic=" topic " msg=" msg))
     )))

;; eof
