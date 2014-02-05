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

;; bindings for the mosquitto MQTT library
;; MQTT is a publish/subscribe protocol for the Internet of Things (IoT)

(define mqtt:debuglevel 0)
(define (mqtt:log level . x)
   (if (>= mqtt:debuglevel level) (apply log-system (append (list "mqtt: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <mosquitto.h>

void _scm_msg_callback(void *, char *, int);
void log_c(char *);

static const struct mosquitto_message *msg=0;

void _mosq_copy_payload(unsigned char *data)
{
  if (msg) { memcpy(data,msg->payload,msg->payloadlen); msg=0; }
}

void _mosq_msg_callback(struct mosquitto *mosq, void *obj, const struct mosquitto_message *message)
{
  while (msg) { }  // lock
  msg=message; 
  _scm_msg_callback(mosq, message->topic, message->payloadlen);
}

void _mosq_log_callback(struct mosquitto *mosq, void *obj, int level, const char *str)
{
  char buf[1024]="mosquitto: ";
  strncat(buf,str,1023);
  // MOSQ_LOG_INFO MOSQ_LOG_NOTICE MOSQ_LOG_WARNING MOSQ_LOG_ERR MOSQ_LOG_DEBUG
  log_c(buf);
}

struct mosquitto *_mqtt_new(char *idstr, int clean_session)
{
  struct mosquitto *mosq = mosquitto_new(idstr,clean_session,0);
  if (mosq) {
//    mosquitto_log_callback_set(mosq,_mosq_log_callback);
    mosquitto_message_callback_set(mosq,_mosq_msg_callback);
  } else {
   log_c("mosquitto: FATAL: mosquitto_new() failed");
  }
  return mosq;
}

unsigned int _mqtt_pid() { return (unsigned int)getpid(); } 

end-of-c-declare
)

(define MOSQ_ERR_CONN_PENDING ((c-lambda () int "___result = MOSQ_ERR_CONN_PENDING;")))
(define MOSQ_ERR_SUCCESS ((c-lambda () int "___result = MOSQ_ERR_SUCCESS;")))
(define MOSQ_ERR_NOMEM ((c-lambda () int "___result = MOSQ_ERR_NOMEM;")))
(define MOSQ_ERR_PROTOCOL ((c-lambda () int "___result = MOSQ_ERR_PROTOCOL;")))
(define MOSQ_ERR_INVAL ((c-lambda () int "___result = MOSQ_ERR_INVAL;")))
(define MOSQ_ERR_NO_CONN ((c-lambda () int "___result = MOSQ_ERR_NO_CONN;")))
(define MOSQ_ERR_CONN_REFUSED ((c-lambda () int "___result = MOSQ_ERR_CONN_REFUSED;")))
(define MOSQ_ERR_NOT_FOUND ((c-lambda () int "___result = MOSQ_ERR_NOT_FOUND;")))
(define MOSQ_ERR_CONN_LOST ((c-lambda () int "___result = MOSQ_ERR_CONN_LOST;")))
(define MOSQ_ERR_TLS ((c-lambda () int "___result = MOSQ_ERR_TLS;")))
(define MOSQ_ERR_PAYLOAD_SIZE ((c-lambda () int "___result = MOSQ_ERR_PAYLOAD_SIZE;")))
(define MOSQ_ERR_NOT_SUPPORTED ((c-lambda () int "___result = MOSQ_ERR_NOT_SUPPORTED;")))
(define MOSQ_ERR_AUTH ((c-lambda () int "___result = MOSQ_ERR_AUTH;")))
(define MOSQ_ERR_ACL_DENIED ((c-lambda () int "___result = MOSQ_ERR_ACL_DENIED;")))
(define MOSQ_ERR_UNKNOWN ((c-lambda () int "___result = MOSQ_ERR_UNKNOWN;")))
(define MOSQ_ERR_ERRNO ((c-lambda () int "___result = MOSQ_ERR_ERRNO;")))
(define MOSQ_ERR_EAI ((c-lambda () int "___result = MOSQ_ERR_EAI;")))

(define mosq:errlut (let loop ((syms
  '(MOSQ_ERR_CONN_PENDING MOSQ_ERR_SUCCESS MOSQ_ERR_NOMEM MOSQ_ERR_PROTOCOL MOSQ_ERR_INVAL
    MOSQ_ERR_NO_CONN MOSQ_ERR_CONN_REFUSED MOSQ_ERR_NOT_FOUND MOSQ_ERR_CONN_LOST 
    MOSQ_ERR_TLS MOSQ_ERR_PAYLOAD_SIZE MOSQ_ERR_NOT_SUPPORTED 
    MOSQ_ERR_AUTH MOSQ_ERR_ACL_DENIED MOSQ_ERR_UNKNOWN MOSQ_ERR_ERRNO MOSQ_ERR_EAI))(res '()))
      (if (= (length syms) 0) res (loop (cdr syms) 
         (append res (list (list (eval (car syms)) (symbol->string (car syms)))))))))

(define (mosq:error->string e)
  (let ((lu (assoc e mosq:errlut)))
    (if lu (cadr lu) (string-append "Unknown Error: " (number->string e)))))

(define mosq:lut (make-table))

;; prefix serialized data with magic "LN<1>"
(define mosq:magic (u8vector 76 78 1))

(define (mosq:encode msg)
  (mqtt:log 4 "mosq:encode " msg)
  (cond ((string? msg) (string->u8vector msg))
        ((number? msg) (string->u8vector (number->string msg)))
        (else (u8vector-append mosq:magic (object->u8vector msg)))))

(define (mosq:decode msg)
  (mqtt:log 4 "mosq:decode " msg)
  (let* ((msglen (u8vector-length msg))
         (maglen (u8vector-length mosq:magic)))
    (if (and (> msglen maglen) (equal? (subu8vector msg 0 maglen) mosq:magic))
      (u8vector->object (subu8vector msg maglen msglen))
        (let ((num (string->number (u8vector->string msg))))
          (if num num (u8vector->string msg))))))

(define mosq:copypayload (c-lambda (scheme-object) void 
  "_mosq_copy_payload(___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)));"))

(c-define (c-event mosq topic len) ((pointer void) char-string int) void "_scm_msg_callback" ""
  (mqtt:log 3 "enter _scm_mqtt_message: " topic " " len)
  (let* ((m (table-ref mosq:lut mosq #f))
         (h (if m (table-ref m 'handler #f) #f)))
    (if (procedure? h)
      (let ((u8data (make-u8vector len)))
        (mosq:copypayload u8data)
        (mqtt:log 3 "dispatch topic: " topic " length=" len)
        (h topic (mosq:decode u8data))))))

(c-initialize "mosquitto_lib_init();")

(define (mosq:new idstr clean_session)
  (mqtt:log 2 "mqtt-new " idstr " " clean_session)
  ((c-lambda (char-string int) (pointer void) "_mqtt_new") idstr clean_session))

(define (mosq:destroy mosq)
  (mqtt:log 2 "mosq:destroy")
  ((c-lambda ((pointer void)) void "mosquitto_destroy")))

(define (mosq:connect mosq host port keepalive)
  (mqtt:log 2 "mosq:connect " mosq " " host " " port " " keepalive)
  (let ((result ((c-lambda ((pointer void) char-string int int) int 
        ;;  "mosquitto_connect"
          "___result=mosquitto_connect_bind(___arg1,___arg2,___arg3,___arg4,NULL);"
          ) mosq host port keepalive)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_connect() failed with error " (mosq:error->string result)))
    result))

(define (mosq:publish mosq topic msg qos retain)
  (mqtt:log 3 "mosq:publish " mosq " " topic " " msg " " qos " " retain)
  (let* ((u8msg (mosq:encode msg))
         (result ((c-lambda ((pointer void) char-string int scheme-object int int) int
    "___result=mosquitto_publish(___arg1, 0, ___arg2, ___arg3,
      ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)), ___arg5, ___arg6);")
     mosq topic (u8vector-length u8msg) u8msg qos retain)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_publish() failed with error " (mosq:error->string result)))
    result))


(define (mosq:will_set mosq topic msg qos retain)
  (mqtt:log 3 "mosq:will_set " mosq " " topic " " msg " " qos " " retain)
  (let* ((u8msg (mosq:encode msg))
         (result ((c-lambda ((pointer void) char-string int scheme-object int int) int
    "___result=mosquitto_will_set(___arg1, ___arg2, ___arg3,
      ___CAST(void*,___BODY_AS(___arg4,___tSUBTYPED)), ___arg5, ___arg6);")
     mosq topic (u8vector-length u8msg) u8msg qos retain)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_publish() failed with error " (mosq:error->string result)))
    result))

(define (mosq:will_clear mosq)
  (mqtt:log 2 "mosq:will_clear")
  ((c-lambda ((pointer void)) int "mosquitto_will_clear") mosq))

(define (mosq:subscribe mosq topic qos)
  (mqtt:log 2 "mosq:subscribe " mosq " " topic " " qos)
  (let ((result ((c-lambda ((pointer void) char-string int) int
     "___result=mosquitto_subscribe(___arg1, 0, ___arg2, ___arg3);" )
      mosq topic qos)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_subscribe() failed with error " (mosq:error->string result)))
    result))

(define (mosq:unsubscribe mosq topic)
  (mqtt:log 2 "mosq:unsubscribe " mosq " " topic)
  (let ((result ((c-lambda ((pointer void) char-string) int
     "___result=mosquitto_unsubscribe(___arg1, 0, ___arg2);" )
      mosq topic)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_unsubscribe() failed with error " (mosq:error->string result)))
    result))

(define (mosq:loop mosq timeout max_packets)
  (mqtt:log 6 "mosq:loop " mosq " " timeout " " max_packets)
  (let ((result ((c-lambda ((pointer void) int int) int "mosquitto_loop") 
    mosq timeout max_packets)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_loop() failed with error " (mosq:error->string result)))
    result))

(define (mosq:username_pw_set mosq uname pw)
  (mqtt:log 2 "mosq:username_pw_set " mosq " " uname " " pw)
    (let ((result ((c-lambda ((pointer void) char-string char-string) int "mosquitto_username_pw_set") mosq uname pw)))
      (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_username_pw_set() failed with error " (mosq:error->string result)))
    result))

(define (mosq:disconnect mosq)
  (mqtt:log 2 "mosq:disconnect" mosq)
  (let ((result ((c-lambda ((pointer void)) int "___result=mosquitto_disconnect(___arg1);" ) mosq)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_disconnect() failed with error " (mosq:error->string result)))
    result))

(define (mosq:tls_psk_set mosq psk identity)
  (mqtt:log 2 "mosq:tls_psk_set" mosq " " psk " "  identity)
  (let ((result ((c-lambda ((pointer void) char-string char-string) int 
          "___result=mosquitto_tls_psk_set(___arg1,___arg2,___arg3,NULL);") mosq psk identity)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_tls_psk_set() failed with error " (mosq:error->string result)))
    result))

(define (mosq:tls_opts_set mosq cert_reqs tls_version)
  (mqtt:log 2 "mosq:tls_opts_set " mosq " " cert_reqs " " tls_version)
  (let ((result ((c-lambda ((pointer void) int char-string) int 
          "___result=mosquitto_tls_opts_set(___arg1,___arg2,___arg3,NULL);") mosq cert_reqs tls_version)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_tls_opts_set() failed with error " (mosq:error->string result)))
    result))

(define (mosq:tls_insecure_set mosq value)
  (mqtt:log 2 "mosq:tls_insecure_set " mosq " " value)
  (let ((result ((c-lambda ((pointer void) int) int 
          "___result=mosquitto_tls_insecure_set(___arg1,___arg2);") mosq value)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_tls_insecure_set() failed with error " (mosq:error->string result)))
    result))

(define (mosq:tls_set mosq cafile capath certfile keyfile)
  (mqtt:log 2 "mosq:tls_set" mosq " " cafile " " capath " " certfile " " keyfile)
  (let ((result ((c-lambda ((pointer void) char-string char-string char-string char-string) int
          "___result=mosquitto_tls_set(___arg1,___arg2,___arg3,___arg4,___arg5,NULL);") 
          mosq cafile capath certfile keyfile)))
    (if (not (fx= result MOSQ_ERR_SUCCESS))
       (mqtt:log 0 "ERROR: mosquitto_tls_set() failed with error " (mosq:error->string result)))
    result))

(define (mqtt:pid)
  (mqtt:log 2 "mqtt:pid")
  ((c-lambda () unsigned-int "_mqtt_pid")))

;; ------------------------------
;; simple mqtt interface with automatic reconnect, re-subscription and optional TLS PSK encryption

(define (mqtt-setup . args)
  (let ((t (make-table)))
    (let loop ((defs '(
       (clean-session 0)
       (host "127.0.0.1") 
       (port 1883) 
       (keepalive 10) 
       (timeout 10) 
       (max-packets 100) 
       (handler #f)
       (subscriptions ())
       (will #f) ;;  ("topic" "msg" qos retain)
       (username #f)
       (password #f)
       (tls-version "tlsv1.2")
       (tls-insecure #f)
       (psk #f) (psk_identity #f)
       (connected #f)
       (id #f)
       (mosq #f) 
      )))
       (if (> (length defs) 0) (begin
         (apply table-set! (append (list t) (car defs)))
         (loop (cdr defs)))))
    (let loop ((as args))
      (if (> (length as) 1) (begin
       (table-set! t (car as) (cadr as))
       (loop (cddr as)))))
    (let ((id (table-ref t 'id #f)))
      (if (not id) 
         (let* ((tmpid (string-append "LN/" (ipaddr->string (host-ipaddr)) "-" (number->string (mqtt:pid))))
                (tmplen (string-length tmpid)))
           (table-set! t 'id (substring tmpid 0 (min tmplen 23))))))
    (let ((mosq (mosq:new (table-ref t 'id #f) (table-ref t 'clean-session 0))))
      (table-set! t 'mosq  mosq)
      (table-set! mosq:lut mosq t)
      (thread-start! (make-safe-thread (lambda () (let loop ()
        (let ((mosq (table-ref t 'mosq #f))
              (connected (table-ref t 'connected #f)))
          (if mosq (table-set! t 'connected (fx= (if connected 
            (mosq:loop mosq (table-ref t 'timeout #f) (table-ref t 'max-packets #f))
            (begin 
              (let ((will (table-ref t 'will #f)))
                (if will (apply mosq:will_set (append (list mosq) will)) (mosq:will_clear mosq)))
              (let ((psk (table-ref t 'psk #f))
                    (psk-identity (table-ref t 'psk-identity #f))
                    (tls-version (table-ref t 'tls-version #f))
                    (tls-insecure (table-ref t 'tls-insecure #f)))
                (if (and psk psk-identity) (begin
                   (mosq:tls_insecure_set mosq (if tls-insecure 1 0))
                   (mosq:tls_psk_set mosq psk psk-identity)
                   (if tls-version (mosq:tls_opts_set mosq 1 tls-version))
                 )))
              (let ((res (mosq:connect mosq (table-ref t 'host #f)  (table-ref t 'port #f) (table-ref t 'keepalive #f))))
                (if (fx= res MOSQ_ERR_SUCCESS) 
                  (let loop2 ((subs (table-ref t 'subscriptions '())))
                     (if (> (length subs) 0) (begin
                       (apply mosq:subscribe (append (list mosq) (car subs))) (loop2 (cdr subs)))))) 
                res))) MOSQ_ERR_SUCCESS)))
          (thread-sleep! (if connected 0.01 1.0))
          (loop)))
       )))
     ) t))

(define (mqtt-subscribe t topic qos)
  (let ((connected (table-ref t 'connected #f)))
    (let ((subs (table-ref t 'subscriptions '()))) 
      (if (not (assoc topic subs)) (table-set! t 'subscriptions (append subs (list (list topic qos))))))
    (if connected (fx= (mosq:subscribe (table-ref t 'mosq #f) topic qos) MOSQ_ERR_SUCCESS)) #f))

(define (mqtt-unsubscribe t topic)
  (let ((connected (table-ref t 'connected #f)))
    (let loop ((subs (table-ref t 'subscriptions '()))(nsubs '()))
      (if (= (length subs) 0) (table-set! t 'subscriptions nsubs)
        (loop (cdr subs) (append nsubs (if (string=? (car (car subs)) topic) '() (list (car subs)))))))
    (if connected (fx= (mosq:unsubscribe (table-ref t 'mosq #f) topic) MOSQ_ERR_SUCCESS) #f)))

(define (mqtt-publish t topic msg qos retain)
  (let ((connected (table-ref t 'connected #f)))
    (if connected (fx= (mosq:publish (table-ref t 'mosq #f) topic msg qos retain) MOSQ_ERR_SUCCESS) #f)))

(define (mqtt-disconnect t)
  (let ((connected (table-ref t 'connected #f)))
    (if connected (mosq:disconnect (table-ref t 'mosq #f)))))

;; eof
