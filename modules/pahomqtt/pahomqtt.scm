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

(define pahomqtt:debuglevel 0)
(define (pahomqtt:log level . x)
   (if (>= pahomqtt:debuglevel level) (apply log-system (append (list "pahomqtt: ") x))))

(c-declare  #<<end-of-c-declare

// Note: callbacks to gambit are NOT recommended here because of the threading 

// #define DEBUG_PAHO 1

#ifdef DEBUG_PAHO
#define DMSG(fmt...) (fprintf(stderr,"DEBUG: pahomqtt: " fmt),fprintf(stderr,"\n"))
#else
#define DMSG(fmt...)
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <MQTTAsync.h>

void MQTTAsync_init();

#define PAHO_PENDING 0
#define PAHO_SUCCESS 1
#define PAHO_FAILURE -1
#define PAHO_UNKNOWN -2

#define PAHO_STATUS_SUBSCRIBE 1
#define PAHO_STATUS_UNSUBSCRIBE 2
#define PAHO_STATUS_CONNECT 3 
#define PAHO_STATUS_PUBLISH 4
#define PAHO_STATUS_MAX 5

#define PAHO_MSG_MAX 100

// ----------------------------------------------------------
// message and status buffering

struct paho_meta {
  MQTTAsync m;
  int status[PAHO_STATUS_MAX];
  MQTTAsync_message *msg_ring[PAHO_MSG_MAX];
  char *topic_ring[PAHO_MSG_MAX];
  int msg_head, msg_tail;
  struct paho_meta *nxt;  
};

static struct paho_meta *fst=0;
static struct paho_meta *lst=0;

static void paho_meta_add(MQTTAsync m)
{
  struct paho_meta *tmp = (struct paho_meta *)malloc(sizeof(struct paho_meta));
  if (!tmp) { DMSG("paho_meta_add: malloc() failed"); return; }
  tmp->m = m;
  tmp->nxt=0;
  int i;
  for (i=0;i<PAHO_STATUS_MAX;i++) tmp->status[i]=PAHO_UNKNOWN;
  for (i=0;i<PAHO_MSG_MAX;i++) { tmp->msg_ring[i]=0; tmp->topic_ring[i]=0; }
  tmp->msg_head=tmp->msg_tail=0;
  if (!fst) fst=tmp;
  if (lst) lst->nxt=tmp; 
  lst=tmp;
}

static void paho_meta_del(MQTTAsync m)
{
  struct paho_meta *tmp = fst, *prv=0;
  while (tmp) {
    if (tmp->m == m) {
      if (prv) prv->nxt=tmp->nxt;
      if (tmp==fst) fst=tmp->nxt;
      if (tmp==lst) lst=prv;
      free(tmp);
      break; 
    }
    prv=tmp;
    tmp=tmp->nxt;
  }
  if (tmp) free(tmp); else { DMSG("paho_meta_del: non-existing pointer %p", m); }
}

static int paho_meta_status_get(MQTTAsync m, int type)
{
  int res = PAHO_UNKNOWN;
  struct paho_meta *tmp = fst;
  while (tmp) {
    if (tmp->m == m) break;
    tmp=tmp->nxt;
  }
  if (tmp) { res = tmp->status[type]; } else { DMSG("paho_meta_status_get: handle %p not found", m); }
  return res;
}

static void paho_meta_status_set(MQTTAsync m, int type, int value)
{
  struct paho_meta *tmp = fst;
  while (tmp) {
    if (tmp->m == m) break;
    tmp=tmp->nxt;
  }
  if (tmp) { tmp->status[type]=value; } else { DMSG("paho_meta_status_set: handle %p not found", m); }
}

static void paho_meta_msg_push(void *context, char *topic, MQTTAsync_message *msg)
{
  DMSG("paho_meta_msg_push");
  struct paho_meta *tmp = fst;
  while (tmp) { if (tmp->m==context) break; tmp=tmp->nxt; } 
  if (!tmp) { DMSG("paho_meta_msg_push: context %p not found", context); return; }
  if (tmp->msg_ring[tmp->msg_head]) {
    DMSG("message queue overrun");
    MQTTAsync_freeMessage(&tmp->msg_ring[tmp->msg_head]);
    MQTTAsync_free(tmp->topic_ring[tmp->msg_head]);
    tmp->msg_ring[tmp->msg_head]=0; tmp->topic_ring[tmp->msg_head]=0;
  }
  tmp->msg_ring[tmp->msg_head]=msg;
  tmp->topic_ring[tmp->msg_head]=topic;
  tmp->msg_head++; if (tmp->msg_head==PAHO_MSG_MAX) tmp->msg_head=0;
}

static void paho_meta_msg_pop(void *context)
{
  DMSG("paho_meta_msg_pop");
  struct paho_meta *tmp = fst;
  while (tmp) { if (tmp->m==context) break; tmp=tmp->nxt; } 
  if (!tmp) { DMSG("paho_meta_msg_pop: context %p not found", context); return; }
  if (tmp->msg_tail!=tmp->msg_head) {
    if (tmp->msg_ring[tmp->msg_tail]) {
      MQTTAsync_freeMessage(&tmp->msg_ring[tmp->msg_tail]);
      MQTTAsync_free(tmp->topic_ring[tmp->msg_tail]);
    }
    tmp->msg_ring[tmp->msg_tail]=0;
    tmp->topic_ring[tmp->msg_tail]=0;
    tmp->msg_tail++; if (tmp->msg_tail==PAHO_MSG_MAX) tmp->msg_tail=0;
  }
}

static int paho_meta_msg_ready(void *context)
{
  struct paho_meta *tmp = fst;
  while (tmp) { if (tmp->m==context) break; tmp=tmp->nxt; } 
  if (!tmp) { DMSG("paho_meta_msg_ready: context %p not found", context); return -1; }
  int res=-1;
  if (tmp->msg_tail!=tmp->msg_head) {
    DMSG("paho_meta_msg_ready on context %p", context);
    res=tmp->msg_ring[tmp->msg_tail]->payloadlen;
  }
  return res;
}

static void paho_meta_msg_payload(void *context, unsigned char *data)
{
  DMSG("paho_meta_msg_payload");
  struct paho_meta *tmp = fst;
  while (tmp) { if (tmp->m==context) break; tmp=tmp->nxt; } 
  if (!tmp) { DMSG("paho_meta_msg_payload: context %p not found", context); return; }
  MQTTAsync_message *msg = tmp->msg_ring[tmp->msg_tail];
  if (msg) { memcpy(data,msg->payload,msg->payloadlen); }
  paho_meta_msg_pop(context);
}

static char *paho_meta_msg_topic(void *context)
{
  DMSG("paho_meta_msg_topic");
  struct paho_meta *tmp = fst;
  while (tmp) { if (tmp->m==context) break; tmp=tmp->nxt; } 
  if (!tmp) { DMSG("paho_meta_msg_topic: context %p not found", context); return 0; }
  return tmp->topic_ring[tmp->msg_tail];
}

// ----------------------------------------------------------

unsigned int _paho_pid() {  return (unsigned int)getpid; }

void _paho_destroy(void *context)
{
  DMSG("_paho_destroy");
  MQTTAsync client = (MQTTAsync)context;
  paho_meta_status_set(context, PAHO_STATUS_CONNECT,PAHO_FAILURE);
  MQTTAsync_destroy(&client);
  paho_meta_del(client);
}

void _paho_onDisconnect(void *context, MQTTAsync_successData* response)
{
  DMSG("_paho_onDisconnect"); 
  paho_meta_status_set(context, PAHO_STATUS_CONNECT,PAHO_FAILURE);
}

void _paho_disconnect(void *context)
{
  int rc;
  DMSG("_paho_disconnect"); 
  MQTTAsync client = (MQTTAsync)context;
  MQTTAsync_disconnectOptions opts = MQTTAsync_disconnectOptions_initializer;
  opts.onSuccess = _paho_onDisconnect;
  opts.context = client;
  paho_meta_status_set(context, PAHO_STATUS_CONNECT,PAHO_PENDING);
  if ((rc = MQTTAsync_disconnect(client, &opts)) != MQTTASYNC_SUCCESS) {
    paho_meta_status_set(context, PAHO_STATUS_CONNECT,PAHO_SUCCESS);
    DMSG("Failed to disconnect, return code %d\n", rc);
  }
}

static void _paho_onSubscribe(void* context, MQTTAsync_successData* response)
{
  DMSG("_paho_onSubscribe");
  paho_meta_status_set(context, PAHO_STATUS_SUBSCRIBE,PAHO_SUCCESS);
}

static void _paho_onSubscribeFailure(void* context, MQTTAsync_failureData* response)
{
  DMSG("_paho_onSubscribeFailure");
  DMSG("Subscribe failed, rc %d\n", response ? response->code : 0);
  paho_meta_status_set(context, PAHO_STATUS_SUBSCRIBE,PAHO_FAILURE);
}

static void _paho_connectionLost(void *context, char *cause)
{
  DMSG("_paho_connectionLost context=%p", context);
  paho_meta_status_set(context, PAHO_STATUS_CONNECT,PAHO_FAILURE);
}

static int _paho_onMsgArrived(void *context, char *topicName, int topicLen, MQTTAsync_message *message)
{
  DMSG("_paho_onMsgArrived from context %p msgid=%i", context, message->msgid);
  paho_meta_msg_push(context,topicName,message);
  return 1;
}

static void _paho_onConnectFailure(void* context, MQTTAsync_failureData* response)
{
  DMSG("_paho_onConnectFailure");
  paho_meta_status_set(context, PAHO_STATUS_CONNECT,PAHO_FAILURE);
}

void _paho_subscribe(void *context, char *topic, int qos)
{
  DMSG("_paho_subscribe");
  int rc;
  MQTTAsync client = (MQTTAsync)context;
  MQTTAsync_responseOptions opts = MQTTAsync_responseOptions_initializer;
  opts.onSuccess = _paho_onSubscribe;
  opts.onFailure = _paho_onSubscribeFailure;
  opts.context = client;
  paho_meta_status_set(context, PAHO_STATUS_SUBSCRIBE,PAHO_PENDING);
  if ((rc = MQTTAsync_subscribe(client, topic, qos, &opts)) != MQTTASYNC_SUCCESS) {
    DMSG("Failed to start subscribe, return code %d\n", rc);
    paho_meta_status_set(context, PAHO_STATUS_SUBSCRIBE,PAHO_FAILURE);
  }
}

static void _paho_onUnsubscribe(void* context, MQTTAsync_successData* response)
{
  DMSG("_paho_onUnsubscribe");
  paho_meta_status_set(context, PAHO_STATUS_UNSUBSCRIBE,PAHO_SUCCESS);
}
 
static void _paho_unsubscribe(void *context, char *topic)
{
  DMSG("_paho_unsubscribe");
  int rc;
  MQTTAsync client = (MQTTAsync)context;
  MQTTAsync_responseOptions opts = MQTTAsync_responseOptions_initializer;
  opts.onSuccess = _paho_onUnsubscribe;
  opts.context = client;
  paho_meta_status_set(context, PAHO_STATUS_UNSUBSCRIBE,PAHO_PENDING);
  if ((rc = MQTTAsync_unsubscribe(client, topic, &opts)) != MQTTASYNC_SUCCESS) {
    paho_meta_status_set(context, PAHO_STATUS_UNSUBSCRIBE,PAHO_FAILURE);
  }
}
 
static void _paho_onConnect(void* context, MQTTAsync_successData* response)
{ 
  DMSG("_paho_onConnect");
  paho_meta_status_set(context, PAHO_STATUS_CONNECT,PAHO_SUCCESS);
}

static void *_paho_connect(
   char *address, char* clientid, char* certfile,
   int timeout, int keepalive, int cleansession, 
   char *username, char *passwd,
   char *will_topic, char *will_msg,
   int will_qos, int will_retain
 )
{
  DMSG("_paho_connect");
  int rc;
  MQTTAsync client;
  rc = MQTTAsync_create(&client, address, clientid, MQTTCLIENT_PERSISTENCE_NONE, NULL);
  if (rc != MQTTASYNC_SUCCESS) { MQTTAsync_destroy(&client); client=0; goto _paho_connect_fail; }
  MQTTAsync_setCallbacks(client, client, _paho_connectionLost, _paho_onMsgArrived, NULL);
  paho_meta_add(client);

  MQTTAsync_willOptions wopts = MQTTAsync_willOptions_initializer;
  MQTTAsync_SSLOptions sslopts = MQTTAsync_SSLOptions_initializer;
  MQTTAsync_connectOptions conn_opts = MQTTAsync_connectOptions_initializer;
  conn_opts.keepAliveInterval = keepalive; // 20
  conn_opts.cleansession = cleansession; // 1 
  conn_opts.onSuccess = _paho_onConnect;
  conn_opts.onFailure = _paho_onConnectFailure;
  conn_opts.context = client;
  if (username) conn_opts.username=username;
  if (passwd)   conn_opts.password=passwd;
  if (certfile) {
    sslopts.trustStore=certfile;
    conn_opts.ssl=&sslopts;
  }
  if (will_topic) {
    conn_opts.will = &wopts;
    conn_opts.will->message = will_msg;
    conn_opts.will->qos = will_qos;
    conn_opts.will->retained = will_retain;
    conn_opts.will->topicName = will_topic;
  }
  paho_meta_status_set(client, PAHO_STATUS_CONNECT,PAHO_PENDING);
  if ((rc = MQTTAsync_connect(client, &conn_opts)) != MQTTASYNC_SUCCESS) {
     DMSG("Failed to start connect, return code %d\n", rc);
     paho_meta_status_set(client, PAHO_STATUS_CONNECT,PAHO_FAILURE);
     MQTTAsync_destroy(&client);
     client=0;
  }
_paho_connect_fail:
  return client;
}

static void _paho_onSend(void* context, MQTTAsync_successData* response)
{
  DMSG("_paho_onSend");
  if (response) { DMSG("Message with token value %d delivery confirmed\n", response->token); }
  paho_meta_status_set(context,PAHO_STATUS_PUBLISH,PAHO_SUCCESS);
}

void _paho_publish(void *context, char *topic, int qos, int retain, char *payload, int payloadlen)
{
  DMSG("_paho_publish");
  MQTTAsync client = (MQTTAsync)context;
  MQTTAsync_responseOptions opts = MQTTAsync_responseOptions_initializer;
  MQTTAsync_message pubmsg = MQTTAsync_message_initializer;
  opts.onSuccess = _paho_onSend;
  opts.context = client;
  pubmsg.payload = payload;
  pubmsg.payloadlen = payloadlen;
  pubmsg.qos = qos;
  pubmsg.retained = retain;
  int rc;
  paho_meta_status_set(client,PAHO_STATUS_PUBLISH,PAHO_PENDING);
  if ((rc = MQTTAsync_sendMessage(client, topic, &pubmsg, &opts)) != MQTTASYNC_SUCCESS) {
    paho_meta_status_set(client,PAHO_STATUS_PUBLISH,PAHO_FAILURE);
    DMSG("Failed to start sendMessage, return code %d\n", rc);
  }
}

// -------------------------

end-of-c-declare
)

(c-initialize "MQTTAsync_init();")

(define PAHO_PENDING ((c-lambda () int "___result=PAHO_PENDING;")))
(define PAHO_SUCCESS ((c-lambda () int "___result=PAHO_SUCCESS;")))
(define PAHO_FAILURE ((c-lambda () int "___result=PAHO_FAILURE;")))

(define PAHO_STATUS_SUBSCRIBE ((c-lambda () int "___result=PAHO_STATUS_SUBSCRIBE;")))
(define PAHO_STATUS_UNSUBSCRIBE ((c-lambda () int "___result=PAHO_STATUS_UNSUBSCRIBE;")))
(define PAHO_STATUS_CONNECT ((c-lambda () int "___result=PAHO_STATUS_CONNECT;")))
(define PAHO_STATUS_PUBLISH ((c-lambda () int "___result=PAHO_STATUS_PUBLISH;")))

(define (paho:pub-status h) ((c-lambda ((pointer void) int) int "paho_meta_status_get") h PAHO_STATUS_PUBLISH))
(define (paho:sub-status h) ((c-lambda ((pointer void) int) int "paho_meta_status_get") h PAHO_STATUS_SUBSCRIBE))
(define (paho:unsub-status h) ((c-lambda ((pointer void) int) int "paho_meta_status_get") h PAHO_STATUS_UNSUBSCRIBE))
(define (paho:con-status h) ((c-lambda ((pointer void) int) int "paho_meta_status_get") h PAHO_STATUS_CONNECT))

(define paho:connect (c-lambda (char-string char-string char-string int int int 
    char-string char-string char-string char-string int int) (pointer void) "_paho_connect"))
(define (paho:isconnected h) (fx= 1 ((c-lambda ((pointer void)) int  "___result=MQTTAsync_isConnected((MQTTAsync)___arg1);") h)))
(define paho:disconnect (c-lambda ((pointer void)) void "_paho_disconnect"))

(define paho:destroy (c-lambda ((pointer void)) void "_paho_destroy"))

(define (paho:publish handle topic qos retain u8data) 
   ((c-lambda ((pointer void) char-string int int scheme-object int) void 
       "_paho_publish(___arg1,___arg2,___arg3, ___arg4, ___CAST(void*,___BODY_AS(___arg5,___tSUBTYPED)),___arg6);")
      handle topic qos retain u8data (u8vector-length u8data)))

(define paho:subscribe  (c-lambda ((pointer void) char-string int) void "_paho_subscribe"))
(define paho:unsubscribe  (c-lambda ((pointer void) char-string) void "_paho_unsubscribe"))

(define paho:msg-payload (c-lambda ((pointer void) scheme-object) void
  "paho_meta_msg_payload(___arg1,___CAST(void*,___BODY_AS(___arg2,___tSUBTYPED)));"))

(define paho:msg-ready (c-lambda ((pointer void)) int "paho_meta_msg_ready"))
(define paho:msg-topic (c-lambda ((pointer void)) char-string "paho_meta_msg_topic"))

(define (paho:pid)
  (pahomqtt:log 2 "paho:pid")
  ((c-lambda () unsigned-int "_paho_pid")))

(define paho:lut (make-table))

;; prefix serialized data with magic "LN<1>"
(define paho:magic (u8vector 76 78 1))

(define (paho:encode msg)
  (pahomqtt:log 4 "paho:encode " msg)
  (cond ((string? msg) (string->u8vector msg))
        ((number? msg) (string->u8vector (number->string msg)))
        ((port? msg) (u8vector-append paho:magic (object->u8vector #f))) ;; we can't serialize ports
        (else (u8vector-append paho:magic (object->u8vector msg)))))

(define (paho:decode msg)
  (pahomqtt:log 4 "paho:decode " msg)
  (let* ((msglen (u8vector-length msg))
         (maglen (u8vector-length paho:magic)))
    (if (and (> msglen maglen) (equal? (subu8vector msg 0 maglen) paho:magic))
      (u8vector->object (subu8vector msg maglen msglen))
        (let ((num (string->number (u8vector->string msg))))
          (if num num (u8vector->string msg))))))

(define (paho:msgloop handle)
  (let loop ((n 0))
    (let ((msglen (paho:msg-ready handle)))
      (if (fx= msglen -1) #t
        (let* ((m (table-ref paho:lut handle #f))
               (h (if m (table-ref m 'handler #f) #f))
               (topic (paho:msg-topic handle))
               (u8data (make-u8vector msglen)))
          (paho:msg-payload handle u8data)
          (pahomqtt:log 1 "[" n "] dispatch topic: " topic " length=" msglen " handle=" handle)
          (if (procedure? h) (h topic (paho:decode u8data)))
          (loop (fx+ n 1)))))))

;; mqtt -----------------

(define (mqtt-reset t . args)
  (pahomqtt:log 2 "mqtt-reset " t  " " args)
  (let loop ((defs '(
     (clean-session 1)
     (host "127.0.0.1")
     (port 1883)
     (keepalive 20)
     (timeout 10)
     (max-packets 100)
     (handler #f)
     (subscriptions ())
     (will #f) ;;  ("topic" "msg" qos retain)
     (username #f)
     (password #f)
     (cafile #f)
     (id #f)
     (handle #f)
     (subscribe ())
     (publish ())
     (publish-all-topicprefix #f)
     (publish-all-qos 2)
     (publish-all-retain 0)
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
         (let* ((tmpid (string-append "LN/" (ipaddr->string (host-ipaddr)) "-" (number->string (paho:pid))))
                (tmplen (string-length tmpid)))
           (table-set! t 'id (substring tmpid 0 (min tmplen 23))))))

    (table-set! t 'thread (make-safe-thread (lambda () (let main-loop ()
      (let* ((handle (table-ref t 'handle #f))
             (constatus (if handle (paho:con-status handle) PAHO_FAILURE)))
        (if (fx= constatus PAHO_SUCCESS) (paho:msgloop handle)
          (if (fx= constatus PAHO_FAILURE)
            (let* ((id (table-ref t 'id #f))
                   (cafile (table-ref t 'cafile #f))
                   (address (string-append (if cafile "ssl://" "tcp://")
                     (table-ref t 'host #f) ":" (number->string (table-ref t 'port #f))))
                   (timeout (table-ref t 'timeout #f))
                   (keepalive (table-ref t 'keepalive #f))
                   (cleansession (table-ref t 'clean-session #f))
                   (username (table-ref t 'username #f))
                   (passwd (table-ref t 'password #f))
                   (will (table-ref t 'will #f))
                   (will-topic (if will (car will) #f))
                   (will-msg (if will (cadr will) #f))
                   (will-qos (if will (caddr will) 0))
                   (will-retain (if will (cadddr will) 0))
                   (newhandle (paho:connect address id cafile
                      timeout keepalive cleansession username passwd 
                      will-topic will-msg will-qos will-retain))
                   (res (let loop () (if (not (fx= (paho:con-status newhandle) PAHO_PENDING))
                      (fx= (paho:con-status newhandle) PAHO_SUCCESS)
                        (begin (thread-sleep! 0.1) (loop))))))
                (if res (begin
                  (let loop2 ((subs (table-ref t 'subscriptions '())))
                     (if (> (length subs) 0) (begin
                       (apply paho:subscribe (append (list newhandle) (car subs))) 

;;                       (let loop3 () (if (not (fx= (paho:sub-status newhandle) PAHO_PENDING))
;;                        (fx= (paho:sub-status newhandle) PAHO_SUCCESS)
;;                        (begin (thread-sleep! 0.1) (loop3))))

                       (loop2 (cdr subs)))))
                  (table-set! t 'handle newhandle)
                  (table-set! paho:lut newhandle t)
                )))))
          (if (thread-receive (if (fx= constatus PAHO_SUCCESS) 0.01 1.0) #t) 
            (main-loop) (pahomqtt:log 2 "thread exited normally"))
        )))))

      (thread-start! (table-ref t 'thread #f))
   t)

(define (mqtt-connected? t)
  (pahomqtt:log 2 "mqtt-connected?")
   (fx= (paho:con-status (table-ref t 'handle #f)) PAHO_SUCCESS))
 
(define (make-mqtt . args)
  (pahomqtt:log 2 "make-mqtt " args)
  (let ((t (make-table)))
    (apply mqtt-reset (append (list t) args))))

(define (mqtt-subscribe t topic qos)
  (pahomqtt:log 2 "mqtt-subscribe " t " " topic " " qos)
  (let ((connected (mqtt-connected? t)))
    (let ((subs (table-ref t 'subscriptions '())))
      (if (not (assoc topic subs)) (table-set! t 'subscriptions (append subs (list (list topic qos))))))
    (if connected (paho:subscribe (table-ref t 'handle #f) topic qos))
    #t))

(define (mqtt-unsubscribe t topic)
  (pahomqtt:log 2 "mqtt-unsubscribe " t " " topic)
  (let ((connected (mqtt-connected? t)))
    (let loop ((subs (table-ref t 'subscriptions '()))(nsubs '()))
      (if (= (length subs) 0) (table-set! t 'subscriptions nsubs)
        (loop (cdr subs) (append nsubs (if (string=? (car (car subs)) topic) '() (list (car subs)))))))
    (if connected  (paho:unsubscribe (table-ref t 'handle #f) topic))
    #t))

(define (mqtt-publish t topic msg qos retain)
  (pahomqtt:log 2 "mqtt-publish " t " " topic " " msg " " qos " " retain)
  (let ((connected (mqtt-connected? t)))
    (if connected (begin (paho:publish (table-ref t 'handle #f) topic qos retain (paho:encode msg)) #t) #f)))

(define (mqtt-destroy t)
  (pahomqtt:log 2 "mqtt-destroy")
  (let ((handle (table-ref t 'handle #f))
        (thread (table-ref t 'thread #f)))
    (if (thread? thread) (thread-send thread #f))
    (if (not (fx= (paho:con-status handle) PAHO_FAILURE)) (begin
      (paho:disconnect handle)
      (let loop () (if (fx= (paho:con-status handle) PAHO_FAILURE) #t
        (begin (thread-sleep! 0.1) (loop))))))
    (table-set! t 'handle #f)
    (if handle (paho:destroy handle))
    #t))

;; eof

