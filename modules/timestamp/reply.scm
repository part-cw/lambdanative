#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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

;; Timestamp reply related functions

(c-declare #<<end-of-c-declare
#include <string.h>
#include <openssl/ts.h>

int get_tsr(unsigned char* tsr, int len, unsigned char* result){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  len = i2d_TS_RESP(ts_resp,NULL);
  i2d_TS_RESP(ts_resp,&result);
  TS_RESP_free(ts_resp);
  return len;
}

int get_tsr_status(unsigned char *tsr, int len){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  TS_STATUS_INFO *ts_status = TS_RESP_get_status_info(ts_resp);
  if (ts_resp==NULL)
    return 0;
  ret = ASN1_INTEGER_get(ts_status->status);
  TS_RESP_free(ts_resp);
  return ret;
}

int get_tsr_statusstr(unsigned char *tsr, int len,unsigned char *txt){
  static const char *status_map[] = {
    "Granted",
    "Granted with modifications",
    "Rejected",
    "Waiting",
    "Revocation warning",
    "Revoked"
  };
  static int status_len[] = {7,26,8,7,18,7};
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  if (ts_resp==NULL)
    return 0;
  TS_STATUS_INFO *ts_status = TS_RESP_get_status_info(ts_resp);
  int st = ASN1_INTEGER_get(ts_status->status);
  if (st > 5)
    return 0;
  ret = status_len[st];
  const unsigned char *retstr=status_map[st];
  int i;
  for (i=0;i<ret;i++){
    txt[i]=retstr[i];
  }
  TS_RESP_free(ts_resp);
  return ret;
}

int get_tsr_version(unsigned char *tsr, int len){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  TS_TST_INFO *ts_info = TS_RESP_get_tst_info(ts_resp);
  if (ts_resp==NULL)
    return 0;
  ret=TS_TST_INFO_get_version(ts_info);
  TS_RESP_free(ts_resp);
  return ret;
}

int get_tsr_nonce(unsigned char *tsr, int len){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  if (ts_resp==NULL)
    return 0;
  TS_TST_INFO *ts_info = TS_RESP_get_tst_info(ts_resp);
  const ASN1_INTEGER *nonce = TS_TST_INFO_get_nonce(ts_info);
  ret = ASN1_INTEGER_get(nonce);
  TS_RESP_free(ts_resp);
  return ret;
}

int get_tsr_serial(unsigned char *tsr, int len, unsigned char *msg){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  TS_TST_INFO *ts_info = TS_RESP_get_tst_info(ts_resp);
  if (ts_resp==NULL)
    return 0;
  const ASN1_INTEGER *serial = TS_TST_INFO_get_serial(ts_info);
  BIGNUM *num_bn = BN_new();
  if (num_bn==NULL)
    return 0;
  ASN1_INTEGER_to_BN(serial, num_bn);
  ret=BN_bn2bin(num_bn,msg);
  TS_RESP_free(ts_resp);
  return ret;
}

int get_tsr_policy(unsigned char *tsr, int len, unsigned char *msg){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  if (ts_resp==NULL)
    return 0;
  TS_TST_INFO *ts_info = TS_RESP_get_tst_info(ts_resp);
  ASN1_OBJECT *policy_id = TS_TST_INFO_get_policy_id(ts_info);
  if (policy_id == NULL) {
    msg = "";
    ret = 0;
  } else {
    char obj_txt[128];
    ret = OBJ_obj2txt(obj_txt, sizeof(obj_txt), policy_id, 0);
    int i;
    for (i=0;i<ret;i++) {
      msg[i]=obj_txt[i];
    }
  }
  TS_RESP_free(ts_resp);
  return ret;
}

int get_tsr_msg(unsigned char *tsr, int len, unsigned char *msg){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  if (ts_resp==NULL)
    return 0;
  TS_TST_INFO *ts_info = TS_RESP_get_tst_info(ts_resp);
  TS_MSG_IMPRINT *msg_imprint = TS_TST_INFO_get_msg_imprint(ts_info);
  ASN1_OCTET_STRING *str = TS_MSG_IMPRINT_get_msg(msg_imprint);
  ret = ASN1_STRING_length(str);
  unsigned char *data = ASN1_STRING_data(str);
  int i;
  for (i=0;i<ret;i++){
    msg[i]=data[i];
  }
  TS_RESP_free(ts_resp);
  return ret;
}

double get_tsr_accuracy(unsigned char *tsr, int len){
  double ret=0.;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  TS_TST_INFO *ts_info = TS_RESP_get_tst_info(ts_resp);
  if (ts_resp==NULL)
    return 0;
  TS_ACCURACY *accuracy = TS_TST_INFO_get_accuracy(ts_info);
  if (accuracy==NULL)
    return 0;
  const ASN1_INTEGER *sec = TS_ACCURACY_get_seconds(accuracy);
  if (sec != NULL)
    ret+= ASN1_INTEGER_get(sec);
  const ASN1_INTEGER *msec = TS_ACCURACY_get_millis(accuracy);
  if (msec != NULL)
    ret+= (double)ASN1_INTEGER_get(msec)/1000;
  const ASN1_INTEGER *usec = TS_ACCURACY_get_micros(accuracy);
  if (usec != NULL)
    ret+= (double)ASN1_INTEGER_get(usec)/1000000;
  TS_RESP_free(ts_resp);
  return ret;
}

int get_tsr_ordering(unsigned char *tsr, int len){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  TS_TST_INFO *ts_info = TS_RESP_get_tst_info(ts_resp);
  if (ts_resp==NULL)
    return 0;
  ret=TS_TST_INFO_get_ordering(ts_info);
  TS_RESP_free(ts_resp);
  return ret;
}

int get_tsr_gentime(unsigned char *tsr, int len, unsigned char *msg){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  TS_TST_INFO *ts_info = TS_RESP_get_tst_info(ts_resp);
  if (ts_resp==NULL)
    return 0;
  const ASN1_GENERALIZEDTIME *ts = TS_TST_INFO_get_time(ts_info);
  ret = ts->length;
  if (ret < 12)
    return 0;
  unsigned char *data = ts->data;
  int i;
  for (i=0;i<ret;i++){
    msg[i]=data[i];
  }
  TS_RESP_free(ts_resp);
  return ret;
}

int get_tsr_tsa(unsigned char *tsr, int len, unsigned char *msg, int msglen){
  int ret=0;
  const unsigned char *t = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &t, len);
  if (ts_resp==NULL)
    return 0;
  TS_TST_INFO *ts_info = TS_RESP_get_tst_info(ts_resp);
  GENERAL_NAME *name = TS_TST_INFO_get_tsa(ts_info);
  if (name==NULL)
    return 0;
  if (name->type == GEN_DIRNAME){
  //X509_NAME_oneline (X509_get_issuer_name (cert), issuer_DN, 255);
     X509_NAME_oneline(name->d.dirn,msg,msglen);
     return strlen(msg);
  }
  TS_RESP_free(ts_resp);
  return ret;
}

end-of-c-declare
)

;; Scheme bindings
(define (timestamp-tsr-save filename tsr)
  (if (and (string? filename) (u8vector? tsr))
    (let ((fname (string-append filename ".tsr")))
      (u8vector->file tsr fname)
      (file-exists? fname)
    )
    #f
  )
)

(define (timestamp-tsr-load filename)
  (let ((split (string-split filename #\.)))
    (if (and (fx>= (length split) 2) (string=? (car (reverse split)) "tsr"))
      (file->u8vector filename)
      #f
    )
  ))

(define (timestamp-tsr-getstatus tsr)
  ((c-lambda (scheme-object int) int "___result=
    get_tsr_status(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
    tsr (u8vector-length tsr))
)
(define (timestamp-tsr-granted? tsr)
  (or (fx= (timestamp-tsr-getstatus tsr) 0) (fx= (timestamp-tsr-getstatus tsr) 1))
)

(define (timestamp-tsr-getstatusstr tsr)
  (let* ((v (make-u8vector 30))
         (f ((c-lambda (scheme-object int scheme-object) int "___result=
            get_tsr_statusstr(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
            ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
            tsr (u8vector-length tsr) v)))
    (if f (u8vector->string (subu8vector v 0 f)) #f)
  ))

(define (timestamp-tsr-getversion tsr)
 ((c-lambda (scheme-object int) int "___result=
   get_tsr_version(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
   tsr (u8vector-length tsr))
)

(define (timestamp-tsr-getpolicy tsr)
 (let* ((v (make-u8vector 128))
        (f ((c-lambda (scheme-object int scheme-object) int "___result=
           get_tsr_policy(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
           ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
           tsr (u8vector-length tsr) v)))
    (if f (u8vector->string (subu8vector v 0 f)) #f)
  ))

(define (timestamp-tsr-getmessage tsr)
 (let* ((v (make-u8vector 100))
        (f ((c-lambda (scheme-object int scheme-object) int "___result=
           get_tsr_msg(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
           ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
           tsr (u8vector-length tsr) v)))
   (if f (subu8vector v 0 f) #f)
 ))

(define (timestamp-tsr-getserial tsr)
 (let* ((v (make-u8vector 100))
        (f ((c-lambda (scheme-object int scheme-object) int "___result=
           get_tsr_serial(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
           ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
           tsr (u8vector-length tsr) v)))
   (if f (subu8vector v 0 f) #f)
 ))

(define (timestamp-tsr-getgentimestr tsr)
 (let* ((v (make-u8vector 16))
        (f ((c-lambda (scheme-object int scheme-object) int "___result=
           get_tsr_gentime(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
           ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
           tsr (u8vector-length tsr) v)))
    (if f (u8vector->string (subu8vector v 0 f)) #f)
  ))
(define (timestamp-tsr-getepoch tsr)
  (string->seconds (timestamp-tsr-getgentimestr tsr) "%Y%m%d%H%M%S~z")
)

(define (timestamp-tsr-getaccuracy tsr)
 ((c-lambda (scheme-object int) double "___result=
   get_tsr_accuracy(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
   tsr (u8vector-length tsr))
)

(define (timestamp-tsr-getordering tsr)
  ((c-lambda (scheme-object int) bool "___result=
    get_tsr_ordering(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
    tsr (u8vector-length tsr))
)

(define (timestamp-tsr-getnonce tsr)
  ((c-lambda (scheme-object int) int "___result=
    get_tsr_nonce(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
    tsr (u8vector-length tsr))
)

(define (timestamp-tsr-gettsa tsr)
 (let* ((v (make-u8vector 100))
        (f ((c-lambda (scheme-object int scheme-object int) int "___result=
           get_tsr_tsa(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
           ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4);")
           tsr (u8vector-length tsr) v (u8vector-length v))))
    (if f (u8vector->string (subu8vector v 0 f)) #f)
  ))

(define (timestamp:gettsr tsr)
    (let* ((v (make-u8vector 4096))
           (f ((c-lambda (scheme-object int scheme-object) int "___result=
              get_tsr(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
              ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
              tsr (u8vector-length tsr) v)))
      (if f (subu8vector v 0 f) #f)
))

;; eof
