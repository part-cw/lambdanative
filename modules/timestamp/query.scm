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

;; Timestamp query related functions

(c-declare #<<end-of-c-declare
#include <string.h>
#include <openssl/ts.h>

static TS_REQ *createTSQ(unsigned char *hash, int len){
  // Get proper type
  const EVP_MD *md = NULL;
  if (len==SHA512_DIGEST_LENGTH)
    md=EVP_sha512();
  else if (len==SHA256_DIGEST_LENGTH)
    md=EVP_sha256();
  else if (len==SHA_DIGEST_LENGTH)
    md=EVP_sha1();
  else
    return 0;
  // Define structures
  TS_REQ *ts_req =  TS_REQ_new();
  X509_ALGOR *algo = X509_ALGOR_new();
  TS_MSG_IMPRINT *msg_imprint = TS_MSG_IMPRINT_new();
  if (ts_req==NULL || algo==NULL || msg_imprint==NULL)
    return 0;
  // Set request version and add hash algorithm identifier
  TS_REQ_set_version(ts_req, 1);
  algo->algorithm = OBJ_nid2obj(EVP_MD_type(md));
  algo->parameter = ASN1_TYPE_new();
  if (algo->algorithm == NULL || algo->parameter == NULL)
    return 0;
  algo->parameter->type = V_ASN1_NULL;
  if (!TS_MSG_IMPRINT_set_algo(msg_imprint, algo))
    return 0;
  // Add the hash
  if (!TS_MSG_IMPRINT_set_msg(msg_imprint, hash, len))
    return 0;
  if (!TS_REQ_set_msg_imprint(ts_req, msg_imprint))
    return 0;
  // Add the nonce
  ASN1_INTEGER *nonce_asn1 = ASN1_INTEGER_new();
  time_t t;
  srand((unsigned) time(&t));
  ASN1_INTEGER_set(nonce_asn1, rand());
  if (!TS_REQ_set_nonce(ts_req, nonce_asn1))
    return 0;
  // Add the certificate request
  if (!TS_REQ_set_cert_req(ts_req, 1))
    return 0;
  // Cleanup and return request
  TS_MSG_IMPRINT_free(msg_imprint);
  X509_ALGOR_free(algo);
  ASN1_INTEGER_free(nonce_asn1);
  return ts_req;
}

int get_tsq(unsigned char* hash, int hashlen, unsigned char* result){
  TS_REQ *query = createTSQ(hash,hashlen);
  int len = i2d_TS_REQ(query,NULL);
  i2d_TS_REQ(query,&result);
  TS_REQ_free(query);
  return len;
}

int get_tsq_version(unsigned char *tsq, int len){
  int ret=0;
  const unsigned char *t = tsq;
  TS_REQ *ts_req =  TS_REQ_new();
  d2i_TS_REQ(&ts_req, &t, len);
  if (ts_req==NULL)
    return 0;
  ret=TS_REQ_get_version(ts_req);
  TS_REQ_free(ts_req);
  return ret;
}

int get_tsq_nonce(unsigned char *tsq, int len){
  int ret=0;
  const unsigned char *t = tsq;
  TS_REQ *ts_req =  TS_REQ_new();
  d2i_TS_REQ(&ts_req, &t, len);
  if (ts_req==NULL)
    return 0;
  const ASN1_INTEGER *nonce = TS_REQ_get_nonce(ts_req);
  ret = ASN1_INTEGER_get(nonce);
  TS_REQ_free(ts_req);
  return ret;
}

int get_tsq_msg(unsigned char *tsq, int len, unsigned char *msg){
  int ret=0;
  const unsigned char *t = tsq;
  TS_REQ *ts_req =  TS_REQ_new();
  d2i_TS_REQ(&ts_req, &t, len);
  if (ts_req==NULL)
    return 0;
  TS_MSG_IMPRINT *msg_imprint = TS_REQ_get_msg_imprint(ts_req);
  ASN1_OCTET_STRING *str = TS_MSG_IMPRINT_get_msg(msg_imprint);
  ret = ASN1_STRING_length(str);
  unsigned char *data = ASN1_STRING_data(str);
  int i;
  for (i=0;i<ret;i++){
    msg[i]=data[i];
  }
  TS_REQ_free(ts_req);
  return ret;
}

int get_tsq_policy(unsigned char *tsq, int len, unsigned char *msg){
  int ret=0;
  const unsigned char *t = tsq;
  TS_REQ *ts_req =  TS_REQ_new();
  d2i_TS_REQ(&ts_req, &t, len);
  if (ts_req==NULL)
    return 0;
  ASN1_OBJECT *policy_id = TS_REQ_get_policy_id(ts_req);
  if (policy_id == NULL){
    msg = "";
    ret = 0;
  } else {
    char obj_txt[128];
    ret = OBJ_obj2txt(obj_txt, sizeof(obj_txt), policy_id, 0);
    int i;
    for (i=0;i<ret;i++){
      msg[i]=obj_txt[i];
    }
  }
  TS_REQ_free(ts_req);
  return ret;
}

end-of-c-declare
)

;; Scheme bindings
(define (timestamp:gettsq hash)
  (let* ((v (make-u8vector 100))
         (f ((c-lambda (scheme-object int scheme-object) int "___result=
            get_tsq(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
                    ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
            hash (u8vector-length hash) v)))
    (if f (subu8vector v 0 f) #f)
  ))

(define (timestamp-tsq-generate filename)
  (if (string? filename)
    (timestamp:gettsq (sha512sum filename))
    #f
  )
)

(define (timestamp-tsq-save filename tsq)
  (if (and (string? filename) (u8vector? tsq))
    (let ((fname (string-append filename ".tsq")))
      (u8vector->file tsq fname)
      (file-exists? fname)
    )
    #f
  )
)

(define (timestamp-tsq-load filename)
  (let ((split (string-split filename #\.)))
    (if (and (fx>= (length split) 2) (string=? (car (reverse split)) "tsq"))
      (file->u8vector filename)
      #f
    )
  ))

(define (timestamp-tsq-getversion tsq)
  ((c-lambda (scheme-object int) int "___result=
    get_tsq_version(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
    tsq (u8vector-length tsq))
)

(define (timestamp-tsq-getpolicy tsq)
  (let* ((v (make-u8vector 128))
         (f ((c-lambda (scheme-object int scheme-object) int "___result=
            get_tsq_policy(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
            ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
            tsq (u8vector-length tsq) v)))
    (if f (u8vector->string (subu8vector v 0 f)) #f)
  ))

(define (timestamp-tsq-getnonce tsq)
  ((c-lambda (scheme-object int) int "___result=
    get_tsq_nonce(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
    tsq (u8vector-length tsq))
)

(define (timestamp-tsq-getmessage tsq)
  (let* ((v (make-u8vector 100))
         (f ((c-lambda (scheme-object int scheme-object) int "___result=
            get_tsq_msg(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
            ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)));")
            tsq (u8vector-length tsq) v)))
    (if f (subu8vector v 0 f) #f)
  ))

;; eof
