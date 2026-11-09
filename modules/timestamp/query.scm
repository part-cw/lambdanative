#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2026, University of British Columbia
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
#include <openssl/evp.h>
#include <openssl/rand.h>

static TS_REQ *createTSQ(unsigned char *hash, int len){
  TS_REQ *ts_req = NULL;
  X509_ALGOR *algo = NULL;
  TS_MSG_IMPRINT *msg_imprint = NULL;
  ASN1_INTEGER *nonce_asn1 = NULL;
  ASN1_OBJECT *algorithm = NULL;
  unsigned char nonce_bytes[4];
  unsigned long nonce_value;
  const EVP_MD *md = NULL;
  int ok = 0;

  if (hash == NULL)
    return NULL;
  if (len==SHA512_DIGEST_LENGTH)
    md=EVP_sha512();
  else if (len==SHA256_DIGEST_LENGTH)
    md=EVP_sha256();
  else if (len==SHA_DIGEST_LENGTH)
    md=EVP_sha1();
  else
    return NULL;

  ts_req = TS_REQ_new();
  algo = X509_ALGOR_new();
  msg_imprint = TS_MSG_IMPRINT_new();
  if (ts_req==NULL || algo==NULL || msg_imprint==NULL)
    goto cleanup;

  if (!TS_REQ_set_version(ts_req, 1))
    goto cleanup;
  algorithm = OBJ_dup(OBJ_nid2obj(EVP_MD_get_type(md)));
  if (algorithm == NULL || !X509_ALGOR_set0(algo, algorithm, V_ASN1_NULL, NULL))
    goto cleanup;
  algorithm = NULL;
  if (!TS_MSG_IMPRINT_set_algo(msg_imprint, algo))
    goto cleanup;
  if (!TS_MSG_IMPRINT_set_msg(msg_imprint, hash, len))
    goto cleanup;
  if (!TS_REQ_set_msg_imprint(ts_req, msg_imprint))
    goto cleanup;

  nonce_asn1 = ASN1_INTEGER_new();
  if (nonce_asn1 == NULL || RAND_bytes(nonce_bytes, sizeof(nonce_bytes)) != 1)
    goto cleanup;
  nonce_value = ((unsigned long)nonce_bytes[0] << 24) | ((unsigned long)nonce_bytes[1] << 16) |
                ((unsigned long)nonce_bytes[2] << 8) | (unsigned long)nonce_bytes[3];
  nonce_value &= 0x7fffffffUL;
  if (!ASN1_INTEGER_set(nonce_asn1, (long)nonce_value) || !TS_REQ_set_nonce(ts_req, nonce_asn1))
    goto cleanup;
  if (!TS_REQ_set_cert_req(ts_req, 1))
    goto cleanup;
  ok = 1;

cleanup:
  ASN1_OBJECT_free(algorithm);
  ASN1_INTEGER_free(nonce_asn1);
  TS_MSG_IMPRINT_free(msg_imprint);
  X509_ALGOR_free(algo);
  if (!ok) {
    TS_REQ_free(ts_req);
    return NULL;
  }
  return ts_req;
}

static TS_REQ *decodeTSQ(const unsigned char *tsq, int len){
  const unsigned char *t = tsq;
  TS_REQ *ts_req;
  if (tsq == NULL || len <= 0)
    return NULL;
  ts_req = d2i_TS_REQ(NULL, &t, len);
  if (ts_req == NULL || t != tsq + len) {
    TS_REQ_free(ts_req);
    return NULL;
  }
  return ts_req;
}

int get_tsq(unsigned char *hash, int hashlen, unsigned char *result,
            int result_capacity){
  if (result == NULL || result_capacity <= 0)
    return 0;
  TS_REQ *query = createTSQ(hash,hashlen);
  if (query == NULL)
    return 0;
  int len = i2d_TS_REQ(query,NULL);
  if (len <= 0 || len > result_capacity) {
    TS_REQ_free(query);
    return 0;
  }
  unsigned char *out = result;
  int written = i2d_TS_REQ(query,&out);
  TS_REQ_free(query);
  return written == len ? written : 0;
}

int get_tsq_version(unsigned char *tsq, int len){
  TS_REQ *ts_req = decodeTSQ(tsq, len);
  if (ts_req == NULL)
    return 0;
  int ret = TS_REQ_get_version(ts_req);
  TS_REQ_free(ts_req);
  return ret;
}

int get_tsq_nonce(unsigned char *tsq, int len){
  TS_REQ *ts_req = decodeTSQ(tsq, len);
  if (ts_req == NULL)
    return 0;
  const ASN1_INTEGER *nonce = TS_REQ_get_nonce(ts_req);
  long value = nonce == NULL ? -1 : ASN1_INTEGER_get(nonce);
  TS_REQ_free(ts_req);
  return value >= 0 && value <= 0x7fffffffL ? (int)value : 0;
}

int get_tsq_msg(unsigned char *tsq, int len, unsigned char *msg,
                int msg_capacity){
  TS_REQ *ts_req = decodeTSQ(tsq, len);
  if (ts_req == NULL || msg == NULL || msg_capacity < 0) {
    TS_REQ_free(ts_req);
    return 0;
  }
  TS_MSG_IMPRINT *msg_imprint = TS_REQ_get_msg_imprint(ts_req);
  ASN1_OCTET_STRING *str = msg_imprint == NULL ? NULL : TS_MSG_IMPRINT_get_msg(msg_imprint);
  int ret = str == NULL ? -1 : ASN1_STRING_length(str);
  const unsigned char *data = str == NULL ? NULL : ASN1_STRING_get0_data(str);
  if (ret < 0 || ret > msg_capacity || (ret > 0 && data == NULL)) {
    TS_REQ_free(ts_req);
    return 0;
  }
  if (ret > 0)
    memcpy(msg, data, ret);
  TS_REQ_free(ts_req);
  return ret;
}

int get_tsq_policy(unsigned char *tsq, int len, unsigned char *msg, int msg_capacity){
  TS_REQ *ts_req = decodeTSQ(tsq, len);
  if (ts_req == NULL || msg == NULL || msg_capacity <= 0) {
    TS_REQ_free(ts_req);
    return 0;
  }
  ASN1_OBJECT *policy_id = TS_REQ_get_policy_id(ts_req);
  if (policy_id == NULL) {
    TS_REQ_free(ts_req);
    return 0;
  }
  char obj_txt[128];
  int ret = OBJ_obj2txt(obj_txt, sizeof(obj_txt), policy_id, 0);
  if (ret <= 0 || ret >= (int)sizeof(obj_txt) || ret > msg_capacity) {
    TS_REQ_free(ts_req);
    return 0;
  }
  memcpy(msg, obj_txt, ret);
  TS_REQ_free(ts_req);
  return ret;
}

end-of-c-declare
)

;; Scheme bindings
(define (timestamp:gettsq hash)
  (let* ((v (make-u8vector 100))
         (f ((c-lambda (scheme-object int scheme-object int) int "___result=
            get_tsq(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
                    ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4);")
            hash (u8vector-length hash) v (u8vector-length v))))
    (if f (subu8vector v 0 f) #f)
  ))

(define (timestamp-tsq-generate-sha512sum sha)
  (if (u8vector? sha) (timestamp:gettsq sha) #f))

(define (timestamp-tsq-generate filename)
  (if (string? filename)
    (timestamp-tsq-generate-sha512sum (sha512sum filename)) #f))

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
         (f ((c-lambda (scheme-object int scheme-object int) int "___result=
            get_tsq_policy(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
            ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4);")
            tsq (u8vector-length tsq) v (u8vector-length v))))
    (if f (u8vector->string (subu8vector v 0 f)) #f)
  ))

(define (timestamp-tsq-getnonce tsq)
  ((c-lambda (scheme-object int) int "___result=
    get_tsq_nonce(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
    tsq (u8vector-length tsq))
)

(define (timestamp-tsq-getmessage tsq)
  (let* ((v (make-u8vector 100))
         (f ((c-lambda (scheme-object int scheme-object int) int "___result=
            get_tsq_msg(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
            ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4);")
            tsq (u8vector-length tsq) v (u8vector-length v))))
    (if f (subu8vector v 0 f) #f)
  ))

;; eof
