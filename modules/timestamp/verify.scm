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

;; Timestamp verification related functions

(c-declare #<<end-of-c-declare
#include <string.h>
#include <openssl/ts.h>
void log_c(char *);

static int verify_cb(int ok, X509_STORE_CTX *ctx){
  if (ok==0){
    const unsigned char *err= X509_verify_cert_error_string(X509_STORE_CTX_get_error(ctx));
    X509 *err_cert = X509_STORE_CTX_get_current_cert(ctx);
    X509_NAME *subj = X509_get_subject_name(err_cert);
    log_c(X509_NAME_oneline(subj,0,0));
    log_c(err);
  }
  return ok;
}

int hash_tsr_verify(unsigned char *hash, int hash_len, unsigned char *tsr, int tsr_len,
                    unsigned char *CAfile){
  int ret=0;
  const unsigned char *tr = tsr;
  TS_RESP *ts_resp =  TS_RESP_new();
  d2i_TS_RESP(&ts_resp, &tr, tsr_len);
  if (ts_resp==NULL)
    return 0;
  TS_VERIFY_CTX *ctx = TS_VERIFY_CTX_new();
  if (ctx==NULL)
    return 0;
  long imprint_len;
  ctx->flags |= TS_VFY_IMPRINT;
  ctx->imprint = hash;
  ctx->imprint_len = hash_len;
  if (ctx->imprint==NULL)
    return 0;

  ctx->flags |= TS_VFY_SIGNATURE;
  X509_STORE *cert_ctx = X509_STORE_new();
  X509_STORE_set_verify_cb(cert_ctx, verify_cb);
  X509_LOOKUP *lookup = X509_STORE_add_lookup(cert_ctx, X509_LOOKUP_file());
  if (lookup == NULL)
    return 0;
  int pass = X509_LOOKUP_load_file(lookup, CAfile, X509_FILETYPE_PEM);
  if (!pass)
    return 0;
  ctx->store = cert_ctx;
  if (ctx->store==NULL)
    return 0;

  // If this isn't here verification fails ... OpenSSL_add_all_algorithms(); works too
  OpenSSL_add_all_digests();
  ret = TS_RESP_verify_response(ctx, ts_resp);
  TS_RESP_free(ts_resp);
  EVP_cleanup();
  return ret;
}

end-of-c-declare
)

;; Scheme bindings
(define (timestamp-verify hash tsr cafile)
  (if (and hash tsr cafile)
    ((c-lambda (scheme-object int scheme-object int char-string) bool "___result=
      hash_tsr_verify(___CAST(unsigned char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,
                      ___CAST(unsigned char*,___BODY_AS(___arg3,___tSUBTYPED)),___arg4,
                      ___arg5);")
      hash (u8vector-length hash) tsr (u8vector-length tsr) cafile)
    #f
  ))

;; eof
