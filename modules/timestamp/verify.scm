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

;; Timestamp verification related functions

(c-declare #<<end-of-c-declare
#include <string.h>
#include <openssl/bio.h>
#include <openssl/crypto.h>
#include <openssl/err.h>
#include <openssl/ts.h>
void log_c(char *);

static int log_openssl_error(const char *message, size_t len, void *arg){
  (void)arg;
  char text[1024];
  if (len >= sizeof(text))
    len = sizeof(text) - 1;
  memcpy(text, message, len);
  text[len] = '\0';
  log_c(text);
  return 1;
}

static int verify_cb(int ok, X509_STORE_CTX *ctx){
  if (ok==0){
    const char *err = X509_verify_cert_error_string(X509_STORE_CTX_get_error(ctx));
    X509 *err_cert = X509_STORE_CTX_get_current_cert(ctx);
    char subject[256];
    if (err_cert != NULL) {
      X509_NAME *subj = X509_get_subject_name(err_cert);
      if (subj != NULL)
        log_c(X509_NAME_oneline(subj, subject, sizeof(subject)));
    }
    log_c((char *)err);
  }
  return ok;
}

int hash_tsr_verify(unsigned char *hash, int hash_len, unsigned char *tsr, int tsr_len,
                    unsigned char *CAfile){
  int ret = 0;
  TS_RESP *ts_resp = NULL;
  TS_VERIFY_CTX *ctx = TS_VERIFY_CTX_new();
  X509_STORE *cert_ctx = NULL;
  unsigned char *imprint = NULL;
  const unsigned char *tr = tsr;
  char *failure_reason = "Timestamp verification failed";

  ERR_clear_error();
  if (hash == NULL || hash_len <= 0 || tsr == NULL || tsr_len <= 0 ||
      CAfile == NULL) {
    failure_reason = "Invalid timestamp verification input";
    goto cleanup;
  }
  if (ctx == NULL) {
    failure_reason = "Unable to allocate timestamp verification context";
    goto cleanup;
  }
  ts_resp = d2i_TS_RESP(NULL, &tr, tsr_len);
  if (ts_resp == NULL) {
    failure_reason = "Unable to decode timestamp response";
    goto cleanup;
  }
  if (tr != tsr + tsr_len) {
    failure_reason = "Trailing data after timestamp response";
    goto cleanup;
  }
  imprint = OPENSSL_memdup(hash, (size_t)hash_len);
  if (imprint == NULL) {
    failure_reason = "Unable to copy timestamp message imprint";
    goto cleanup;
  }
  if (TS_VERIFY_CTX_set0_imprint(ctx, imprint, hash_len) != 1) {
    failure_reason = "Unable to set timestamp message imprint";
    goto cleanup;
  }
  imprint = NULL;
  int verify_flags = TS_VFY_IMPRINT | TS_VFY_SIGNATURE;
  if ((TS_VERIFY_CTX_add_flags(ctx, verify_flags) & verify_flags) != verify_flags) {
    failure_reason = "Unable to configure timestamp verification flags";
    goto cleanup;
  }
  cert_ctx = X509_STORE_new();
  if (cert_ctx == NULL) {
    failure_reason = "Unable to allocate certificate store";
    goto cleanup;
  }
  X509_STORE_set_verify_cb(cert_ctx, verify_cb);
  X509_LOOKUP *lookup = X509_STORE_add_lookup(cert_ctx, X509_LOOKUP_file());
  if (lookup == NULL) {
    failure_reason = "Unable to create certificate file lookup";
    goto cleanup;
  }
  if (X509_LOOKUP_load_file(lookup, CAfile, X509_FILETYPE_PEM) != 1) {
    failure_reason = "Unable to load timestamp CA certificate file";
    goto cleanup;
  }
  if (TS_VERIFY_CTX_set0_store(ctx, cert_ctx) != 1) {
    failure_reason = "Unable to set timestamp certificate store";
    goto cleanup;
  }
  cert_ctx = NULL;
  ret = TS_RESP_verify_response(ctx, ts_resp);
  if (!ret)
    failure_reason = "Timestamp response verification failed";

cleanup:
  if (!ret) {
    log_c(failure_reason);
    ERR_print_errors_cb(log_openssl_error, NULL);
  }
  OPENSSL_free(imprint);
  X509_STORE_free(cert_ctx);
  TS_VERIFY_CTX_free(ctx);
  TS_RESP_free(ts_resp);
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
