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
;; minimal code for https support
;; This does not support multiple concurrent connections

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>

#ifndef WIN32
#include <poll.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#endif

#include <openssl/crypto.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/rand.h>

static   int s=0;
static   SSL *ssl=0;
static   SSL_CTX *ctx=0;

#ifdef WIN32
#define bzero(a, b) memset(a, 0x0, b)
#define bcopy(a, b, c) memmove(b, a, c)
#endif

static int httpsclient_open(char *host, int port, int use_keys, char *cert, char *key, char *pwd){
  int ret,flags;
  struct hostent *servhost; 
  struct sockaddr_in server;
  s = socket(AF_INET, SOCK_STREAM, 0); 
  if ( s < 0 ) { return 0; }
  servhost = gethostbyname(host);
  if ( servhost == NULL ) { return 0; } 
  bzero((char *)&server, sizeof(server));
  server.sin_family = AF_INET;
  bcopy(servhost->h_addr, (char *)&server.sin_addr.s_addr, servhost->h_length);
  server.sin_port = htons(port);
  if (connect(s, (struct sockaddr*) &server, sizeof(server)) == -1 ) {
    if (errno==EINTR) {
      #ifndef WIN32
      // wait for call to complete
      struct pollfd tmp;
      tmp.fd = s;
      tmp.events = POLLOUT;
      while (poll(&tmp,1,-1) == -1 ) {
        if (errno==EINTR) continue;
        return 0;
      }
      #else
      // what are we supposed to do here?
      return 0;
      #endif
    } else { return 0; }
  }
  SSL_load_error_strings();
  SSL_library_init();
  ctx = SSL_CTX_new(SSLv23_client_method());
  if ( ctx == NULL ) { return 0; }
  SSL_CTX_set_options(ctx,SSL_OP_NO_SSLv2); //disable SSLv2

  // If we want to use key for authentication.
  if (use_keys == 1) {
    ret = SSL_CTX_use_certificate_file(ctx, cert, SSL_FILETYPE_PEM);
    if (ret <= 0) { return 0; }
    SSL_CTX_set_default_passwd_cb_userdata(ctx,pwd);
    ret = SSL_CTX_use_PrivateKey_file(ctx, key, SSL_FILETYPE_PEM);
    if (ret <= 0) { return 0; }
  }

  ssl = SSL_new(ctx);
  if ( ssl == NULL ){ return 0; }
  ret = SSL_set_fd(ssl, s);
  if ( ret == 0 ){ return 0; }

  RAND_poll();
  while ( RAND_status() == 0 ){
    unsigned short rand_ret = rand() % 65536;
    RAND_seed(&rand_ret, sizeof(rand_ret));
  }
  int retries = 0;
  while (1) {
    ret=SSL_connect(ssl);
    if (ret==1) break;
    // wait for call to complete
    if (retries++<1000&&SSL_get_error(ssl,ret)==SSL_ERROR_WANT_READ ) continue;
    return 0;
  }
  return 1;
}

int SSL_retryread(SSL *ssl, void *buf, int num){
  int ret;
  int retries = 0;
  while (1) {
    ret =  SSL_read(ssl,buf,num);
    if (ret<0) {
      // wait for call to complete
      if (retries++<1000&&SSL_get_error(ssl,ret)==SSL_ERROR_WANT_READ) continue;
      return (retries>=1000?-2:-1);
    } else break;
  }
  return (retries>=1000?-2:ret);
}

end-of-c-declare
)

(define (httpsclient-open host . port)
  (if (fx= (length port) 1)
    (httpsclient-key-open host "" "" "" (car port))
    (httpsclient-key-open host "" "" "")
  ))

(define (httpsclient-key-open host certchain key password . port)
  ((c-lambda (char-string int int char-string char-string char-string) int
    "httpsclient_open")
    host (if (fx= (length port) 1) (car port) 443)
    (if (string=? key "") 0 1) certchain key password))

(define (httpsclient-send buf)
  ((c-lambda (scheme-object int) int
     "___result=SSL_write(ssl,___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
     buf (u8vector-length buf)))

(define (httpsclient-recv buf)
  (u8vector-fill! buf 0)
  ((c-lambda (scheme-object int) int
     "___result=SSL_retryread(ssl,___CAST(void*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
     buf (u8vector-length buf)))

(define (httpsclient-recv-reentrant buf)
  (let loop ()
    (let ((ret (httpsclient-recv buf)))
      (if (>= ret -1)  ret (begin
        (thread-sleep! 0.1)
        (loop))))))

(define httpsclient-close (c-lambda () void "if (ssl) SSL_shutdown(ssl); if (ctx) SSL_CTX_free(ctx); if (s) close(s);"))

;; ------
;; simple test code - this just sends a GET command and dumps the response
(define httpsclient:testbuf (##still-copy (make-u8vector 1024)))
(define (httpsclient-test host file)
  (let ((ret (httpsclient-open host)))
    (if (> ret 0)
      (let* ((request (string-append "GET " file " HTTP/1.0\r\nHost: " host "\r\n\r\n"))
             (status  (httpsclient-send (string->u8vector request))))
        (for-each display (list "SSL BYTES SENT: " status "\n"))
        (let ((n (httpsclient-recv httpsclient:testbuf)))
          (for-each display (list "SSL BYTES RCVD: " n "\n"
            (u8vector->string httpsclient:testbuf) "\n"))
          (httpsclient-close))))))
;; ------

;; eof
