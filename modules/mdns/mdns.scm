#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2016, University of British Columbia
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

;; experimental support for mdns publishing (for Bonjour/Avahi discovery)

(##namespace ("mdns#"))
(##include "~~lib/gambit#.scm")
(##include "mdns#.scm")

(##namespace (""
  log-system 
))

(define mdns:debuglevel 0)
(define (mdns:log level . x)
   (if (>= mdns:debuglevel level)
      (apply log-system (append (list "mdns: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <winsock2.h>
//#include <in6addr.h>
#include <ws2tcpip.h>
#else
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>
#endif

#include <tinysvcmdns/mdns.h>
#include <tinysvcmdns/mdnsd.h>

struct mdnsd *svr=0;

static struct mdnsd *mdnsd_register(char *hostname, char *ipaddr, int port, char* type, char *descr)
{
  if (!svr) svr = mdnsd_start();
  if (svr) {
    mdnsd_set_hostname(svr, hostname, inet_addr(ipaddr));
    struct rr_entry *a2_e = NULL;
    a2_e = rr_create_a(create_nlabel(hostname), inet_addr(ipaddr));
    mdnsd_add_rr(svr, a2_e);
//    const char *txt[] = { "path=/index.html", NULL };
    struct mdns_service *svc = mdnsd_register_svc(svr, descr, type, port, NULL, NULL);
    mdns_service_destroy(svc);
  }
  return svr;
}

end-of-c-declare
)

(c-define-type mdnsd* (pointer void))

(define mdnsd:register (c-lambda (char-string char-string int char-string char-string) mdnsd* "mdnsd_register"))

(define mdnsd-main-loop-init (c-lambda (mdnsd*) void "mdnsd_main_loop_init"))
(define mdnsd-main-loop-run (c-lambda (mdnsd*) int "mdnsd_main_loop_run"))
(define mdnsd-main-loop-exit (c-lambda (mdnsd*) void "mdnsd_main_loop_exit"))

(define (mdns-register hostname ipaddr port type descr)
  (mdns:log 1 "mdns-register " hostname " " ipaddr " " port " " type " " descr)
  (let ((svr (mdnsd:register hostname ipaddr port type descr)))
    (thread-start! (make-thread (lambda ()
      (mdnsd-main-loop-init svr)
      (let loop ((n 0))
        (thread-sleep! 1)
        (if (fx= n 60) (begin 
          (mdns:log 1 "mdns-register <thread> " hostname " " ipaddr " " port " " type " " descr)
          (mdnsd:register hostname ipaddr port type descr)))
        (if (fx= (mdnsd-main-loop-run svr) 0)
          (begin (mdns:log 1 "mdns-register thread exit") (mdnsd-main-loop-exit svr))
          (loop (if (fx= n 60) 0 (+ n 1))))))))
    #t
  ))

;; eof
