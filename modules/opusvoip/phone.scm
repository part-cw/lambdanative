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

;; OPUS VoIP Phone call setup

;; Phone states
(define PHONE_ONHOOK  0)
(define PHONE_CONNECTED 1)
(define PHONE_RINGING 2)
(define PHONE_DIALING 4)
(define phone:state? (c-lambda () int "___result=opusvoip_state();"))
(define (phone-onhook?)  (fx= (phone:state?) PHONE_ONHOOK))
(define (phone-ringing?) (fx= (phone:state?) PHONE_RINGING))
(define (phone-dialing?) (fx= (phone:state?) PHONE_DIALING))
(define (phone-connected?) (fx= (phone:state?) PHONE_CONNECTED))

;; Local and remote IPs
(define phone:local #f)
(define phone:remote #f)

;; Phone communication setup
(define phone:communicationthread #f)
(define (phone-init)
  ((c-lambda () void "opusvoip_setup();"))
  ;; Start thread to handle the communication
  (set! phone:communicationthread (thread-start! (make-safe-thread (lambda ()
    (let loop ()
      (let ((timeout 0.01))
        ((c-lambda () void "opusvoip_handledata();"))
        (thread-receive timeout #f)
        (loop)
      )
    ) 'communication-thread))))
)

(define (phone-close)
  ((c-lambda () void "opusvoip_close();"))
  (thread-terminate! phone:communicationthread)
  (set! phone:communicationthread #f)
)

(define (phone-hangup)
  (if (not (phone-onhook?))
    ((c-lambda () void "opusvoip_hangup();"))
  )
  (rtaudio-stop)
  (set! phone:remote #f)
)

(define (phone-pickup)
  ((c-lambda () void "opusvoip_pickup();"))
)

(define (phone-dial ip)
  (set! phone:local (host-ipaddr))
  (let* ((split-ip (string-split ip #\.))
         (ip-num  (map string->number split-ip))
         (valid-ip (and (fx= (length ip-num) 4) (fx= (sum (map (lambda (l) (if (and l (< l 256)) 0 1)) ip-num)) 0))))
    (if (and (phone-onhook?) valid-ip) (begin
      (set! phone:remote (list->u8vector ip-num))
      ((c-lambda (int int) void "opusvoip_call(___arg1,___arg2);")
        (u8vector->s32 phone:local) (u8vector->s32 phone:remote))
      (rtaudio-start 8000 -1.0)
    ))
  ))

;; eof
