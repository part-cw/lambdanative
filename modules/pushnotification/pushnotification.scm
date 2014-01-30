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

;; get APN data (ios only)
(c-declare  #<<end-of-c-declare
#ifdef IOS
  extern char ios_pushnotification_devicetoken[32];
  extern char ios_pushnotification_gottoken;
#else
  char ios_pushnotification_devicetoken[32];
  char ios_pushnotification_gottoken=0;
#endif

void ios_pushnotification_getdevicetoken(char* buf, int len){
  int i;
  for (i=0;i<len;i++){
    buf[i]=ios_pushnotification_devicetoken[i];
  }
}
end-of-c-declare
)


(define pushnotification:gottoken
  (c-lambda () bool "___result = ios_pushnotification_gottoken;"))
(define (pushnotification:devicetoken)
  (let ((buf (make-u8vector 32)))
    ((c-lambda (scheme-object int) void
      "ios_pushnotification_getdevicetoken(___CAST(char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
        buf (u8vector-length buf))
    buf
  ))

(define (pushnotification-gettoken)
  (if (pushnotification:gottoken)
    (pushnotification:devicetoken)
    #f
  )
)

(define pushnotification:host "gateway.sandbox.push.apple.com")
;;(define pushnotification:host "gateway.push.apple.com")
(define pushnotification:port 2195)

(define (pushnotification:makepayload message . badgenumber)
  (if (fx= (length badgenumber) 0)
    (string-append "{\"aps\":{\"alert\":\"" message "\",\"sound\":\"default\"}}")
    (string-append "{\"aps\":{\"alert\":\"" message "\",\"sound\":\"default\",\"badge\":"
                   (number->string (car badgenumber)) "}}")
  ))

(define (pushnotification:makenotation devicetoken payload)
  ;; APN message format is |COMMAND|ID|EXPIRY|TOKENLEN|TOKEN|PAYLOADLEN|PAYLOAD|
  (let ((command (u8vector 1))
        (identifier (u32->u8vector (fix ##now)))
        (expiry (u32->u8vector (fix (+ ##now 300))))
        (tokenlength (u8vector 0 32))
        (payloadlength (u16->u8vector (fix (string-length payload)))))
  (u8vector-append command identifier expiry tokenlength devicetoken
    payloadlength (string->u8vector payload))
  ))

(define (pushnotification:parserror err)
  (cond ((fx= err 1) "Processing error")
        ((fx= err 2) "Missing device token")
        ((fx= err 3) "Missing topic")
        ((fx= err 4) "Missing payload")
        ((fx= err 5) "Invalid token size")
        ((fx= err 6) "Invalid topic size")
        ((fx= err 7) "Invalid payload size")
        ((fx= err 8) "Invalid token")
        ((fx= err 10) "Shutdown")
        (else "UNKNOWN")
  ))

(define (pushnotification:parseresponse buf)
  ;; APN response format is |COMMAND|STATUS|IDENTIFIER|
  (if (and (fx= (u8vector-ref buf 0) 8) (fx= (u8vector-ref buf 1) 0)) 
    #t
    (begin 
      (log-error "APN Error: " (pushnotification:parserror (u8vector-ref buf 1)) " id: " (subu8vector buf 2 6)) 
      #f
    )
  ))

(define (pushnotification-send certfile keyfile password token msg . badgenum)
  (let ((ret (httpsclient-key-open pushnotification:host certfile keyfile password pushnotification:port))
        (buf (##still-copy (make-u8vector 6))))
    (if (> ret 0)
      ;; Unfortunately successfully send messages are not replied to, hence no test for the return at the moment.
      (if (fx= (length badgenum) 0)
        (httpsclient-send (pushnotification:makenotation token (pushnotification:makepayload msg)))
        (httpsclient-send (pushnotification:makenotation token (pushnotification:makepayload msg (car badgenum))))
      )
      #f
    )
  ))

;; Testing function that shows how it can be done.
(define (pushnotification-test)
  (let ((token (u8vector 166 240 26 12 90 100 191 58 48 224 18 178 53 157 103 79 
                         156 174 38 103 112 139 18 47 65 174 182 196 16 207 74 2))
        (message "Hello World! This is a test.")
        (message2 "LambdaNative with BadgeNum")
        (certfilename "AppNameCert.pem")
        (keyfilename "AppNameKey.pem")
        (password "AppName-pw"))
    (display (pushnotification-send certfilename keyfilename password token message)) (newline)
    (thread-sleep! 5)
    (display (pushnotification-send certfilename keyfilename password token message2 99)) (newline)
  ))
