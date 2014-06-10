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

;; The server part, supporting APN and GCM
;;(define pushnotification:apn-host "gateway.sandbox.push.apple.com")
(define pushnotification:apn-host "gateway.push.apple.com")
(define pushnotification:apn-port 2195)
(define pushnotification:gcm-host "android.googleapis.com")
(define pushnotification:gcm-port 443)
(define pushnotification:gcm-url "/gcm/send")
(define pushnotification:gcm-user-agent "lambdanative/1.0")

;; get APN/GCM data (ios and android only)
(c-declare  #<<end-of-c-declare
#ifdef IOS
  extern char pushnotification_devicetoken[512];
  extern int pushnotification_gottoken;
#elif ANDROID
  extern char pushnotification_devicetoken[512];
  extern int pushnotification_gottoken;
#else
  char pushnotification_devicetoken[512];
  int pushnotification_gottoken=0;
#endif

void pushnotification_getdevicetoken(char* buf, int len){
  if (len>512 || len==0) return;
  int i;
  for (i=0;i<len;i++){
    buf[i]=pushnotification_devicetoken[i];
  }
}
end-of-c-declare
)


(define pushnotification:gottoken
  (c-lambda () bool "___result = pushnotification_gottoken;"))
(define pushnotification:tokenlen
  (c-lambda () int "___result = pushnotification_gottoken;"))
(define (pushnotification:devicetoken)
  (let ((buf (make-u8vector (pushnotification:tokenlen))))
    ((c-lambda (scheme-object int) void
      "pushnotification_getdevicetoken(___CAST(char*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2);")
        buf (u8vector-length buf))
    buf
  ))
(define (pushnotification-gettoken)
  (if (or (string=? (system-platform) "ios")
          (string=? (system-platform) "android"))
    (if (pushnotification:gottoken) (pushnotification:devicetoken) #f)
    #f
  ))

;;
;; Apple Push Notification functions
;;
(define (pushnotification:apn-makepayload message . badgenumber)
  (if (fx= (length badgenumber) 0)
    (string-append "{\"aps\":{\"alert\":\"" message "\",\"sound\":\"default\"}}")
    (string-append "{\"aps\":{\"alert\":\"" message "\",\"sound\":\"default\",\"badge\":"
                   (number->string (car badgenumber)) "}}")
  ))

(define (pushnotification:apn-makenotation devicetoken payload)
  ;; APN message format is |COMMAND|ID|EXPIRY|TOKENLEN|TOKEN|PAYLOADLEN|PAYLOAD|
  (let ((command (u8vector 1))
        (identifier (u32->u8vector (fix ##now)))
        (expiry (u32->u8vector (fix (+ ##now 300))))
        (tokenlength (u8vector 0 32))
        (payloadlength (u16->u8vector (fix (string-length payload)))))
  (u8vector-append command identifier expiry tokenlength devicetoken
    payloadlength (string->u8vector payload))
  ))

(define (pushnotification:apn-parserror err)
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

(define (pushnotification:apn-parseresponse buf)
  ;; APN response format is |COMMAND|STATUS|IDENTIFIER|
  (if (and (fx= (u8vector-ref buf 0) 8) (fx= (u8vector-ref buf 1) 0)) 
    #t
    (begin 
      (log-error "APN Error: " (pushnotification:apn-parserror (u8vector-ref buf 1)) " id: " (subu8vector buf 2 6)) 
      #f
    )
  ))

(define (pushnotification-apn-send certfile keyfile password token msg . badgenum)
  (let ((ret (httpsclient-key-open pushnotification:apn-host certfile keyfile password pushnotification:apn-port))
        (buf (##still-copy (make-u8vector 6))))
    (if (> ret 0)
      ;; Unfortunately successfully send messages are not replied to, hence no test for the return at the moment.
      (begin 
        (if (fx= (length badgenum) 0)
          (set! ret (httpsclient-send (pushnotification:apn-makenotation token
                                        (pushnotification:apn-makepayload msg))))
          (set! ret (httpsclient-send (pushnotification:apn-makenotation token
                                        (pushnotification:apn-makepayload msg (car badgenum)))))
        )
        (httpsclient-close)
        ret
      )
      #f
    )
  ))

;;
;; Google Cloud Messaging
;;
(define (pushnotification:gcm-makepayload apikey token msg . data)
  (let ((request (string-append "{"
      (string-append "\"data\":{\"msg:" msg "\","
        (if (fx> (length data) 0) 
          (apply string-append (map (lambda (l) (string-append "\"" (car l) "\":\"" (cadr l) "\",")) (car data)))
          ""
        )
        "},"
      )
      "\"registration_ids\":[" 
      (if (list? token)
        (apply string-append (map (lambda (l) (string-append "\"" (u8vector->string l) "\",")) token))
        (string-append "\"" (u8vector->string token) "\"")
      )
      "]}")))
    (string-append "POST " pushnotification:gcm-url " HTTP/1.0" "\n"
      "User-Agent: " pushnotification:gcm-user-agent  "\n"
      "Host: " pushnotification:gcm-host "\n"
      "Authorization: key=" apikey "\n"
      "Content-Type: application/json" "\n"
      "Content-Length: " (number->string (string-length request)) "\n"
      "\r\n" request "\n")
  ))

(define (pushnotification:split-headerbody str)
  (let ((pos (string-contains str "\r\n\r\n")))
    (if pos (list (substring str 0 pos) (substring str (+ pos 4) (string-length str))) (list str (list)))
  ))

(define (pushnotification-gcm-send apikey token msg . data)
  (let ((ret (httpsclient-open pushnotification:gcm-host pushnotification:gcm-port))
        (buf (##still-copy (make-u8vector 1024))))
    (if (fx> ret 0)
      (begin
        (if (fx= (length data) 0)
          (set! ret (httpsclient-send (string->u8vector (pushnotification:gcm-makepayload apikey token msg))))
          (set! ret (httpsclient-send (string->u8vector (pushnotification:gcm-makepayload apikey token msg data))))
        )
        (if (fx> ret 0) (begin
          (set! ret (httpsclient-recv buf))
          (if (fx> ret 0) 
            (let ((res (pushnotification:split-headerbody (u8vector->string buf))))
              (httpsclient-close)
              (if (and (string? (car res)) (fx> (string-length (car res)) 12) 
                    (or (string=? (substring (car res) 9 12) "201")
                        (string=? (substring (car res) 9 12) "200")))
                (cadr res)
                (begin
                  (log-error "Pushnotification GCM error: " (cadr res))
                   #f
                )
              )
            )
            #f
          )
        ))
      )
      #f
    )
  ))

;;
;; Testing function that shows how it can be done.
;;
(define (pushnotification-apn-test)
  (let ((token (u8vector 6 80 12 18 45 58 143 7 178 250 237 193 201 
                         120 50 201 78 74 60 137 25 17 172 83 254 243 15 57 240 135 237 234))
        (message "Hello World! This is a test.")
        (message2 "LambdaNative with BadgeNum")
        (certfilename "AppNameCert.pem")
        (keyfilename "AppNameKey.pem")
        (password "AppName-pw"))
    (display (pushnotification-apn-send certfilename keyfilename password token message)) (newline)
    (thread-sleep! 5)
    (display (pushnotification-apn-send certfilename keyfilename password token message2 99)) (newline)
  ))

(define (pushnotification-gcm-test)
  (let ((apikey "ApiKeyYouGotFromGoogleDeveloperConsole")
        (token (string->u8vector "ABC")))
    (display (pushnotification-gcm-send apikey token))
 ))

;;eof
