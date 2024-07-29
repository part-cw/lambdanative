#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2021, University of British Columbia
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

;; Percent encoding for special characters
(define (atalk:percent-encode str)
  (string-replace-substring 
  (string-replace-substring
  (string-replace-substring
  (string-replace-substring  
  (string-replace-substring
  (string-replace-substring
  (string-replace-substring  
  (string-replace-substring
  (string-replace-substring
  (string-replace-substring  
  (string-replace-substring
  (string-replace-substring
  (string-replace-substring  
  (string-replace-substring
  (string-replace-substring
  (string-replace-substring
  (string-replace-substring  
  (string-replace-substring
  (string-replace-substring
  (string-replace-substring
  (string-replace-substring
  (string-replace-substring  
  (string-replace-substring str 
    "%" "%25")
    " " "%20")
    "\n" "%0A")
    "\r" "%0D")
    "\t" "%09")
    "!" "%21")
    "#" "%23")
    "$" "%24")
    "&" "%26")
    "'" "%27")
    "(" "%28")
    ")" "%29")
    "*" "%2A")
    "+" "%2B")
    "," "%2C")
    "/" "%2F")
    ":" "%3A")
    ";" "%3B")
    "=" "%3D")
    "?" "%3F")
    "@" "%40")
    "[" "%5B")
    "]" "%5D"))

(define (atalk:split-headerbody str)
  (let ((pos (string-contains str "\r\n\r\n")))
    (if pos 
      (list (substring str 0 pos) (substring str (+ pos 4) (string-length str))) 
      (list str (list)))))

;; For creating POST request to Africa's Talking
(define (atalk:make-request-str host url username apikey number message from)
  (let* ((requestTo (string-append "username=" username "&to=" (atalk:percent-encode number)))
         (requestFrom (if from (string-append "&from=" from) ""))
         (request (string-append requestTo requestFrom "&message=" (atalk:percent-encode message))))
    (string-append "POST " url " HTTP/1.1" "\r\n"
      "Host: " host "\r\n"
      "Content-Length: " (number->string (string-length request)) "\r\n"
      "Content-Type: application/x-www-form-urlencoded" "\r\n"
      "Accept: application/json" "\r\n"
      "apiKey: " apikey "\r\n"
      "\r\n" request "\n")))

;; Send the message and return the response
(define (atalk:sendSMS host url username apikey number message . sender)
  (let* ((buf (##still-copy (make-u8vector 1024)))
        (conn (httpsclient-open host))
        (from (if (and (list? sender) (fx> (length sender) 0) (string? (car sender))) (car sender) #f))
        (request-str (atalk:make-request-str host url username apikey number message from)))
    (httpsclient-send (string->u8vector request-str))
    (httpsclient-recv buf)
    (httpsclient-close)
    (u8vector->string buf)))
