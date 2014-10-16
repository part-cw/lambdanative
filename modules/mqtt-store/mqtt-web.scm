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

;; basic mqtt inspector (for test and debugging) - this starts a webserver
(define mqttweb:serverupdaterate 2)

(define (mqttweb:safecall x . y)
  (with-exception-catcher
    (lambda (e) (log-error "mqttweb:safecall : exception: " (exception->string e)) #f)
    (lambda () (apply x y))))

(define (mqttweb:format val)
  (cond 
    ((string? val) val)
    ((number? val) (number->string val))
    ((list? val) (string-append "list[" (number->string (length val)) "]"))
    ((u8vector? val) (string-append "u8vector[" (number->string (u8vector-length val))"]"))
    ((boolean? val) (if val "TRUE" "FALSE"))
    (else "??")
  ))

(define (mqtt-web store port)
  (thread-start! (make-safe-thread (mqttweb:server store port) 'mqtt-web))
)

(define (mqttweb:server store port)
  (lambda ()
  (let* ((accept-port (open-tcp-server (list server-address: "*" port-number: port reuse-address: #t))))
    (let loop () (let ((connection (read accept-port)))
      (if (not (eof-object? connection)) (begin 
        (thread-start! (make-safe-thread (lambda () (mqttweb:serve store connection))))
        (loop)
      ))
    ))
  )))

(define (mqttweb:serve store port)
  (with-exception-catcher 
    (lambda (e) (log-error "MQTTweb:" e) (mqttweb:safecall close-port port) #f)
    (lambda () (let ((request (read-line port)))
      (if (string? request)
        (let* ((r (string-split request #\space))
               (m (car r)))
          (if (string-ci=? m "GET") (mqttweb:html store port (cadr r)))
            (force-output port)
            (close-port port)
        )))
    )
  ))

(define (mqttweb:html store port document)
  (display "HTTP/1.0 200 OK\ncontent-type: text/html\n\n" port)
  (sxml->xml `(html
    (head
      (title "MQTT: Inspector")
      (meta (@ ("http-equiv" "refresh") ("content" ,(number->string mqttweb:serverupdaterate))))
    )
    (body
      (h2 "MQTT: Inspector")
      (font (@ (face "courier") (size "-1"))
        ,(let loop ((color "#ffffff")
                    (es (sort (store-listcat store "mqtt") (lambda (a b) (string<=? (car a) (car b)))))
                    (res '(table (@ (cellpadding "0")(cellspacing "0")(border "0")) 
                            (tr (th "Topic") (th "Value")))))
           (if (fx= (length es) 0) res
             (loop (if (string=? color "#ffffff") "#eeeeee" "#ffffff") (cdr es)
               (append res `((tr (@ (bgcolor ,color))
                                 (td (@ (width 800)) ,(car (car es)))
                                 (td ,(let ((val (cadr (car es)))) (mqttweb:format val))))))
             )
           ))
        (hr)
        ,(system-appname) " v" ,(system-appversion) " [" ,(system-buildhash)  " built " ,(system-builddate) "]" (p)
        (b "Powered by " (a (@ (href http://www.lambdanative.org)) "LambdaNative")) (p)
     ))
   ) port))

;; eof
