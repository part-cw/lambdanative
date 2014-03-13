
;;(include "../../libs/gambit/common.sch")
;;(include "../../libs/gambit/myenv.sch")
;;(include "../../multi-parser/id/srfi-12.sch")
;;(include "../../libs/input-parse.sch")
;;(include "../../multi-parser/id/http.sch")

; Top-level cond-expand expanded automatically
(define (open-tcp-connection host port-number)
  (assert (integer? port-number) (positive? port-number))
  (let ((p
         (open-tcp-client
           (list server-address: host port-number: port-number))))
    (cons p p)))

(define flush-output-port force-output)

(define close-tcp-connection close-port)

(define shutdown-sender force-output)

; The body of the function. 
; The function is written as a collection of mutually-recursive
; procedures that implement a transactional FSM.

(define (http-transaction req-method req-url req-parms response-handler)

  ; expected keyword arguments and their default values
  (define-def http-proxy req-parms  #f)
  (define-def user-agent req-parms  "Scheme-HTTP/1.0")
  (define-def http-req   req-parms  '())
  (define-def logger     req-parms
    (lambda (port msg . other-msgs) (cerr msg other-msgs nl)))

  (define CRLF (string (integer->char 13) (integer->char 10)))

  (define (die reason headers port)
    (if port (close-output-port port))
    (abort (make-property-condition 'HTTP-TRANSACTION
	      'REASON reason 'HEADERS headers)))

  ; re-throw the exception exc as a HTTP-TRANSACTION exception
  (define (die-again exc reason headers port)
    (if port (close-output-port port))
    (abort (make-composite-condition
	    (make-property-condition
	     'HTTP-TRANSACTION 'REASON reason 'HEADERS headers)
	    exc)))

  ; Open a connection, send the request, and if successful,
  ; invoke the read-resp-status-line on the opened http-port.
  (define (make-req schema dummy host resource)
    (let* ((target-host (or http-proxy host))
	   (target-addr-lst (ssax-string-split target-host '(#\:)))
	   (target-host-proper (car target-addr-lst))
	   (target-port
	    (if (pair? (cdr target-addr-lst))
		(string->integer (cadr target-addr-lst) 0
				 (string-length (cadr target-addr-lst)))
		80))
	   (dummy (logger #f "Connecting to " target-host-proper ":"
			  target-port))
	   ; prevent hacking
	   (dummy (if (string-index target-host-proper #\|)
		      (error "Bad target addr: " target-host-proper)))
	   (http-ports (open-tcp-connection target-host-proper target-port))
	   (http-i-port (car http-ports))
	   (http-o-port (cdr http-ports))
	   )

      (for-each
       (lambda (str) (display str http-o-port))
       `(,req-method " "
		  ; if the proxy is set, request the full REQ-URL; otherwise,
		  ; send only the relative URL
	 ,@(if http-proxy (list req-url) (list "/" resource))
	 " HTTP/1.0" ,CRLF
	 "Host: " ,host ,CRLF
	 "User-agent: " ,user-agent ,CRLF
	 "Connection: close" ,CRLF))
      (if (procedure? http-req)
	  (http-req http-o-port)	; let the user write other headers
	  (begin
	    (for-each (lambda (header-name-value)
			(display (car header-name-value) http-o-port)
			(write-char #\: http-o-port)
			(display (cdr header-name-value) http-o-port)
			(display CRLF http-o-port))
		      http-req)
	    (display CRLF http-o-port) ; An empty line ends headers
	    ))
      (flush-output-port http-o-port)
      (shutdown-sender http-o-port)
      (logger http-o-port "sent request. Now listening for the response...")
      (read-resp-status-line http-i-port)))


  ; Read the first line of the server's response, something like
  ; HTTP/1.x 200 OK
  ; and extract the response code
  ; Invoke
  ;  read-headers http-i-port resp-code
  ;		'(HTTP-RESPONSE . the-whole-response-line)
  ; or raise an exception if the response line is absent or invalid
  (define (read-resp-status-line http-port)
    (let* ((resp-line (read-line http-port))
	   (dummy (logger http-port "Got response :" resp-line)) 
	   (resp-headers (list (cons 'HTTP-RESPONSE resp-line))))
      (cond
       ((eof-object? resp-line)
	(die 'NO-REPLY '() http-port))
       ((not (string-prefix? "HTTP/1." resp-line))
	(die 'BAD-RESP-LINE resp-headers http-port))
       (else
	(let* ((resp-line-parts (ssax-string-split resp-line '() 3))
	       (resp-code
		(and (pair? resp-line-parts)
		     (pair? (cdr resp-line-parts))
		     (string->integer (cadr resp-line-parts) 0
				      (string-length (cadr resp-line-parts)))))
	       )
	  (if resp-code
	      (read-headers http-port resp-code resp-headers)
	      (die 'BAD-RESP-LINE resp-headers http-port)))))))


  ; read-headers http-port resp-code init-resp-headers
  ; The http-port is positioned after the response line.
  ; The procedure reads HTTP response headers and adds them to
  ; init-resp-headers.
  ; On success, the procedure exits to response-handler, passing
  ; it the response code, the read headers and the http-port. The
  ; port is positioned after the empty line that terminates the headers.
  (define (read-headers http-port resp-code init-resp-headers)
    (let ((headers
	   (with-exception-handler
            (lambda (exc)
              (die-again exc 'BAD-HEADER init-resp-headers http-port))
            (lambda ()
              (MIME:read-headers http-port)))))
      (response-handler resp-code (append init-resp-headers headers)
			 http-port)))

  ; parse the req-url and exit either to make-req, or to 
  ; the response-handler to handle the error
  (let ((url-parts (ssax-string-split req-url '(#\/) 4)))
    ; this stub is added by Dmitry Lizorkin for handling URIs consisting of
    ; just a schema and a host, say, "http://www.plt-scheme.org"
    (let ((url-parts
           (if (and (string=? "http:" (car url-parts))
                    (= 3 (length url-parts)))
               (append url-parts '(""))
               url-parts)))    
    (cond
     ((not (= 4 (length url-parts)))
      (die 'BAD-REQ-URL '() #f))
     ((string=? "http:" (car url-parts))
      (apply make-req url-parts))
     (else
      (die 'UNSUPPORTED-SCHEMA '() #f)
      ))))
)
