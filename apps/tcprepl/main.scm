;; evals request, printing each return value to port
(define (tcprepl:eval-print-fn request port)
  (map
   (lambda (value)
     (display value port)
     (newline port))
   (call-with-values
       (lambda ()
         (eval request))
     list)))

;; Print errors using a special magic syntax
(define (tcprepl:print-error e port)
  (display "#E(\"" port)
  (display (exception->string e) port)
  (display "\")" port))

;;Wrap read, eval and print in an error handler
(define (tcprepl:read-eval-print-fn port)
  (with-exception-catcher
   (lambda (e)
     (log-error e)
     (tcprepl:print-error e port))
   (lambda ()
     (let ((request (read port)))
       (tcprepl:eval-print-fn request port)))))

;; opens port and connection
(define (tcprepl:repl-fn)
  (let ((accept-port (open-tcp-server (list server-address: "*"
                                            port-number: 8000 reuse-address: #t))))
    (let loop ()
      (let ((connection (read accept-port)))
        (if (not (eof-object? connection))
            (begin
              (thread-start! (make-thread
                              (lambda ()
                                (tcprepl:read-eval-print-fn connection)
                                (close-port connection))))
              (loop)))))))

;; Fire up tcp repl and debug repl
(begin
  (thread-start! (make-thread tcprepl:repl-fn))
  (let loop ()
    (with-exception-catcher (lambda (e)
                              (display (exception->string e))
                              (newline))
                            ##repl-debug)
    (loop)))

;; eof
