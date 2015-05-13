;; starts thread
(define (tcprepl-server)
  (thread-start! (make-thread tcprepl:server)))

;; evals incoming lisp on a port until
(define (eval-printer request port)
  (map
   (lambda (value)
     (display value port)
     (newline port))
   (call-with-values
       (lambda ()
         (eval request))
     list)))

(define (tcprepl:serve port)
  (with-exception-catcher
   (lambda (e)
     (log-error e)
     (display "#E(\"" port)
     (display (exception->string e) port)
     (display "\")" port)
     (close-port port)
     #f)
   (lambda ()
     (let ((request (read port)))
       (eval-printer request port)
       (close-port port)))))

;; opens port and connection
(define (tcprepl:server)
  (let ((accept-port (open-tcp-server (list server-address: "*"
                                            port-number: 8000 reuse-address: #t))))
    (let loop ()
      (let ((connection (read accept-port)))
        (if (not (eof-object? connection))
            (begin (thread-start! (make-thread
                                   (lambda ()
                                     (tcprepl:serve connection))))
                   (loop)))))))

(begin
  (tcprepl-server)
  (let loop ()
    (with-exception-catcher (lambda (e)
                              (display (exception->string e))
                              (newline))
                            ##repl-debug)
    (loop)))

;; (with-exception-catcher (lambda (e)
;;                           (display e (current-output-port)))
;;                         (lambda () (eval 'foo)))

;; eof
