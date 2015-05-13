
;; starts thread
(define (tcprepl-server) (thread-start! (make-thread tcprepl:server)))


(call-with-values (lambda () (values 1 2)) list)

;; opens port and connection
(define (tcprepl:server)
  (let ((accept-port (open-tcp-server (list server-address: "*"
                                            port-number: 8000 reuse-address: #t))))
    (let loop ()
      (let ((connection (read accept-port)))
        (if (not (eof-object? connection))
            (begin (thread-start! (make-thread
                                   (lambda ()
                                     (tcprepl:serve connection) )))
                   (loop)))))))

(define (eval-printer request port)
  (map
   (lambda (value)
     (display value port)
     (display "
" port))
   (call-with-values (lambda () (eval request))
     list)))

(define (tcprepl:serve port)
  (with-exception-catcher (lambda (e)
                            (log-error e)
                            (force-output port)
                            (close-port port))
    (lambda ()
      (let ((request (read port)))
        (if (list? request)
            (with-exception-catcher (lambda (e)
                                      (log-error e)
                                      (force-output port)
                                      (close-port port)
                                      #f)
                                    (eval-printer request port)
                                    (force-output port)))))))

(begin
  (tcprepl-server)
  (let loop ()
    (with-exception-catcher (lambda (e)
                              (for-each display (list (exception->string e) "\n")) #f)
                            (lambda () (##repl-debug)))
    (loop)))

;; eof
