;; this is the Gambit web-repl with a few tweaks
;; this module automatically starts a REPL server on port 7000

(##namespace ("ln_repl#"))
(##include "~~lib/gambit#.scm")
(##namespace ("" system-appname system-appversion exception->string
                 fix secondselapsed->string system-buildepoch system-builddatetime))
(##include "~~lib/define-global-macros.scm")

(define repl-server-address "*:7000")

(define (ide-repl-pump ide-repl-connection in-port out-port tgroup)
  (define m (make-mutex (gensym 'ide-repl-pump)))
  (define (process-input)
    (let loop ((state 'normal))
      (let ((c (read-char ide-repl-connection)))
        (if (not (eof-object? c))
	    (case state
	      ((normal)
	       (if (char=? c #\xff) ;; telnet IAC (interpret as command) code?
		   (loop c)
		   (begin
		     (mutex-lock! m)
		     (if (char=? c #\x04) ;; ctrl-d ?
			 (close-output-port out-port)
			 (begin
			   (write-char c out-port)
			   (force-output out-port)))
		     (mutex-unlock! m)
		     (loop state))))
	      ((#\xfb) ;; after WILL command?
	       (loop 'normal))
	      ((#\xfc) ;; after WONT command?
	       (loop 'normal))
	      ((#\xfd)	     ;; after DO command?
	       (if (char=? c #\x06) ;; timing-mark option?
		   (begin	    ;; send back WILL timing-mark
		     (mutex-lock! m)
		     (write-char #\xff ide-repl-connection)
		     (write-char #\xfb ide-repl-connection)
		     (write-char #\x06 ide-repl-connection)
		     (force-output ide-repl-connection)
		     (mutex-unlock! m)))
	       (loop 'normal))
	      ((#\xfe) ;; after DONT command?
	       (loop 'normal))
	      ((#\xff) ;; after IAC command?
	       (case c
		 ((#\xf4) ;; telnet IP (interrupt process) command?
		  (for-each
		   ##thread-interrupt!
		   (thread-group->thread-list tgroup))
		  (loop 'normal))
		 ((#\xfb #\xfc #\xfd #\xfe) ;; telnet WILL/WONT/DO/DONT command?
		  (loop c))
		 (else
		  (loop 'normal))))
	      (else
	       (loop 'normal)))))))

  (define (process-output)
    (let loop ()
      (let ((c (read-char in-port)))
	(if (not (eof-object? c))
	    (begin
	      (mutex-lock! m)
	      (write-char c ide-repl-connection)
	      (force-output ide-repl-connection)
              (mutex-unlock! m)
	      (loop))))))


  (let ((tgroup (make-thread-group 'repl-pump #f)))
    (thread-start! (make-thread process-input #f))
    (thread-start! (make-thread process-output #f tgroup))))


        (define (make-ide-repl-ports ide-repl-connection tgroup)
	  (receive (in-rd-port in-wr-port) (open-string-pipe '(direction: input permanent-close: #f))
		   (receive (out-wr-port out-rd-port) (open-string-pipe '(direction: output))
			    (begin

			      ;; Hack... set the names of the ports for usage with gambit.el
			      (##vector-set! in-rd-port 4 (lambda (port) '(stdin)))
			      (##vector-set! out-wr-port 4 (lambda (port) '(stdout)))


			      (ide-repl-pump ide-repl-connection out-rd-port in-wr-port tgroup)
			      (values in-rd-port out-wr-port)))))

        (define repl-channel-table (make-table test: eq?))


        (set! ##thread-make-repl-channel
        (lambda (thread)
        (let ((tgroup (thread-thread-group thread)))
        (or (table-ref repl-channel-table tgroup #f)
        (##default-thread-make-repl-channel thread)))))


        (define (setup-ide-repl-channel ide-repl-connection tgroup)
        (receive (in-port out-port) (make-ide-repl-ports ide-repl-connection tgroup)
        (let ((repl-channel (##make-repl-channel-ports in-port out-port)))
        (table-set! repl-channel-table tgroup repl-channel))))

(define (ln-repl-banner)
  (##write-string "----" (##repl-output-port))
  (##newline (##repl-output-port))
  (##write-string "LambdaNative REPL" (##repl-output-port))
  (##newline (##repl-output-port))
  (##write-string (string-append (system-appname) " " (system-appversion)) (##repl-output-port))
  (##newline (##repl-output-port))
  (let* ((now (fix (time->seconds (current-time))))
         (thn (system-buildepoch))
         (str (secondselapsed->string (- now thn) "%Hh %Mm %Ss ago")))
    (##write-string (string-append "Built " (system-builddatetime) " (" str ")") (##repl-output-port))
    )
  (##newline (##repl-output-port))
  )

(define (ln-repl-exception e)
  (##write-string (with-output-to-string ""
    (lambda () (display (exception->string e))))
		  (##repl-output-port))
  (##newline (##repl-output-port))
  (##repl-debug #f #t))

(define (start-safe-ide-repl)
  (ln-repl-banner)
  (with-exception-handler ln-repl-exception (lambda () (##repl-debug #f #t))))

(define (repl-server)
  (map eval global-macros)
  (let ((server
	 (open-tcp-server
	  (list server-address: repl-server-address
		reuse-address: #t))))
    (let loop ()
      (let* ((ide-repl-connection
	      (read server))
	     (tgroup
	      (make-thread-group 'repl-service #f))
	     (thread
	      (make-thread
	       (lambda ()
		 (setup-ide-repl-channel ide-repl-connection tgroup)
		 (start-safe-ide-repl))
	       'repl
	       tgroup)))
        (thread-start! thread)
        (loop)))))

(define (ln-repl-start!)
  (thread-start! (make-thread (lambda () (repl-server)))))

(ln-repl-start!)

;; eof
