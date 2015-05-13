(ql:quickload :usocket)


(defun shebang-reader (stream sub-char num-args)
  (declare (ignore sub-char num-args))
  (unless (equal (read stream) 'void)
    (error "what the hell is this!?")))

(setf (readtable-case *readtable*) :invert)
(set-dispatch-macro-character #\# #\! #'shebang-reader)

(defun dump-stream-to-string (stream)
  (labels ((foo (so-far)
             (let ((newline (read-line stream nil)))
               (if newline
                   (foo (concatenate 'string so-far newline (string #\newline)))
                   so-far))))
    (string-right-trim '(#\newline)(foo ""))))

(defun map-thing (fn thing)
  (let ((chunk (funcall fn thing)))
    (if chunk
        (cons chunk (map-thing fn thing)))))

(defun scheme-eval (cmd-string sock)
  (write-line cmd-string
              (usocket:socket-stream sock))
  (force-output (usocket:socket-stream sock)))

(defun scheme-repl (cmd)
  (let* ((cmd-string (with-output-to-string (bar)
                       (print cmd bar)))
         (foo   (usocket:socket-connect "localhost" 8000)))
    (format t "outputting cmd-string ~a~%" cmd-string)
    (unwind-protect
         (progn (scheme-eval cmd-string foo)
                (let* ((return-string (dump-stream-to-string
                                       (usocket:socket-stream foo)))
                       (dummy-stream (make-string-input-stream return-string)))
                  (format t "received reply-string: ~a~%" return-string)
                  (apply #'values
                         (map-thing (lambda (stream)
                                      (read stream nil))
                                    dummy-stream))))
      (usocket:socket-close foo))))

(defmacro in-scheme (&rest rest)
  `(scheme-repl (quote ,(cons 'begin rest))))
