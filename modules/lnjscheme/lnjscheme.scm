(cond-expand
 (android
  (c-declare "extern const char* android_app_class();")
  (define android-app-class (c-lambda () char-string "android_app_class")))
 (else (define (android-app-class)
         (log-error "android-app-class: called in non-Android context")
         "android-app-class")))

(define call-with-lnjscheme-result
  ;; SIGNATURE (NAME S-EXPR #!optional (RECEIVER force))
  ;;
  ;; S-EXPR: Scheme s-expression in JSCM dialect
  ;;
  ;; RECEIVER: 1-ary procedure signature copatible to `force`
  ;;
  ;; Note: On the RECEIVERs descrection the result could be `forced`ed
  ;; with, e.g., exception handlers in place, another threads context.
  ;; The default is to fail as early as possible: on the events
  ;; reception.  However delaying the failure into the RECEIVERs
  ;; context has the advantage of more focused failure location while
  ;; risking to mask failures in background processing.
  ;;
  ;; Ergo: fail immeditately when testing or in background.  RECEIVER,
  ;; if provided, may override.
  (let ()
    (define jscheme-send
      (c-lambda (char-string) void "
#ifdef ANDROID
extern void lnjscheme_eval_send(const char *);
lnjscheme_eval_send(___arg1);
#endif
"))
    (define jscheme-receive
      (c-lambda () char-string "
#ifdef ANDROID
extern const char *lnjscheme_eval_receive_result();
#endif
___result=
#ifdef ANDROID
(char*) lnjscheme_eval_receive_result();
#else
NULL;
#endif
"))
    (define (jscheme-read-reply obj)
      (if (string? obj)
          (call-with-input-string
           obj
           (lambda (port)
             (let* ((key (read port))
                    (value
                     (with-exception-catcher
                      (lambda (exn) (raise (string-append "jscheme-call: unreadable result: " obj)))
                      (lambda () (read port)))))
               (case key
                 ((D) value)
                 ((E) (raise value))
                 (else (error "jscheme-call: unexpected reply " obj))))))
          (error "jscheme-call: unexpected reply " obj)))
    (define (jscheme-refine-result obj)
      (cond
       ;; Numbers are always printed as inexacts by jscheme.
       ((integer? obj) (inexact->exact obj))
       (else obj)))
    (define (jscheme-call obj #!optional (receiver force))
      (cond-expand
       (android)
       (else (log-error "jscheme-call: not availible on platform" (system-platform))))
      (on-jscm-result
       (lambda (t x y)
         (let* ((reply (jscheme-receive)) ;; extract the result from Java
                ;; delay evalutation
                (promise (delay (jscheme-refine-result (jscheme-read-reply reply)))))
           ;; The optional receiver MAY either dispatch to
           ;; asynchroneous forcing the promise catching exceptions
           ;; etc. by default force it expection the application to
           ;; abort on any exception.
           (receiver promise))))
        (jscheme-send (object->string obj))
        (thread-yield!))
  jscheme-call))

(define (lnjscheme-future obj)
  ;; a promise waiting for the evaluation of OBJ
  (let ((result (make-mutex obj)))
    (mutex-lock! result #f #f)
    (call-with-lnjscheme-result
     obj
     (lambda (promise)
       (mutex-specific-set! result promise)
       (mutex-unlock! result)))
    (delay
      (begin
        (mutex-lock! result #f #f)
        (force (mutex-specific result))))))

(define (lnjscheme-eval obj)
  ;; BEWARE: This blocks the current thread.  WILL deadlock NOT when
  ;; run in event handler thread.
  (force (lnjscheme-future obj)))
