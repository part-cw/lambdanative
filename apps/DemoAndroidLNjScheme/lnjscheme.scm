(cond-expand
 (android (define android-app-class (c-lambda () char-string "___result=android_app_class();")))
 (else (define (android-app-class) "android-app-class")))

(define lnjscheme-eval
  ;; Not sure that we need a mutex here.  But what if the java side
  ;; manages to call into gambit?
  (let ((mutex (make-mutex 'lnjscheme)))
    (define lnjscheme-invoke/s2s
      (c-lambda (char-string) char-string "
#ifdef ANDROID
extern const char *lnjscheme_eval(const char *);
#endif
___result=
#ifdef ANDROID
(char*) lnjscheme_eval(___arg1);
#else
NULL;
#endif
"))
    (define (lnjscheme-call obj)
      (let* ((s (let ((req (object->string obj)))
                  (mutex-lock! mutex)
                  (cond-expand
                   (android (lnjscheme-invoke/s2s req))
                   (else (error "lnjscheme-call: not availible on platform" (system-platform))))))
             (r0 (begin
                   (mutex-unlock! mutex)
                   (if (string? s)
                       (call-with-input-string s
                         (lambda (port)
                           (let* ((key (read port))
                                  (value (read port)))
                             (case key
                               ((D) value)
                               ((E) (raise value))
                               (else (error "lnjscheme-call: unexpected reply " s))))))
                       (error "lnjscheme-call: unexpected reply " s)))))
        (cond
         ;; Numbers are always printed as inexacts by jscheme.
         ((integer? r0) (inexact->exact r0))
         (else r0))))
  lnjscheme-call))
