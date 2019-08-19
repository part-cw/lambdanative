;; this module creates an android service to drive the native lambdanative payload in the background

(define foreground-service!
  (let ((running #f)
        (start! (c-lambda () void "
#if defined(__ANDROID__)
 android_start_ln_foreground_service();
#endif
"))
        (stop! (c-lambda () void "
#if defined(__ANDROID__)
 android_stop_ln_foreground_service();
#endif
")))
    (lambda (flag)
      (cond
       ((and flag (not running)) (set! running #t) (start!))
       ((and (not flag) running) (set! running #f) (stop!))))))
