;; this module creates an android service to drive the native lambdanative payload in the background

;; Usage:
;;
;; (foreground-service! #t) ;; start service
;;
;; (foreground-service! #f) ;; stop service

(c-declare "int android_start_ln_foreground_service();")
(c-declare "int android_stop_ln_foreground_service();")

(define foreground-service!
  (let ((running #f)
        (start! (c-lambda () int "
#if defined(__ANDROID__)
 ___return(android_start_ln_foreground_service());
#else
 ___return(0);
#endif
"))
        (stop! (c-lambda () int "
#if defined(__ANDROID__)
 ___return(android_stop_ln_foreground_service());
#else
 ___return(0);
#endif
")))
    (lambda (flag)
      (cond
       ((and flag (not running))
        (set! running #t)
        (let ((result (start!)))
          (when (negative? result) (log-error "foreground-service! failed to start " result))))
       ((and (not flag) running)
        (set! running #f)
        (let ((result (stop!)))
          (when (negative? result) (log-error "foreground-service! failed to stop " result))))))))
