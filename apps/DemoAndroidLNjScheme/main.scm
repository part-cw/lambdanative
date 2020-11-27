;; LambdaNative LNjScheme Demo

(cond-expand
 ;; FIXME: There where several issues (at least on Android 8) whenever
 ;; (terminate) was called.  Exacerbated (or always?) when onPause of
 ;; the GLSurfaceView is invoked.
 ;;
 ;; Not actually terminating seems to fix the issue.  Sure there
 ;; should be a better solution.
 (android
  (set! terminate (lambda () (log-error "No terminate on Android!") #f))
  )
 (else))

(define (try-LNjScheme)
  (define (try-expr expr)
    (display "Input:\n")
    (pretty-print expr)
      (let ((result (lnjscheme-eval expr)))
      (display "Result: ")
      (pretty-print result)))
  (define (try-exprs exprs)
    (with-exception-catcher
     (lambda (exn)
       (display "EXN: ")
       (display-exception exn)
       (newline))
     (lambda () (for-each try-expr (force exprs)))))
  (define (file-result fn)
    (with-output-to-string
      (lambda ()
        (try-exprs
         (delay
           (let ((exprs (call-with-input-file fn read-all))) ;; 1st read them all
             ;; FIXME: Do we need this here?
             (set! exprs (cons `(define (android-app-class) ,(android-app-class)) exprs))
             exprs))))))
  (define (try-file! fn)
    (thread-start! (make-thread (lambda () (dbset 'testresults (file-result fn))) fn)))
  (let ((fn test-path-name))
    ;; Important: We must return from the button's action immediately,
    ;; hence running the actual change in background thread.
    (if (file-exists? fn)
        (try-file! fn)
        (dbset 'testresults (string-append "failed to find file: " fn))))
  #f)

(define test-file-name "lnjstest.scm")
(define test-path-name (string-append (system-directory) (system-pathseparator) test-file-name))

(define (make-uiforms)
  `(
    (main
    "LNjScheme"
    #f
    #f
    (button
     text "Try Webview" action
     ,(lambda ()
        (webview-launch! "http://www.lambdanative.org" via: 'webview)
        #f))
    (spacer)
    (label text ,(string-append "Push Button to load '" test-path-name "'"))
    (spacer)
    (button text "Load It!" action ,try-LNjScheme)
    (spacer)
    ,(lambda () `(label align left size small text ,(dbget 'testresults "<no results yet>")))
    )
    ))

(define gui #f)
(define form #f)

(main
 ;; initialization
 (lambda (w h)
   (make-window 320 480)
   (glgui-orientation-set! GUI_PORTRAIT)
   (set! gui (make-glgui))

   ;; initialize gui here

   (let ((aw (glgui-width-get))
         (ah (glgui-height-get)))
     (glgui-box gui 0 0 aw ah DarkGreen)
     (set! form (glgui-uiform gui 0 0 aw ah)))
   
   (glgui-widget-set! gui form 'uiform (list->table (make-uiforms)))

   ;; Set the fonts
   (glgui-widget-set! gui form 'fnt ascii_18.fnt)
   (glgui-widget-set! gui form 'smlfnt ascii_14.fnt)
   (glgui-widget-set! gui form 'hdfnt ascii_24.fnt)
   (glgui-widget-set! gui form 'bigfnt ascii_40.fnt)

   ;; Create the table to store data (default location for widget values)
   (glgui-widget-set! gui form 'database (make-table))

   )
 ;; events
 (lambda (t x y)
   ;; It would be great to know why this is disabled in LN on android.
   (##thread-heartbeat!)
   (thread-yield!)
   (cond
    ((= t EVENT_KEYPRESS) (if (= x EVENT_KEYESCAPE) (terminate)))
    (else (glgui-event gui t x y))))
 ;; termination
 (lambda () #t)
 ;; suspend
 (lambda () (glgui-suspend) (terminate))
 ;; resume
 (lambda () (glgui-resume))
)

;; eof
