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

(define test-file-name "lnjstest.scm")
(define test-path-name (string-append (system-directory) (system-pathseparator) test-file-name))

(include "lnjscheme.scm")

(define (exception-->printable exc)
  (if (os-exception? exc)
      (list 'OS-EXCEPTION (os-exception-procedure exc)
	    (os-exception-arguments exc)
	    (os-exception-code exc)
	    (err-code->string (os-exception-code exc))
	    (os-exception-message exc))
      exc))

(define (try-LNjScheme)
  (cond-expand
   (android
    (define (evl expr) (force (lnjscheme-future expr))))
   (else (define evl eval)))
  (define exprs '())
  (define (try-expr expr)
    (display "Input:\n")
    (pretty-print expr)
    (newline)
    (with-exception-catcher
     (lambda (exn)
       (display "EXN: ")
       (display (exception-->printable exn))
       (newline))
     (lambda ()
       (let ((result (evl expr)))
         (display "Result: ")
         (write result)
         (newline)))))
  ;; Important: We need to return from the button's action
  ;; immediately, hence running the actual change in background
  ;; thread.
  (thread-start!
   (make-thread
    (lambda ()
      (try-expr `(define (android-app-class) ,(android-app-class)))
      (let ((fn test-path-name))
        (if (file-exists? fn) (set! exprs (call-with-input-file fn read-all)) (set! exprs (list "failed to find" fn)))
        (dbset
         'testresults
         (with-output-to-string (lambda () (for-each try-expr exprs))))))
    'LNjScheme-worker))
  #f)

(define (make-uiforms)
  `(
    (main
    "LNjScheme"
    #f
    #f
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
    ;; EVENT #126: retrieve and dispatch LNjScheme result.
    ;; TBD: move this out of the application into LN core.
    ((eq? t 126) (LNjScheme-result))
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
