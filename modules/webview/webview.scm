(define (run-in-LNjScheme #!key (success values) (fail raise) #!rest body)
  (thread-start!
   (make-thread
    (lambda ()
      (for-each
       (lambda (expr)
         (cond-expand
          (android
           (log-debug "jScheme EVAL:" 1 expr)
           (call-with-lnjscheme-result
            expr
            (lambda (promise)
              (with-exception-catcher
               (lambda (exn)
                 (log-error
                  (call-with-output-string
                   (lambda (port)
                     (display "EXN in\n" port)
                     (pretty-print expr port)
                     (display "EXN: " port)
                     (display-exception exn port)))))
               (cond
                ((procedure? promise) (lambda () (success (promise))))
                (else (lambda () (success (force promise)))))))))
          (else #f)))
       body))
    'jscheme-worker))
  #t)

(define (android-webview-code)
  `(let* ((app ,(android-app-class))
          (this ln-this)
          (intValue (method "intValue" "java.lang.Double"))  ;; FIXME teach jscheme fixnums!
          (String (lambda (x) (new "java.lang.String" x)))
          )
     (let (
           (getWindow (method "getWindow" app))
           (getParent (method "getParent" "android.view.View")) ;; TBD: get rid of this
           (setText (method "setText" "android.widget.TextView" "java.lang.CharSequence"))
           (addView! (method "addView" "android.view.ViewGroup" "android.view.View"))
           (addView/params! (method "addView" "android.view.ViewGroup" "android.view.View"
                                    "android.view.ViewGroup$LayoutParams"))
           (setContentView (method "setContentView" app "android.view.View"))
           (addContentView (method "addContentView" app "android.view.View" "android.view.ViewGroup$LayoutParams"))
           (setOrientation (method "setOrientation" "android.widget.LinearLayout" "int"))
           ;;
           (onclick-set! (method "LNjScheme_Set_OnClickListener" app "android.view.View" "java.lang.Object"))
           (checkOrRequestPermission (method "checkOrRequestPermission" app "java.lang.String"))
           (loadUrl (method "loadUrl" "android.webkit.WebView" "java.lang.String"))
           (getUrl (method "getUrl" "android.webkit.WebView"))
           (wv-can-go-back? (method "canGoBack" "android.webkit.WebView"))
           (wv-goBack! (method "goBack" "android.webkit.WebView"))
           (wv-setClient! (method "setWebViewClient" "android.webkit.WebView" "android.webkit.WebViewClient"))
           ;;
           (websettings (method "getSettings" "android.webkit.WebView"))
           (wvs-javascript-enabled-set! (method "setJavaScriptEnabled" "android.webkit.WebSettings" "boolean"))
           (wvs-zoom-support-set! (method "setSupportZoom" "android.webkit.WebSettings" "boolean"))
           (wvs-zoom-builtin-set! (method "setBuiltInZoomControls" "android.webkit.WebSettings" "boolean"))
           (wvs-zoom-builtin-controls-set! (method "setDisplayZoomControls" "android.webkit.WebSettings" "boolean"))
           )
       (define (set-layout-vertical! x)
         (setOrientation x (intValue 1)))
       (define (arrange-in-order! parent childs)
         (for-each (lambda (v) (addView! parent v)) childs))
       (let (
             (frame (new "android.widget.LinearLayout" this))
             (wv (make-webview this))
             (navigation (new "android.widget.LinearLayout" this)) ;; horizontal is default
             (back (new "android.widget.Button" this))
             (back-pressed-h #f)
             (reload (new "android.widget.Button" this))
             (Button3 (new "android.widget.Button" this))
             (Bcopy (new "android.widget.Button" this))
             )
         (define (switch-back-to-glgui! v)
           (on-back-pressed back-pressed-h)
           (set! back-pressed-h #f)
           (setContentView this (lambdanative-glview)))
         (define (back-pressed)
           (if (wv-can-go-back? wv) (wv-goBack! wv) (switch-back-to-glgui! frame)))
         ;; (webview! wv 'onpagecomplete (lambda (view url) (log-message "webview post visual state")))
         ;; (webview! wv 'onLoadResource (lambda (view url) (log-message (string-append "onLoadResource " url))))
         ;; (webview! wv 'onPageFinished (lambda (view url) (log-message (string-append "onPageFinished " url))))
         (let* ((wvs (websettings wv))
                (js+- (let ((is #f))
                        (lambda _
                          (set! is (not is))
                          (wvs-javascript-enabled-set! wvs is)))))
           ;; (wvs-javascript-enabled-set! wvs #t)
           (begin
             (setText Button3 (String "JS+-"))
             (onclick-set! this Button3 js+-))
           (begin
             (setText Bcopy (String "COPY"))
             (onclick-set! this Bcopy (lambda _ (setClipboardContent (getUrl wv)))))
           (wvs-zoom-support-set! wvs #t)
           (wvs-zoom-builtin-set! wvs #t)
           (wvs-zoom-builtin-controls-set! wvs #f))
         (arrange-in-order! navigation (list back reload Button3 Bcopy))
         (setText back (String "Back"))
         (setText reload (String "Reload"))
         (onclick-set! this back switch-back-to-glgui!)
         (onclick-set! this reload (lambda (v) ((method "reload" "android.webkit.WebView") wv)))
         (set-layout-vertical! frame)
         (arrange-in-order! frame (list navigation wv))
         (lambda (cmd arg)
           (case cmd
             ((load) (webview! wv cmd arg))
             ((getURL) (getUrl wv))
             (else
              (if (not back-pressed-h)
                  (begin
                    (set! back-pressed-h (on-back-pressed))
                    (on-back-pressed back-pressed)))
              (setContentView this frame))))))))

(define android-webview
  (let ((in-android-webview
         (lambda (args)
           (define (otherwise)
             (log-error "android-webview:  call not recognized" (object->string args))
             #f)
           (cond
            ((null? args) (otherwise))
            (else
             (let ((a1 (car args)))
               (cond
                ((eq? a1 #t) '(webview #t #t))
                ((string? a1) `(webview 'load ,a1))
                ((eq? a1 'getURL) `(webview 'getURL #t))
                (else (otherwise))))))))
        (webview-running #f))
    (lambda args
      (cond
       ((eq? webview-running #f)
        (set! webview 'initializing)
        (apply
         run-in-LNjScheme
         ;; success: (lambda _ (set! webview #t) (run-in-LNjScheme '(webview 'redraw #t)))
         #;`(define (log-message str)
            (let* ((app ,(android-app-class))
                   ;; DOES NOT work on Android 10!!!
                   (log (method "ln_log" app  "java.lang.String")))
              (log ln-this (new "java.lang.String" str))
              #t))
         ;; '(log-message "log-message working, app class:")
         ;; `(log-message ,(debug 'android-app-class (android-app-class)))
         `(if (not (bound? 'webview)) (define webview ,(android-webview-code)))
         (map in-android-webview args))
        (set! webview-running #f))
       (else
        (log-error
         "android-webview: called again while previous call did not yet return.  IGNORED: "
         (object->string args))))
      #!void)))

(define
  webview-launch!
 (let ((orginal-launch-url launch-url))
   (lambda (url #!key (via #f))
     (cond-expand
      (android
       (case via
         ((webview) (android-webview '("about:blank") `(,url) '(#t)))
         ((extern) (orginal-launch-url url))
         (else (orginal-launch-url url))))
      (else (orginal-launch-url url))))))
