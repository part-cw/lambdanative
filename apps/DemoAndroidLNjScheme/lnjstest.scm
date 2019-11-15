(define intValue (method "intValue" "java.lang.Double"))  ;; FIXME teach jscheme fixnums!

;; Try this to find out how where methods are defined:
;;
;; (method "checkOrRequestPermission" (android-app-class) "java.lang.String")

;; Just to see an error:
;;
;; (error "nananana")

(let* ((app (android-app-class))
       (this ((method "me" app)))
       (ln-mglview (method "LNmGLView" app)) ;; deprecated
       (trigger-redraw! (let ((run (method "LNtriggerRedraw" app)))
                          (lambda () (run this))))
       )
  (let (
        (main-layout (new "android.widget.LinearLayout" this))
        (tv (new "android.widget.TextView" this))
        (button (new "android.widget.Button" this))
        ;;
        (getApplicationContext (method "getApplicationContext" app))
        (getWindow (method "getWindow" app))
        (getParent (method "getParent" "android.view.View"))
        (removeView! (method "removeView" "android.view.ViewGroup" "android.view.View"))
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
        )
    (define (set-layout-vertical! x)
      (setOrientation x (intValue 1)))
    (define (arrange-in-order! parent childs)
      (for-each (lambda (v) (addView! parent v)) childs))
    (set-layout-vertical! main-layout)
    (setText button (new "java.lang.String" "Back"))
    (setText tv (new "java.lang.String" "Hallo kleine Welt!"))
    (let* ((ln-glview (ln-mglview this)))
      (define (switch-back-to-ln! v)
        (removeView! (getParent ln-glview) ln-glview)
        (setContentView this ln-glview)
        (trigger-redraw!))
      (onclick-set! this button switch-back-to-ln!)
      (removeView! (getParent ln-glview) ln-glview)
      (let ((frame (new "android.widget.LinearLayout" this))
            (wv (new "android.webkit.WebView" (getApplicationContext this)))
            (frame2 (new "android.widget.LinearLayout" this)))
        (set-layout-vertical! frame)
        (addView/params! frame2 ln-glview (new "android.view.ViewGroup$LayoutParams" (intValue -1) (intValue 280)))
        (arrange-in-order! main-layout (list frame))
        (arrange-in-order! frame (list frame2 tv button wv))
        (if (checkOrRequestPermission this (new "java.lang.String" "android.permission.INTERNET"))
            (begin
              (setText tv (new "java.lang.String" "Hallo World!"))
              ((method "loadUrl" "android.webkit.WebView" "java.lang.String") wv (new "java.lang.String" "http://www.lambdanative.org")))
            (setText tv (new "java.lang.String" "I'm sorry, there are no permissions to dispaly the URL.")))
        )
      (let ((wrap_content (intValue -2)) ;; #xfffffffe
            (fill_parent (intValue -1)) ;; #xffffffff
            )
        (addContentView
         this main-layout
         (new "android.view.ViewGroup$LayoutParams" fill_parent wrap_content)))
      ;; Finally trigger redraw.
      (trigger-redraw!)
      ln-glview)))
