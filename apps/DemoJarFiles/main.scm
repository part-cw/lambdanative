;; Primitive jar file demo for android



(c-declare  #<<end-of-c-declare


#ifdef ANDROID
  int android_process_mypid(void);
#endif

static int android_mypid(void){

#ifdef ANDROID
      return android_process_mypid();
#else
      return 0;
#endif
}


end-of-c-declare
)

(define android-mypid (c-lambda () int "android_mypid"))

(define gui #f)

(main
 ;; initialization
 (lambda (w0 h0)
   (make-window 320 480)
   (glgui-orientation-set! GUI_PORTRAIT)
   (set! gui (make-glgui))
   (let* ((w (glgui-width-get))
          (h (glgui-height-get))
          (h2 (/ h 2.))
          (w2 (/ w 2.))
          (bw 150) (bh 50)
          (bx (/ (- w bw) 2.))
          (by (/ (- (/ h2 2.) bh) 2.)))
     (let ((wgt (glgui-label gui 0  (+ (/ h 2.) 50 30) w 16 (string-append "My Process Id = " (object->string (android-mypid))) ascii_12.fnt White)))
       (glgui-widget-set! gui wgt 'align GUI_ALIGNCENTER))
     )
   )
 ;; events
 (lambda (t x y)
   (if (= t EVENT_KEYPRESS) (begin
                              (if (= x EVENT_KEYESCAPE) (terminate))))
   (glgui-event gui t x y))

 ;; termination
 (lambda () #t)
 ;; suspend
 (lambda () (glgui-suspend))
 ;; resume
 (lambda () (glgui-resume))
 )

;; eof
