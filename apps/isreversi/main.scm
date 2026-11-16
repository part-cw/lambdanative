;; SPDX identifier GPL-3.0-or-later

(c-declare "#include \"data.c\"")

(define get-resource-size (c-lambda (char-string)  int "get_resource_size"))
(define get-resource-bytes (c-lambda (char-string int int scheme-object) int
    "___result=get_resource_bytes(___arg1,___arg2,___arg3,___CAST(uint8_t*,___BODY_AS(___arg4,___tSUBTYPED)));"))

;; -1 empty
;;  1 white
;;  0 black
(define board-state
  `(-1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1  1  0 -1 -1 -1
    -1 -1 -1  0  1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1
    -1 -1 -1 -1 -1 -1 -1 -1))

(define buttons
  `(#f #f #f #f #f #f #f #f
    #f #f #f #f #f #f #f #f
    #f #f #f #f #f #f #f #f
    #f #f #f #f #f #f #f #f
    #f #f #f #f #f #f #f #f
    #f #f #f #f #f #f #f #f
    #f #f #f #f #f #f #f #f
    #f #f #f #f #f #f #f #f))

(define books-setup #f)
(define egaroucid-initialize #f)
(define gui #f)
(define reversi-display #f)
(define reversi-subdisplay #f)
(define egaroucid-version-display #f)
(define playing #f)
(define square-width 40)
(define square-border 2)
(define inner-square-width (- square-width (* square-border 2)))

(define (xind button-number)
  (fxremainder button-number 8))

(define (yind button-number)
  (fxquotient button-number 8))

(define (set-colors g b)
  (glgui-widget-set! g b 'button-normal-color DarkGreen)
  (glgui-widget-set! g b 'solid-color #t))

(define (create-button button-number)
  (cond
    ((= (list-ref board-state button-number) -1)
         (list-set! buttons button-number
		    (glgui-button gui (+ (* (xind button-number) square-width)
                                         2)
                                      (- (glgui-height-get)
                                         (+ 120 (* square-width (yind button-number))))
                                      inner-square-width
                                      inner-square-width
                                      empty.img
                                      button-callback)))
    ((= (list-ref board-state button-number) 0)
         (list-set! buttons button-number
		    (glgui-button gui (+ (* (xind button-number) square-width)
                                         2)
                                      (- (glgui-height-get)
                                         (+ 120 (* square-width (yind button-number))))
                                      inner-square-width
                                      inner-square-width
                                      black.img
                                      button-callback)))
    (else
         (list-set! buttons button-number
		    (glgui-button gui (+ (* (xind button-number) square-width)
                                         2)
                                      (- (glgui-height-get)
                                         (+ 120 (* square-width (yind button-number))))
                                      inner-square-width
                                      inner-square-width
                                      white.img
                                      button-callback))))
  (set-colors gui (list-ref buttons button-number)))

(define (get-button-x x)
  (+ (fxquotient x square-width) 1))

(define (get-button-y y)
  (- 11 (fxquotient y square-width) 1))

(define (get-button-id x y)
 (string-append (string (integer->char (+ (get-button-x x) 96)))
		(number->string (get-button-y y))))

(define (button-callback gui b t x y)
  (glgui-widget-set! gui reversi-subdisplay 'label (get-button-id x y))
  (set! b (glgui-button gui (+ (* (- (get-button-x x) 1) square-width) 2)
			    (- (glgui-height-get) (+ 80 (* square-width (get-button-y y))))
			    inner-square-width
			    inner-square-width
			    white.img
			    button-callback))
  )


(define (save-resource path resource resource-size)
    (if (file-exists? path) (delete-file path))
    (set! vector-copy-size 4096)
    (set! vector-copy-iterations (fxquotient resource-size vector-copy-size))
    (set! remainder-size (fxremainder resource-size vector-copy-size))
    (set! remainder-copy-start (- resource-size remainder-size))
    (set! fh (open-output-file path))
    (set! u8 (make-u8vector vector-copy-size 0))
    ; Full vector loop to allow for fewer stores
    (let loop((iteration-number 0))
       (if (= iteration-number vector-copy-iterations)
         (set! iteration-number vector-copy-iterations) ; exit
          (begin
           (set! copy-check (get-resource-bytes resource (* iteration-number vector-copy-size) vector-copy-size u8))
           (write-subu8vector u8 0 vector-copy-size fh)
           (loop (+ iteration-number 1)))))
    ; Remainder
    (set! copy-check (get-resource-bytes resource remainder-copy-start remainder-size u8))
    (write-subu8vector u8 0 remainder-size fh)
    (close-output-port fh))

(define (initialize-egaroucid)
  (glgui-widget-set! gui reversi-subdisplay 'label "Setting up books...")
  (set! appdir (android-get-filesdir))
  (if (not (file-exists? appdir)) (create-directory appdir))
  (set! resourcedir (string-append appdir
                                   (system-pathseparator)
                                   "resources"
                                   (system-pathseparator)))
  (if (not (file-exists? resourcedir)) (create-directory resourcedir))
  (set! hashdir (string-append resourcedir
                               "hash"
                               (system-pathseparator)))
  (if (not (file-exists? hashdir)) (create-directory hashdir))
  (glgui-widget-set! gui reversi-subdisplay 'label "Reading in books...")
  (set! booksize (get-resource-size "book"))
  (glgui-widget-set! gui reversi-subdisplay 'label (string-append "Reading in book, size:" (number->string booksize) " bytes"))
  (save-resource (string-append resourcedir "book.egbk3") "book" booksize)
  (set! evalsize (get-resource-size "eval"))
  (glgui-widget-set! gui reversi-subdisplay 'label (string-append "Reading in eval, size:" (number->string evalsize) " bytes"))
  (save-resource (string-append resourcedir "eval.egev2") "eval" evalsize)
  (set! evalendsize (get-resource-size "evalend"))
  (glgui-widget-set! gui reversi-subdisplay 'label (string-append "Reading in evalend, size:"
                                                                  (number->string evalendsize) " bytes"))
  (save-resource (string-append resourcedir
                                (system-pathseparator)
                                "eval_move_ordering_end.egev")
                  "evalend"
                  evalendsize)
  (set! hash25size (get-resource-size "hash25"))
  (glgui-widget-set! gui reversi-subdisplay 'label (string-append "Reading in hash25, size:" (number->string hash25size) " bytes"))
  (save-resource (string-append hashdir (system-pathseparator) "hash25.eghs") "hash25" hash25size)
  (set! hash26size (get-resource-size "hash26"))
  (glgui-widget-set! gui reversi-subdisplay 'label (string-append "Reading in hash26, size:" (number->string hash26size) " bytes"))
  (save-resource (string-append hashdir (system-pathseparator) "hash26.eghs") "hash26" hash26size)
  (set! hash27size (get-resource-size "hash27"))
  (glgui-widget-set! gui reversi-subdisplay 'label (string-append "Reading in hash27, size:" (number->string hash27size) " bytes"))
  (save-resource (string-append hashdir (system-pathseparator) "hash27.eghs") "hash27" hash27size)
  (glgui-widget-set! gui reversi-subdisplay 'label "Books read in, initializing Egaroucid"))
;  (set! initialized (egaroucid-global-init resourcedir))
;  (cond
;    ((= initialized  EGAROUCID_OK)
;     (glgui-widget-set! gui reversi-subdisplay 'label "Egaroucid initialized, press a square to play")
;     (set! initialize-egaroucid-button #f))
;    (else
;      (glgui-widget-set! gui reversi-subdisplay 'label "Egaroucid initialization failed"))))


(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let* ((w (glgui-width-get))
           (h (glgui-height-get)))
      (set! reversi-subdisplay (glgui-label gui 0 (- h 75) w  30
					    "" ascii_14.fnt Orange))
      (glgui-widget-set! gui reversi-subdisplay 'align GUI_ALIGNLEFT)
      (set! reversi-display (glgui-label gui 0 (- h 45) w 40 "ISReversi" ascii_18.fnt White))
      (glgui-widget-set! gui reversi-display 'align GUI_ALIGNCENTER)
      (set! egaroucid-version-display (glgui-label gui 0 5 w 40
						   (string-append "Egaroucid engine " (egaroucid-version))
						   ascii_14.fnt DarkGreen))
      (glgui-widget-set! gui egaroucid-version-display 'align GUI_ALIGNCENTER)
      (set! initialize-egaroucid-button (glgui-button-string gui 90 50 140 15 "Initialize Egaroucid" ascii_14.fnt
           (lambda (un . used) (initialize-egaroucid))))
      (glgui-widget-set! gui initialize-egaroucid-button 'align GUI_ALIGNCENTER)
      (glgui-widget-set! gui initialize-egaroucid-button 'solid-color #t)
      ;; Create board
      (let loop((button-number 64))
	(if (= button-number 0)
	  (set! button-number 0) ;; exit
	  (begin (create-button (- button-number 1))
		 (loop (- button-number 1)))))

    ))
 
;; events
  (lambda (t x y)
    (if (= t EVENT_KEYRELEASE)
      (cond
        ((= x EVENT_KEYESCAPE)     (terminate))
     ))
    (glgui-event gui t x y))

;; termination
  (lambda ()
    (egaroucid-destroy engine)
    #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
