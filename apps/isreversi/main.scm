;; SPDX identifier GPL-3.0-or-later


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

(define download-books-button #f)

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
    (if (file-exists? path)
      (delete-file path))
    (set! vector-copy-size 1024)
    (set! vector-copy-iterations (fxquotient resource-size vector-copy-size))
    (set! scalar-copy-start (- resource-size (fxremainder resource-size vector-copy-size)))
    (set! fh (open-output-file path))
    (set! u8 (make-u8vector vector-copy-size))
    ; Vector loop to allow for fewer stores
    (let loop((iteration-number 0))
        (if (= iteration-number vector-copy-iterations)
            (set! iteration-number vector-copy-iterations) ; exit
          (begin (let vectorloop((vector-iteration 0))
                  (if (= vector-iteration vector-copy-size)
                     (write-subu8vector u8 0 vector-copy-size fh)
                     (begin (u8vector-set! u8
					   vector-iteration
                                           (egaroucid-get-resource-byte resource
                                               (+ vector-iteration (* iteration-number vector-copy-size))))
		            (vectorloop (+ vector-iteration 1)))))
                 (loop (+ iteration-number 1)))))
    ; Scalar remainder loop
    (let loop((iteration-number scalar-copy-start))
        (if (= iteration-number resource-size)
            (set! iteration-number resource-size) ; exit
          (begin (set! byte-data (egaroucid-get-resource-byte resource iteration-number))
                 (write-u8 byte-data fh)
                 (loop (+ iteration-number 1)))))
    (close-output-port fh))

;; Books have not currently been setup for persistent downloads
(define (initialize-egaroucid)
  (glgui-widget-set! gui reversi-subdisplay 'label "Initializing Egaroucid...")
  ;; Create directories if not yet created
  (set! appdir (android-get-filesdir))
  (if (not (file-exists? appdir)) (create-directory appdir))
  (set! resourcedir (string-append appdir (system-pathseparator) "resources"))
  (if (not (file-exists? resourcedir)) (create-directory resourcedir))
  (set! hashdir (string-append resourcedir (system-pathseparator) "hash"))
  (if (not (file-exists? hashdir)) (create-directory hashdir))
  (set! booksize (egaroucid-get-resource-size "book"))
  (glgui-widget-set! gui reversi-subdisplay 'label "Reading in book")
  (save-resource (string-append resourcedir (system-pathseparator) "book.egbk3") "book" booksize)
  (set! evalsize (egaroucid-get-resource-size "eval"))
  (glgui-widget-set! gui reversi-subdisplay 'label "Reading in eval")
  (save-resource (string-append resourcedir (system-pathseparator) "eval.egev2") "eval" evalsize)
  (set! eval_move_ordering_endsize (egaroucid-get-resource-size "eval_move_ordering_end"))
  (glgui-widget-set! gui reversi-subdisplay 'label "Reading in eval_move_ordering_end")
  (save-resource (string-append resourcedir (system-pathseparator) "eval.egev2") "eval_move_ordering_end" evalsize)
  (set! hash25size (egaroucid-get-resource-size "hash25"))
  (glgui-widget-set! gui reversi-subdisplay 'label "Reading in hash25")
  (save-resource (string-append hashdir (system-pathseparator) "hash25.eghs") "hash25" hash25size)
  (set! hash26size (egaroucid-get-resource-size "hash26"))
  (glgui-widget-set! gui reversi-subdisplay 'label "Reading in hash26")
  (save-resource (string-append hashdir (system-pathseparator) "hash26.eghs") "hash26" hash26size)
  (set! hash27size (egaroucid-get-resource-size "hash27"))
  (glgui-widget-set! gui reversi-subdisplay 'label "Reading in hash27")
  (save-resource (string-append hashdir (system-pathseparator) "hash27.eghs") "hash27" hash27size)
  ;(set! egaroucid-init-status (egaroucid-init resourcedir))
  (glgui-widget-set! gui reversi-subdisplay 'label "Readin books"))


(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let* ((w (glgui-width-get))
           (h (glgui-height-get)))
      (set! reversi-subdisplay (glgui-label gui 0 (- h 75) w  30
					    "Press a square to play" ascii_14.fnt Orange))
      (glgui-widget-set! gui reversi-subdisplay 'align GUI_ALIGNLEFT)
      (set! reversi-display (glgui-label gui 0 (- h 45) w 40 "isREVERsi" ascii_18.fnt White))
      (glgui-widget-set! gui reversi-display 'align GUI_ALIGNCENTER)
      (set! egaroucid-version-display (glgui-label gui 0 5 w 40
						   (string-append "Egaroucid engine " (egaroucid-version))
						   ascii_14.fnt DarkGreen))
      (glgui-widget-set! gui egaroucid-version-display 'align GUI_ALIGNCENTER)
      ;; Download books
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
