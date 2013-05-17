#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2013, University of British Columbia
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the
following conditions are met:

* Redistributions of source code must retain the above
copyright notice, this list of conditions and the following
disclaimer.

* Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following
disclaimer in the documentation and/or other materials
provided with the distribution.

* Neither the name of the University of British Columbia nor
the names of its contributors may be used to endorse or
promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|#
;; uSquish
;; simple game to measure reaction time in kids

(define animal-images (list
alligator-icon.img ant-icon.img bat-icon.img bear-icon.img
bee-icon.img bird-icon.img bull-icon.img bulldog-icon.img
butterfly-icon.img cat-icon.img chicken-icon.img cow-icon.img
crab-icon.img crocodile-icon.img deer-icon.img dog-icon.img
donkey-icon.img duck-icon.img eagle-icon.img elephant-icon.img
fish-icon.img fox-icon.img frog-icon.img giraffe-icon.img
gorilla-icon.img hippo-icon.img horse-icon.img insect-icon.img
lion-icon.img monkey-icon.img moose-icon.img mouse-icon.img
owl-icon.img panda-icon.img penguin-icon.img pig-icon.img
rabbit-icon.img rhino-icon.img rooster-icon.img shark-icon.img
sheep-icon.img snake-icon.img tiger-icon.img turkey-icon.img
turtle-icon.img wolf-icon.img
))

(define gui #f)

(define sprites #f)
(define stars #f)
(define bar #f)
(define trophy #f)
(define splash #f)
(define title #f)
(define label #f)
(define arrow #f)
(define barlen 0)
(define stats #f)

(define hits 0)
(define now #f)
(define prv #f)
(define start #f)
(define level 0)
(define lastsquish #f)

(define sound-pop #f)
(define sound-win #f)

(define score '())

(define (stars-update)
  (let loop ((n 0)(s stars))
    (if (and (> (length s) 0)) (begin
       (glgui-widget-set! gui (car s) 'image (if (> (/ hits 7.) n) star.img stargrey.img))
         (loop (fx+ n 1) (cdr s))))))

(define (bar-update)
  (let ((elapsed (min 30. (- now start))))
    (glgui-widget-set! gui bar 'w (* barlen (/ elapsed 30.)))))

(define gamestate #f)
(define STATE_ALIVE 0)
(define STATE_COMATOSE 1)
(define STATE_DEAD 2)
(define STATE_PREGAME 16)
(define STATE_GAME 17)
(define STATE_POSTGAME 18)

(define (animal-animate gui wgt)
  (let ((state (glgui-widget-get gui wgt 'state)))
    (cond 
      ((fx= state STATE_ALIVE)
         (glgui-widget-set! gui wgt 'angle (fl* 10. (flcos (fl* 5. now))))
      )
      ((fx= state STATE_COMATOSE)
         (let ((a (glgui-widget-get gui wgt 'angle)))
           (glgui-widget-set! gui wgt 'angle (fl+ a (fl* 10. (fl- now (floor now))))))
         (let ((fadefactor (fl* (fl- now (glgui-widget-get gui wgt 'state_time)) 4.)))
           (if (fl> fadefactor 1.) (begin
              (glgui-widget-set! gui wgt 'state STATE_DEAD)
              (glgui-widget-set! gui wgt 'hidden #t)
            ) (glgui-widget-set! gui wgt 'color (color-fade White (fl- 1. fadefactor))))
         ))
   ) 
 )) 

(define (animal-press gui wgt)
  (let ((state (glgui-widget-get gui wgt 'state)))
    (if (fx= state STATE_ALIVE) (set! arm wgt))))

(define (animal-release gui wgt)
  (let ((state (glgui-widget-get gui wgt 'state))
        (squish (time->seconds (current-time))))
    (if (and (fx= state STATE_ALIVE) (equal? wgt arm)) (begin
      (glgui-widget-set! gui wgt 'state STATE_COMATOSE) 
      (glgui-widget-set! gui wgt 'state_time now)
      (glgui-widget-set! gui wgt 'angle (fl* (random-real) 180.))
      (glgui-widget-set! gui wgt 'image cloud.img)
      (if (fx= gamestate STATE_GAME) 
        (let* ((reaction_time (- squish lastsquish)))
          (set! hits (fx+ hits 1))
          (set! score (append score (list reaction_time)))
          (stars-update)
          (animal-create)
        ) (game-changestate STATE_GAME))
    ))
    (set! lastsquish squish)
  ))     

(define (animal-create . pos)
  (let ((x (if (fx> (length pos) 0) (car pos) 
          (min (- (glgui-width-get) 64 10) (max 10 (* (random-real) (glgui-width-get))))))
        (y (if (fx> (length pos) 0) (cadr pos)
          (min (- (glgui-height-get) 64 32 16) (max (+ 64 32 16) (* (random-real) (glgui-height-get))))))
        (image (list-ref animal-images (random-integer (length animal-images)))))
    (let loop ((s sprites))
      (if (fx> (length s) 0)
         (let* ((wgt (car s))
                (state (glgui-widget-get gui wgt 'state)))
           (if (fx= state STATE_DEAD) (begin
             (glgui-widget-set! gui wgt 'state STATE_ALIVE)
             (glgui-widget-set! gui wgt 'image image)
             (glgui-widget-set! gui wgt 'color White)
             (glgui-widget-set! gui wgt 'x x)
             (glgui-widget-set! gui wgt 'y y)
             (glgui-widget-set! gui wgt 'hidden #f)
           ) (loop (cdr s)))))))
    (audiofile-play sound-pop))

(define (animal-clear)
  (let loop ((s sprites))
    (if (fx> (length s) 0) (begin
      (glgui-widget-set! gui (car s) 'hidden #t)
      (glgui-widget-set! gui (car s) 'state STATE_DEAD)
      (loop (cdr s)))))) 

(define (game-changestate newstate)
  (cond
    ((fx= newstate STATE_PREGAME)
       (animal-clear)
       (if (> (length score) 0) (glgui-widget-set! gui arrow 'hidden #f))
       (set! hits 0)
       (set! level 0)
       (animal-create (- (/ (glgui-width-get) 2.) 32.) (- (/ (glgui-height-get) 2.) 32.))
       (glgui-widget-set! gui splash 'hidden #f)
       (glgui-widget-set! gui trophy 'hidden #t)
       (glgui-widget-set! gui title 'hidden #f)
       (let loop ((s stats))
         (if (> (length s) 0) (begin
           (glgui-widget-set! gui (car s) 'hidden #t)
           (loop (cdr s)))))
       (set! now (time->seconds (current-time)))
       (set! start now)
    ) 
    ((fx= newstate STATE_GAME)
       (set! score '())
       (glgui-widget-set! gui title 'hidden #t)
       (glgui-widget-set! gui splash 'hidden #t)
       (glgui-widget-set! gui trophy 'hidden #t)
       (glgui-widget-set! gui label 'hidden #t)
       (glgui-widget-set! gui arrow 'hidden #t)
       (set! start (time->seconds (current-time)))
       (stars-update)
       (animal-create)
    )
    ((fx= newstate STATE_POSTGAME)
       (glgui-widget-set! gui arrow 'hidden #t)
       (glgui-widget-set! gui title 'hidden #t)
       (glgui-widget-set! gui splash 'hidden #t)
       (glgui-widget-set! gui trophy 'hidden #f)
       (glgui-widget-set! gui label 'hidden #f)
       (let loop ((s stats))
         (if (> (length s) 0) (begin
           (glgui-widget-set! gui (car s) 'hidden #f)
           (loop (cdr s)))))
       (animal-clear)
       (set! start (time->seconds (current-time)))
    )
  )
  (set! gamestate newstate))

(define (game-over)
  (if (> (length score) 0)
    (let* ((stat_population (length score))
           (stat_median (median score))
           (stat_mean   (mean score))
           (stat_variance (variance score))
           (id (seconds->string now "%Y%m%d%H%M%S"))
           (sysdir (system-directory))
           (file (string-append sysdir "/" id ".csv")))
       (audiofile-play sound-win)
       (glgui-widget-set! gui label 'label (float->zeropaddedstring stat_median 3)) ;; ccstring
       (glgui-widget-set! gui (list-ref stats 0) 'label (string-append "ID:     " id))
       (glgui-widget-set! gui (list-ref stats 1) 'label
         (string-append "VARIANCE:       " (float->zeropaddedstring stat_variance 5)))
       (glgui-widget-set! gui (list-ref stats 2) 'label
         (string-append "MEAN:           " (float->zeropaddedstring stat_mean 5)))
       (glgui-widget-set! gui (list-ref stats 3) 'label
         (string-append "MEDIAN:         " (float->zeropaddedstring stat_median 5)))
       (glgui-widget-set! gui (list-ref stats 4) 'label
         (string-append "POPULATION:       " (if (< stat_population 10) " " "")  
            (number->string stat_population)))
       (glgui-widget-set! gui (list-ref stats 5) 'label "..:: REACTION STATS ::..")
       (if (not (file-exists? sysdir)) (create-directory sysdir))
       (csv-write file (map (lambda (x) (list x)) score))
    )))

;; --------------
;; application loop

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (glgui-pixmap gui 0 0 landscape.img (+ (glgui-width-get) 1) (+ (glgui-height-get) 1))
    (let* ((r 0.0875) (pymin 8) (pymax 25) 
          (pxmin (* r (glgui-width-get))) 
          (pxmax (* (- 1. r) (glgui-width-get))))
      (set! bar (glgui-box gui pxmin pymin (- pxmax pxmin) (- pymax pymin) Red))
      (set! barlen (- pxmax pxmin))
    )
    (glgui-image gui 0 0 (glgui-width-get) 32 progressbar.img Orange)
    (let* ((dx (/ (glgui-width-get) 11.))
          (y0  (- (glgui-height-get) 32))
          (x0 (/ dx 2.)))
      (set! stars
        (let loop ((n 0)(x x0)(res '()))
          (if (fx= n 10) res 
            (loop (fx+ n 1) (+ x dx) (append res (list
              (glgui-sprite gui 'x x 'y y0 'image stargrey.img))))))))                
    (set! sprites 
      (let loop ((n 0)(res '()))
        (if (fx= n 8) res 
          (loop (fx+ n 1) (append res (list
            (glgui-sprite gui 'rendercallback animal-animate 
              'presscallback animal-press 'releasecallback animal-release
           )))))))
    (set! trophy (glgui-image gui 0 (* (glgui-height-get) 0.25) (glgui-width-get) (* (glgui-height-get) 0.75) trophy.img White))
    (set! splash (glgui-image gui 0 0 (glgui-width-get) (/ (glgui-height-get) 2.) splash.img Black))
    (set! title (glgui-image gui 0 (/ (glgui-height-get) 2.) (glgui-width-get) (/ (glgui-height-get) 2.5) title.img Orange))
    (set! label (glgui-label gui 0 (- (glgui-height-get) 75) (glgui-width-get) 32 "" num_50.fnt Orange))
    (glgui-widget-set! gui label 'hidden #t)
    (glgui-widget-set! gui label 'align GUI_ALIGNCENTER)
    (set! arrow (glgui-image gui 20 (- (glgui-height-get) 75) (- (glgui-width-get) 40) 32 arrow.img Orange))
    (glgui-widget-set! gui arrow 'align GUI_ALIGNRIGHT)
    (glgui-widget-set! gui arrow 'hidden #t)
    (glgui-widget-set! gui arrow 'callback (lambda (g . arbage)
       (game-changestate STATE_POSTGAME)))
    (set! stats 
      (let loop ((y 60)(n 0)(res '()))
        (if (= n 6) res
          (let ((wgt (glgui-label gui 10 y (- (glgui-width-get) 20) 24 "" ascii_15.fnt Black)))
            (glgui-widget-set! gui wgt 'align GUI_ALIGNCENTER)
            (loop (+ y 24)(+ n 1)(append res (list wgt)))))))
    (audiofile-init)
    (random-source-randomize! default-random-source)
    (set! sound-win (audiofile-load "win"))
    (set! sound-pop (audiofile-load "pop"))
    (game-changestate STATE_PREGAME)
    (glgui-propagate-set! #t)
  )
;; events
  (lambda (t x y) 
    (if (and (fx= t EVENT_KEYPRESS) (fx= x EVENT_KEYESCAPE)) (terminate))
    (set! prv now) 
    (set! now (time->seconds (current-time)))
    (if (fx= gamestate STATE_GAME) (cond
        ((and (fl> (fl- now start) 10.) (fx= level 0))
           (set! level 1) (animal-create))
        ((and (fl> (fl- now start) 20.) (fx= level 1))
           (set! level 2) (animal-create))
        ((and (fl> (fl- now start) 25.) (fx= level 2))
           (set! level 3) (animal-create))
      ))
    (if (fx= gamestate STATE_GAME) (bar-update))
    (cond
      ((and (fx= gamestate STATE_GAME) (fl> (fl- now start) 30.))
         (game-over)
         (game-changestate (if (> (length score) 0) STATE_POSTGAME STATE_PREGAME)))
      ((and (fx= gamestate STATE_POSTGAME) (fl> (fl- now start) 10.)) 
         (game-changestate STATE_PREGAME))
    )
    (glgui-event gui t x y))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof 
