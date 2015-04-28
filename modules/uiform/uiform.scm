#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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

;; -------------
;; uiform form-driven universal widget

(define uiform:debuglevel 0)
(define (uiform:log level . x)
    (if (>= uiform:debuglevel level) (apply log-system (append (list "uiform: ") x))))
 
(define uiform:g #f)
(define uiform:wgt #f)

(define (uiget id . optdef) 
  (uiform:log 2 "uiget " id " " optdef)
  (apply glgui-widget-get-def (append (list uiform:g uiform:wgt id) optdef)))

(define (uiset id val) 
  (uiform:log 2 "uiset " id " " val)
  (glgui-widget-set! uiform:g uiform:wgt id val))

(define (uiclear id)
  (uiform:log 2 "uiclear" id)
  (glgui-widget-clear! uiform:g uiform:wgt id))

(define (stget id . optdef)
  (uiform:log 2 "stget " id " " optdef)
  (let ((def (if (fx= (length optdef) 1) (car optdef) #f))
        (db (uiget 'store)))
    (if db (store-ref db id def) def)))
 
(define (stgetnclear id . optdef)
  (uiform:log 2 "stgetnclear " id " " optdef)
  (let ((def (if (fx= (length optdef) 1) (car optdef) #f))
        (db (uiget 'store)))
    (if db (store-ref-clear db id def) def)))
 
(define (stset id val)
  (uiform:log 2 "stset " id " " val)
  (let ((db (uiget 'store)))
    (if db (store-set! db id val))))

(define (stclear id)
  (uiform:log 2 "stclear " id)
  (let ((db (uiget 'store))) 
    (if db (store-clear! db id))))

(define (dbget id . optdef)
  (uiform:log 2 "dbget " id " " optdef)
  (let ((def (if (fx= (length optdef) 1) (car optdef) #f))
        (db (uiget 'database)))
    (if db (table-ref db id def) def)))
 
(define (dbset id val) 
  (uiform:log 2 "dbset " id " " val)
  (let ((db (uiget 'database)))
    (if db (table-set! db id val))))

(define (dbclear id)
  (uiform:log 2 "dbclear " id)
  (let ((db (uiget 'database)))
    (if db (table-set! db id))))

(define (xxget location . args)
  (uiform:log 2 "xxget " location " " args)
  (apply (case location ((db) dbget) ((ui) uiget) ((st) stget)) args))

(define (xxset location . args)
  (uiform:log 2 "xxset " location " " args)
  (apply (case location ((db) dbset) ((ui) uiset) ((st) stset)) args))

(define (xxclear location . args)
  (uiform:log 2 "xxclear " location " " args)
  (apply (case location ((db) dbclear) ((ui) uiclear) ((st) stclear)) args))

(define (dbload file key)
  (uiform:log 2 "dbload " file " " key)
  (let ((filepath (string-append (system-directory) (system-pathseparator) file)))
    (if (file-exists? filepath)
      ;; try encrypted format
      (let ((res1 (cdb->table filepath key)))
        (if (table? res1) res1 
           ;; retry with a plain text list - useful to seed a database
           (let ((res2 (with-exception-catcher (lambda (e) (log-error "dbload failed on " filepath) #f) (lambda ()
                   (list->table (with-input-from-file filepath (lambda () (read))))))))
             (if (table? res2) (table->cdb res2 filepath key)) ;; overwrite immediately with encrypted form
             res2)))      
      (begin (log-error "dbload: file not found: " filepath) (make-table)))))

(define (dbsave file key db)
  (uiform:log 2 "dbsave " file " " key " " db)
  (let ((filepath (string-append (system-directory) (system-pathseparator) file)))
    (table->cdb db filepath key)
  ))

(define (uiform-db-listinsert! id element)
  (let ((oldentry (dbget id '())))
    (if (not (member element oldentry))
      (dbset id (append oldentry (list element))))))

(define (uiform-db-listremove! id element)
  (let ((oldentry (dbget id '())))
    (dbset id (list-delete-item oldentry element))))

(define (sane id saneproc missingmsg invalidmsg)
  (let* ((val (dbget id #f))
         (sane (if val (apply saneproc (list val)) #f)))
    (uiset 'errmsg (if sane "" (if val invalidmsg missingmsg)))
    (and val sane)))

(define (uiform:eval f)
  (if (procedure? f)
    (with-exception-catcher (lambda (e)
      (let ((emsg (exception->string e)))
        (log-error "eval failed : " emsg) #f))
       (lambda () (f))) f))

(define (uiform:evalarg key p . args)
    (with-exception-catcher (lambda (e)
      (let ((emsg (exception->string e)))
        (log-error "evaluation of argument " key " failed : " emsg)
        (case key
          ((action) `(,emsg ("OK" #f)))
          ((text) emsg)
          ((color) Red)
          (else #f))))
     (lambda () (apply p args))))

(define (uiform:evalelement p . args)
    (with-exception-catcher (lambda (e)
      (let ((emsg (exception->string e)))
        (log-error "element failed : " emsg)
        `(label text ,emsg color ,Red)))
     (lambda () (apply p args))))

(define (glgui:uiform-modal-up modal)
  (uiset 'modal-content modal)
  (uiset 'modal-output #f)
  (uiset 'modal-on #t)
  (uiset 'input-handle glgui:uiform-modal-input)
)

(define (glgui:uiform-modal-down)
  (let ((action (uiget 'modal-action)))
    (uiset 'modal-action #f)
    (uiset 'modal-on #f)
    (uiset 'input-handle glgui:uiform-input)
    (if action (glgui:uiform-action action))
  ))

(define (glgui:uiform-keypad-up)
  (let ((keypad-on (uiget 'keypad-on))
        (node-y (uiget 'node-y))
        (node-height (uiget 'node-height))
        (keypad-height (uiget 'keypad-height)))
    (if (not keypad-on) (begin
      (if (> keypad-height (- node-y node-height)) 
        (let ((keypad-shift (- keypad-height (- node-y node-height 10)))
              (ofs (uiget 'offset)))
        (uiset 'keypad-shift keypad-shift)
        (uiset 'offset (+ ofs keypad-shift))
      ))
      (uiset 'keypad-on #t)
    ) 
   ;; keypad is on, so just recalculate shift
    (let* ((oldshift (uiget 'keypad-shift))
          (ofs (uiget 'offset)) 
          (oldofs (- ofs oldshift))
          (newshift (max 0 (- keypad-height (- node-y node-height 10))))) 
      (uiset 'keypad-shift newshift)
      (uiset 'offset (+ ofs (- oldshift) newshift))
    ))))

(define (glgui:uiform-keypad-down)
  (let ((keypad-on (uiget 'keypad-on))
        (keypad-shift (uiget 'keypad-shift))
        (ofs (uiget 'offset)))
    (if keypad-on (begin
      (uiset 'keypad-on #f)
      (uiset 'offset (- ofs keypad-shift))
      (uiset 'keypad-shift 0)
      (uiset 'focusid #f)
    ))))

(define (glgui:uiform-keycb-name floc fid str)
   (if (string=? str "")
     (uiset 'shift #t)
     ;; Otherwise get the last character of the string
     (let ((last (string-ref str (- (string-length str) 1))))
       (if (char=? last #\space)
         (uiset 'shift #t))))
)

(define (glgui:uiform-action action)
  (let ((evalaction (if (procedure? action) (uiform:evalarg 'action action) action)))
    (glgui:uiform-keypad-down)
    (uiset 'nodemap '())
    (uiset 'offset 0)
    (uiset 'nxt-action #f)
    (uiset 'prv-action #f)
    (if evalaction 
      (if (list? evalaction)
        (glgui:uiform-modal-up evalaction)
        (uiset 'page evalaction)
    ))
))

(define (glgui:uiform-arg args key defval)
  (let* ((entry-noeval (member key args))
         (entry (if (and entry-noeval (procedure? (cadr entry-noeval))) 
           (list 'nop (uiform:evalarg key (cadr entry-noeval))) entry-noeval)))
    (if entry (cadr entry) defval)))

;; -------------
;; widget element registration
(define uiform:elements (make-table))

(define (uiform-register name draw input)
  (uiform:log 3 "uiform-register " name " " draw " " input)
  (table-set! uiform:elements name (list draw input))
)

(define (uiform-register-legacy name draw input)
  (uiform:log 3 "uiform-register-legacy " name " " draw " " input)
  (let ((newdraw (lambda (x y w . args)
     (let ((ox (uiget 'x)) (oy (uiget 'y)) (ow (uiget 'w)) (oh (uiget 'h)))
       (glgui-widget-set! uiform:g uiform:wgt 'x x)
       (glgui-widget-set! uiform:g uiform:wgt 'y y)
       (glgui-widget-set! uiform:g uiform:wgt 'w w)
       (let loop ((as args))
         (if (fx> (length as) 0) (begin
           (glgui-widget-set! uiform:g uiform:wgt (car as) (cadr as))
           (loop (cddr as)))
         )
       )
       (draw uiform:g uiform:wgt)
       (let ((ret (uiget 'h)))
         (uiset 'x ox) (uiset 'y oy) (uiset 'w ow) (uiset 'h oh)
         (if (= ret oh) 30 ret)
       )
     )))
     (newinput (lambda (type mx my . args)
       (let loop ((as args))
         (if (fx> (length as) 0) (begin
           (glgui-widget-set! uiform:g uiform:wgt (car as) (cadr as))
           (loop (cddr as)))
         )
       )
       (input uiform:g uiform:wgt type mx my)
     )))
    (table-set! uiform:elements name (list newdraw newinput))
  ))

;; -------------
;; spacer 

(define (glgui:uiform-spacer-draw x y w . args)
  (let ((h (glgui:uiform-arg args 'height 30)))
    h))

(uiform-register 'spacer glgui:uiform-spacer-draw #f)

;; -------------
;; image

(define glgui:uiform-images (make-table))

(define (glgui:uiform-image-draw x y w . args)
  (let* ((imgsrc (glgui:uiform-arg args 'file #f))
         (img (if (string? imgsrc)
           (let ((lut (table-ref glgui:uiform-images imgsrc #f)))
             (if (not lut) (let* ((sandbox (uiget 'sandbox #f))
                                  (imgfile (string-append sandbox (system-pathseparator) imgsrc))
                                  (tmpimg (if (file-exists? imgfile) (png->img imgfile) #f)))
               (if tmpimg (table-set! glgui:uiform-images imgsrc tmpimg) (log-error "image file " imgfile " not found"))
               tmpimg) lut)) imgsrc))
         (h (if img (cadr img) 10))
         (align (glgui:uiform-arg args 'align 'center))
         (drawproc (case align
                       ((center) glgui:draw-pixmap-center)
                       ((left) glgui:draw-pixmap-left)
                       ((right) glgui:draw-pixmap-right))))
    (if (and img (uiget 'sanemap)) (drawproc x y w h img White))
    h))

(uiform-register 'image glgui:uiform-image-draw #f)

;; -------------
;; label

(define (glgui:uiform-label-draw x y w . args)
  (let* ((fntsize (glgui:uiform-arg args 'size 'normal))
         (fnt (uiget (case fntsize 
                       ((normal) 'fnt) 
                       ((small) 'smlfnt) 
                       ((big) 'bigfnt)
                       ((header) 'hdfnt))))
         (h (glgui:fontheight fnt))
         (label (glgui:uiform-arg args 'text ""))
         (wrappedlabel (if (glgui:uiform-arg args 'wrap #t)
           (string-split-width label (fix (* 0.8 w)) fnt)
           (list label)))
         (color (glgui:uiform-arg args 'color White))
         (align (glgui:uiform-arg args 'align 'center))
         (ypos y)
         (toth 0))
     (let loop ((ss (reverse wrappedlabel)))
       (if (> (length ss) 0) (begin
         (if (uiget 'sanemap)
         (case align
           ((center) (glgui:draw-text-center (+ x (* 0.1 w)) ypos (- w (* 0.2 w)) h (car ss) fnt color) )
           ((left) (glgui:draw-text-left (+ x (* 0.1 w)) ypos (- w (* 0.1 w)) h (car ss) fnt color) )
           ((right) (glgui:draw-text-right x ypos (- w (* 0.1 w)) h (car ss) fnt color))
         ))
         (set! toth (+ toth h))
         (set! ypos (+ ypos h))
         (loop (cdr ss)))))
     toth
  ))

(uiform-register 'label glgui:uiform-label-draw #f)

;; -------------
;; text entry

(define (glgui:uiform-textentry-draw x y w . args)
  (let* ((h 48) 
         (fnt (uiget 'fnt))
         (label (glgui:uiform-arg args 'text ""))
         (id (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (password  (glgui:uiform-arg args 'password #f))
         (default (glgui:uiform-arg args 'default #f))
         (focusid  (uiget 'focusid))
         (hasfocus (eq? focusid id))
         (idvalue (if id (xxget loc id #f) #f))
         (defcolor (uiget 'color-default))
         (selcolor (uiget 'color-select))
         (fgcolor White)
         (idvaluestr (cond
           ((and (string? idvalue) (or hasfocus (fx> (string-length idvalue) 0)))
              (if password (make-string (string-length idvalue) #\*) idvalue))
           ((number? idvalue)
              (number->string idvalue))
           (else
             (set! fgcolor (uiget 'color-default))
             default)))
         (indent (glgui:uiform-arg args 'indent
            (if (string=? label "") 0.1 0.3)))
         (indentright (glgui:uiform-arg args 'indentright 0.1))
         (align (glgui:uiform-arg args 'align 'left))
         (drawproc (case align
                      ((left) glgui:draw-text-left)
                      ((center) glgui:draw-text-center)
                      ((right) glgui:draw-text-right)))
         (txtw  (if (and focusid idvalue idvaluestr) (glgui:stringwidth idvaluestr fnt) 0))
         (txth  (if focusid (glgui:fontheight fnt) 0)))
     (if (uiget 'sanemap) (begin
       (glgui:draw-text-right x y (- (* w indent) 10) h label fnt White)
       (glgui:draw-box (+ x (* w indent)) y (* w (- 1. indent indentright)) h (if hasfocus selcolor defcolor))
       (if idvaluestr (drawproc (+ x (* w indent) (if (eq? align 'left) 10 0)) y (- (* w (- 1. indent indentright)) 10) h idvaluestr fnt fgcolor))
       (if hasfocus
          (let* ((cx (case align
                       ((left) (+ x (* w indent) 10 txtw 2))
                       ((center) (+ x (* w indent) (/ (+ (- (* w (- 1. indent indentright)) 10) txtw) 2.) 2))
                       ((right) (- (+ x (* w (- 1. indentright))) 7))))
                 (cy (+ y (/ (- h txth) 2.)))
                 (cw 3)
                 (ch txth)
                 (cc (if (odd? (fix (* 2 ##now))) White selcolor)))
             (glgui:draw-box cx cy cw ch cc)))
       ))
     h
  ))

(define (glgui:uiform-textentry-input type mx my . args)
  (let* ((id  (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (name (glgui:uiform-arg args 'name #f))
         (keycb (glgui:uiform-arg args 'keycb #f))
         (keypad-config (glgui:uiform-arg args 'keypad 'default))
         (focusid (uiget 'focusid))
         (keypad-on (uiget 'keypad-on))
         (keypad-height (uiget 'keypad-height))
         (y (uiget 'y))
         (h (uiget 'h)))
    (if (and id (fx= type EVENT_BUTTON1UP)) (begin
      (uiset 'focusid id)
      (uiset 'focuslocation loc)
      (uiset 'focuskeycb keycb)
      (uiset 'focuskeycb (if name glgui:uiform-keycb-name keycb))
      (uiset 'keypad (case keypad-config
          ((default) keypad:simplified)
          ((numfloat) keypad:numfloat)
          ((numint) keypad:numeric)
          ((numcolon)  keypad:numcolon)
          ((numdash)  keypad:numdash)
          ((full) keypad:default)
          (else      keypad:simplified)))
      (uiset 'toggle #f)
      (uiset 'shift (if (and name id)
                      (let ((str (xxget loc id #f)))
                        (or (not str) (and (string? str) (fx= (string-length str) 0))))
                      #f))
      (glgui:uiform-keypad-up)
      (if (and keypad-on id (eq? id focusid)) (glgui:uiform-keypad-down))
   ))))

(uiform-register 'textentry glgui:uiform-textentry-draw glgui:uiform-textentry-input)

;; -------------
;; time entry

(define (glgui:uiform-timeentry-draw x y w . args)
  (let* ((h 48)
         (fnt (uiget 'fnt))
         (label (glgui:uiform-arg args 'text ""))
         (id (glgui:uiform-arg args 'id #f))
         (ampm (glgui:uiform-arg args 'ampm #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (password  (glgui:uiform-arg args 'password #f))
         (default "HH:MM")
         (focusid  (uiget 'focusid))
         (hasfocus (eq? focusid id))
         (idvalue (if id (xxget loc id #f)))
         (fgcolor White)
         (idvaluestr (if (and (string? idvalue) (or hasfocus (fx> (string-length idvalue) 0)))
                       idvalue
                       (begin
                         (set! fgcolor (uiget 'color-default))
                         default)))
         (ampmvalue (if id (xxget loc ampm #f)))
         (defaultampm (glgui:uiform-arg args 'defaultampm "AM"))
         (ampmvaluestr (if (string? ampmvalue)
                             ampmvalue
                             (begin
                               ;; If no ampmvalue, set it - always has a value displayed
                               (xxset loc ampm defaultampm)
                               defaultampm)))
         (defcolor (uiget 'color-default))
         (selcolor (uiget 'color-select))
         (buttoncolor White)
         (indent (glgui:uiform-arg args 'indent
            (if (string=? label "") 0.1 0.3)))
         (txtw  (if (and focusid idvalue idvaluestr) (glgui:stringwidth idvaluestr fnt) 0))
         (txth  (if focusid (glgui:fontheight fnt) 0))
         (ampmw (* w 0.2)))
    
     (if (uiget 'sanemap) (begin
       (glgui:draw-text-right x y (- (* w indent) 10) h label fnt White)
       (glgui:draw-box (+ x (* w indent)) y (- (* w (- 1. indent)) ampmw 4) h (if hasfocus selcolor defcolor))
       (glgui:draw-box (+ x (- w ampmw 2)) y ampmw h defcolor)
       (glgui:draw-text-center (+ x (- w ampmw 2)) y ampmw h ampmvaluestr fnt White)
       (if idvaluestr (glgui:draw-text-left (+ x (* w indent) 10) y (- (* w (- 1. indent)) ampmw 10) h idvaluestr fnt fgcolor))
       (if hasfocus
          (let* ((cx (+ x (* w indent) 10 txtw 2))
                 (cy (+ y (/ (- h txth) 2.)))
                 (cw 3) (ch txth)
                 (cc (if (odd? (fix (* 2 ##now))) White selcolor)))
             (glgui:draw-box cx cy cw ch cc)))
       ))
     h
  ))

(define (glgui:uiform-timeentry-input type mx my . args)
  (let* ((id  (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (focusid (uiget 'focusid))
         (keypad-on (uiget 'keypad-on))
         (keypad-height (uiget 'keypad-height))
         (x (uiget 'x))
         (w (uiget 'w)))
    
    (if (and id (fx= type EVENT_BUTTON1UP))
      (let ((ampmw (* w 0.2)))
        (if (> mx (+ x (- w ampmw 4)))
          (let* ((ampm (glgui:uiform-arg args 'ampm #f))
                 (ampmvalue (if id (xxget loc ampm #f)))
                 (defaultampm (glgui:uiform-arg args 'defaultampm "AM"))
                 (ampmvaluestr (if (string? ampmvalue) ampmvalue defaultampm)))
            (xxset loc ampm (if (string=? ampmvaluestr "PM") "AM" "PM")))
          (begin
            (uiset 'focusid id)
            (uiset 'focuslocation loc)
            (uiset 'focuskeycb (lambda (floc fid str)
                                  (let ((len (string-length str))
                                        (cindex (string-index str #\:)))
                                    (cond
                                      ((and (fx= len 2) (not cindex))
                                         (xxset floc fid (string-append (substring str 0 1) ":" (substring str 1 2))))
                                      ((and (fx= len 4) (fx= cindex 2))
                                          ;; Move colon left
                                          (xxset floc fid (string-append (substring str 0 1) ":" (substring str 1 2) (substring str 3 4))))
                                      ((and (fx= len 5) (fx= cindex 1))
                                          ;; Move colon right
                                          (xxset floc fid (string-append (substring str 0 1) (substring str 2 3) ":" (substring str 3 5))))
                                      ((fx> len 5)
                                         (xxset floc fid (substring str 0 5)))))))
            
            (uiset 'keypad keypad:numeric)
            (uiset 'toggle #f)
            (uiset 'shift #f)
            (glgui:uiform-keypad-up)
            (if (and keypad-on id (eq? id focusid)) (glgui:uiform-keypad-down))))))))

(uiform-register 'timeentry glgui:uiform-timeentry-draw glgui:uiform-timeentry-input)

;; -------------
;; button

(define (glgui:uiform-button-draw x y w . args)
  (let* ((bfnt (uiget 'btfnt))
         (fntsize (glgui:uiform-arg args 'size 'normal))
         (fnt (cond
                 ((and (eq? fntsize 'normal) bfnt) bfnt)
                 ((eq? fntsize 'normal) (uiget 'fnt))
                 ((eq? fntsize 'small) (uiget 'smlfnt))
                 ((eq? fntsize 'big) (uiget 'bigfnt))
                 ((eq? fntsize 'header) (uiget 'hdfnt))))
         (fnth (glgui:fontheight fnt))
         (color (glgui:uiform-arg args 'color White))
         (bgcolor (uiget 'button-color (uiget 'color-default)))
         (r (glgui:uiform-arg args 'rounded #f))
         (strings (string-split-width (glgui:uiform-arg args 'text "") (fix (* 0.7 w)) fnt))
         (h (glgui:uiform-arg args 'h (+ 32 (* (length strings) fnth))))
         (indent (glgui:uiform-arg args 'indent 0.1)))
     (if (uiget 'sanemap) (begin
       ((if r glgui:draw-rounded-box glgui:draw-box) (+ x (* w indent)) y (* w (- 1.0 (* indent 2.0))) h bgcolor)
       (let loop ((ss (reverse strings))(ypos (+ y (/ (- h (* (length strings) fnth)) 2))))
         (if (fx> (length ss) 0) (begin
           (glgui:draw-text-center x ypos w fnth (car ss) fnt color) 
           (loop (cdr ss) (+ ypos fnth)))))
       ))
     h
  ))

(define (glgui:uiform-button-input type x y . args)
  (let ((action (glgui:uiform-arg args 'action #f)))
    (if action (begin
      (uiset 'nodemap '())
      (glgui:uiform-action action))
 )))

(uiform-register 'button glgui:uiform-button-draw glgui:uiform-button-input)

;; --------------
;; progress bar

(define (glgui:uiform-progress-draw x y w . args)
  (let ((h (glgui:uiform-arg args 'height 32))
        (defcolor (uiget 'color-default)) 
        (selcolor (uiget 'color-select)) 
        (value (min 1. (max 0. (glgui:uiform-arg args 'value 0.)))))
     (if (uiget 'sanemap) (begin 
       (glgui:draw-box (+ x (* w 0.1)) y (* w 0.8) h defcolor)
       (glgui:draw-box (+ x (* w 0.1) 2) (+ y 2) (* value (- (* w 0.8) 4.)) (- h 4) selcolor)
     ))
  h))

(uiform-register 'progress glgui:uiform-progress-draw #f)

;; --------------
;; encode DM

(define (glgui:uiform-dmencode-draw x y w . args)
  (let* ((str (glgui:uiform-arg args 'text #f))
         (strhash (string-append "DM" (number->string (string=?-hash str))))
         (strimg (uiget strhash #f))
         (img (if strimg strimg 
           (let* ((gd (dmtx->gd str))
                  (img (gd->img gd)))
            (gdImageDestroy gd) img)))
         (h (cadr img)))
    (uiset strhash img)
    (if (uiget 'sanemap) (begin
      (glgui:draw-pixmap-center x y w h img White)
    ))
    h))

(uiform-register 'dmencode glgui:uiform-dmencode-draw #f)

;; --------------
;; decode DM

(define (glgui:uiform-dmdecode-draw x y w . args)
  (let* ((id (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (curmsg (xxget loc id #f))
         (tmpfile (string-append (system-directory) (system-pathseparator) "tmp_"
           (if (symbol? id) (symbol->string id) id) ".jpg"))
         (newmsg (if (file-exists? tmpfile) 
            (let* ((fd (gdFileOpen tmpfile "r"))
                   (gd (gdImageCreateFromJpeg fd))
                   (msg (gd->dmtx gd)))
              (gdImageDestroy gd)
              (gdFileClose fd)
              (delete-file tmpfile)
              msg) ""))
         (msg (if (> (string-length newmsg) 0) newmsg 
           (if curmsg curmsg (glgui:uiform-arg args 'default ""))))
         (fnt (uiget 'fnt))
         (fnth (glgui:fontheight fnt))
         (ss (reverse (string-split-width msg (fix (* 0.7 w)) fnt)))
         (h (+ (* fnth (length ss)) 20)))
     (if (> (string-length newmsg) 0) (xxset loc id newmsg))
     (if (uiget 'sanemap) (begin      
       (glgui:draw-box (+ x (* w 0.1)) y (* w 0.8) h (uiget 'color-default))
       (let loop ((s ss)(ypos (+ y 10)))
         (if (fx> (length s) 0) (begin
           (glgui:draw-text-center x ypos w fnth (car s) fnt White)
           (loop (cdr s) (+ ypos fnth)))))
       ))
     h
  ))

(define (glgui:uiform-dmdecode-input type x y . args)
  (let* ((id (glgui:uiform-arg args 'id #f))
         (tmpfile (string-append (system-directory) (system-pathseparator) "tmp_"
           (if (symbol? id) (symbol->string id) id) ".jpg")))
    (if (file-exists? tmpfile) (delete-file tmpfile))
    (camera-start tmpfile)
 ))

(uiform-register 'dmdecode glgui:uiform-dmdecode-draw glgui:uiform-dmdecode-input)

;; --------------
;; camera support

(define (glgui:uiform-camera-draw x y w . args)
  (let* ((filename (glgui:uiform-arg args 'filename #f))
         (imagepath (if filename (string-append (system-directory) (system-pathseparator) filename) #f))
         (curimg (uiget filename #f))
         (newimg (if (file-exists? imagepath)
            (let* ((fd (gdFileOpen imagepath "r"))
                   (gd (gdImageCreateFromJpeg fd))
                   (w0 (gdImageSX gd))
                   (h0 (gdImageSY gd))
                   (w1 (fix (* 0.8 w)))
                   (h1 (fix (/ (* h0 w1) w0)))
                   (gd2 (gdImageCreateTrueColor w1 h1))
                   (img (begin 
                     (gdImageCopyResampled gd2 gd 0 0 0 0 w1 h1 w0 h0)
                     (gd->img gd2))))
              (gdImageDestroy gd)
              (gdImageDestroy gd2)
              (gdFileClose fd) img) #f))
         (img (if newimg newimg (if curimg curimg #f)))
         (h (if img (cadr img) (fix (* w 0.8))))
         (fnt (uiget 'fnt)))
     (if newimg (uiset filename newimg))
     (if (file-exists? imagepath) (delete-file imagepath))
     (if (uiget 'sanemap) (begin
       (if img (glgui:draw-pixmap-center x y w h img White)
       (begin
         (glgui:draw-box (+ x (* w 0.1)) y (* w 0.8) h (uiget 'color-default))
         (glgui:draw-text-center x y w h (glgui:uiform-arg args 'default "Take photo") fnt White)
       ))
     ))
   h
  ))

(define (glgui:uiform-camera-input type x y . args)
  (let* ((filename (glgui:uiform-arg args 'filename #f))
         (imagepath (if filename (string-append (system-directory) (system-pathseparator) filename) #f)))
    (if imagepath (begin
      (if (file-exists? imagepath) (delete-file imagepath))
      (camera-start imagepath)
      (uiset 'nodemap '())
    ))
 ))

(uiform-register 'camera glgui:uiform-camera-draw glgui:uiform-camera-input)

;; --------------
;; radio box

(define (glgui:uiform-radio-draw x y w . args)
  (let* ((h 48) 
         (fnt (uiget 'fnt))
         (id (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (curvalue (xxget loc id #f))
         (boxcolor (uiget 'color-default))
         (text (glgui:uiform-arg args 'text #f))
         (color (glgui:uiform-arg args 'color White))
         (left (glgui:uiform-arg args 'left #f))
         (leftstr (car left))
         (leftvalue (cadr left))
         (leftsel (equal? leftvalue curvalue))
         (lefth (* 0.8 h))
         (leftw (* 0.8 h))
         (indent (glgui:uiform-arg args 'indent 0.3))
         (leftx (* indent w))
         (lefty (+ y (* 0.1 h)))
         (right  (glgui:uiform-arg args 'right #f))
         (righth (* 0.8 h))
         (rightw (* 0.8 h))
         (rightx (* 0.6 w))
         (righty (+ y (* 0.1 h)))
         (rightstr (car right))
         (rightvalue (cadr right))
         (rightsel (equal? rightvalue curvalue)))
     (if (uiget 'sanemap) (begin
       (if text (glgui:draw-text-right x y (- leftx 10) h text fnt color))
       (glgui:draw-box leftx lefty leftw lefth boxcolor)
       (if leftsel (glgui:draw-pixmap-center leftx lefty leftw lefth check.img White))
       (glgui:draw-text-left (+ leftx leftw 10) y (* 0.3 w) h leftstr fnt color)
       (glgui:draw-box rightx righty rightw righth boxcolor)
       (if rightsel (glgui:draw-pixmap-center rightx righty rightw righth check.img White))
       (glgui:draw-text-left (+ rightx rightw 10) y (* 0.3 w) h rightstr fnt color)
     ))
     h
  ))

(define (glgui:uiform-radio-input type x y . args)
  (let* ((w (uiget 'w))
         (id (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (left (glgui:uiform-arg args 'left #f))
         (leftvalue (cadr left))
         (leftaction (caddr left))
         (indent (glgui:uiform-arg args 'indent 0.3))
         (right (glgui:uiform-arg args 'right #f))
         (rightvalue (cadr right))
         (rightaction (caddr right)))
    (if (and id (> x (* indent w)) (< x (* 0.6 w))) (begin
      (xxset loc id leftvalue)
      (uiset 'nodemap '())
      (glgui:uiform-action leftaction)
    ))
    (if (and id (> x (* 0.6 w))) (begin
      (xxset loc id rightvalue)
      (uiset 'nodemap '())
      (glgui:uiform-action rightaction)
    ))
 ))

(uiform-register 'radiobox glgui:uiform-radio-draw glgui:uiform-radio-input)

;; --------------
;; check box

(define (glgui:uiform-check-draw x y w . args)
  (let* ((fnt (uiget 'fnt))
         (fntsize (glgui:uiform-arg args 'size 'normal))
         (fnt (uiget (case fntsize
                       ((normal) 'fnt)
                       ((small) 'smlfnt)
                       ((big) 'bigfnt)))) 
         (fnth (glgui:fontheight fnt))
         (h (+ fnth 20))
         (id (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (curvalue (xxget loc id #f))
         (boxcolor (uiget 'color-default))
         (color (glgui:uiform-arg args 'color White))
         (text (glgui:uiform-arg args 'text #f))
         (indent (glgui:uiform-arg args 'indent 0.2))
         (bx (* indent w))
         (by (+ y (* 0.1 h)))
         (bh (* 0.8 h))
         (bw (* 0.8 h)))
    (if (uiget 'sanemap) (begin
      (glgui:draw-box bx by bw bh boxcolor)
      (if curvalue (glgui:draw-pixmap-center bx by bw bh check.img White))
      (glgui:draw-text-left (+ bx bw 10) (+  y (/ (- h fnth) 2.)) (- w bx bw 10 10) fnth text fnt color) 
    ))
  h
))

(define (glgui:uiform-check-input type x y . args)
  (let* ((w (uiget 'w))
         (id (glgui:uiform-arg args 'id #f))
         (indent (glgui:uiform-arg args 'indent 0.2))
         (loc (glgui:uiform-arg args 'location 'db))
         (curvalue (xxget loc id #f)))
    (if (and id (> x (* indent w)) (< x (* 0.9 w))) (begin
      (xxset loc id (not curvalue))
      (uiset 'nodemap '())
    ))))

(uiform-register 'checkbox glgui:uiform-check-draw glgui:uiform-check-input)

;; --------------
;; drop down

(define (glgui:uiform-dropdown-draw x y w . args)
  (let* ((h 48)
         (hh (/ h 2))
         (limitw (* w 0.6))
         (id (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (expandid (if id (string-append (if (string? id) id (symbol->string id)) ":expanded") #f))
         (defaultstr (glgui:uiform-arg args 'text "Choose one"))
         (fntsize (glgui:uiform-arg args 'size 'normal))
         (fnt (uiget (case fntsize
                       ((normal) 'fnt)
                       ((small) 'smlfnt)
                       ((big) 'bigfnt))))
         (entries (glgui:uiform-arg args 'entries #f))
         (curentry (xxget loc id #f)) 
         (expanded (if expandid (uiget expandid) #f))
         (defcolor (uiget 'color-default))
         (selcolor (uiget 'color-select)))
   (if expanded 
     (let loop ((es (reverse entries))(dy 0))
       (if (= (length es) 0) (begin
           (if (uiget 'sanemap) (begin
               (glgui:draw-box (+ x (* w 0.1)) (+ y dy 1) (* w 0.8) (- h 1) defcolor)
               (if (> (glgui:stringwidth defaultstr fnt) limitw)
                 (let ((lines (string-split-width defaultstr limitw fnt)))
                   (glgui:draw-text-center x (+ y dy hh) w hh (car lines) fnt defcolor)
                   (glgui:draw-text-center x (+ y dy) w hh (cadr lines) fnt defcolor))
                 (glgui:draw-text-center x (+ y dy) w h defaultstr fnt defcolor))
           ))
           (+ dy h) 
         ) (begin
         (if (uiget 'sanemap)
           (let ((text (car es)))
             (glgui:draw-box (+ x (* w 0.1)) (+ y dy 1) (* w 0.8) (- h 2) defcolor)
             (if (> (glgui:stringwidth text fnt) limitw)
               (let ((lines (string-split-width text limitw fnt)))
                 (glgui:draw-text-center x (+ y dy hh) w hh (car lines) fnt White)
                 (glgui:draw-text-center x (+ y dy) w hh (cadr lines) fnt White))
               (glgui:draw-text-center x (+ y dy) w h text fnt White))))
         (loop (cdr es)(+ dy h)))))
     (begin
       (if (uiget 'sanemap)
         (let ((text (if curentry curentry defaultstr))
               (col (if curentry White defcolor)))
           (glgui:draw-box (+ x (* w 0.1)) y (* w 0.8) h defcolor)
           (glgui:draw-box (+ x (* w 0.8)) y (* w 0.1) h selcolor)
           (if (> (glgui:stringwidth text fnt) limitw)
             (let ((lines (string-split-width text limitw fnt)))
               (glgui:draw-text-center x (+ y hh) w hh (car lines) fnt col)
               (glgui:draw-text-center x y w hh (cadr lines) fnt col))
             (glgui:draw-text-center x y w h text fnt col))
       ))
     h))
  ))

(define (glgui:uiform-dropdown-input type x y . args)
  (let*  ((id (glgui:uiform-arg args 'id #f))
          (loc (glgui:uiform-arg args 'location 'db))
          (expandid (if id (string-append (if (string? id) id (symbol->string id)) ":expanded") #f))
          (entries (glgui:uiform-arg args 'entries #f))
          (expanded (if expandid (uiget expandid) #f)))
     (if (not expanded)
       (let* ((ofs (flo (uiget 'offset)))
              (node-y (uiget 'node-y))
              (node-height (uiget 'node-height))
              (n (+ (length entries) 1))
              (bottomy (- node-y (* n node-height))))
          ;; If the bottom of the dropdown cannot be seen with some space, autoscroll down
          (if (< bottomy 5.)
            (uiset 'offset (+ ofs (- bottomy) 5)))
         (uiset 'nodemap '())
         (uiset expandid #t)
     ) (let* ((node-y (uiget 'node-y))
            (node-height (uiget 'node-height))
            (n (+ (length entries) 1))
            (fidx (- (* n (/ (- node-y y) node-height)) 1.))
            (idx (if (>= fidx 0) (fix fidx) -1)))
       (if (and (>= idx 0) (< idx (length entries))) 
          (begin (uiset 'nodemap '())
          (xxset loc id (list-ref entries idx)))
          (begin (uiset 'nodemap '())
          (xxclear loc id)))
       (uiset expandid #f)
   ))))

(uiform-register 'dropdown glgui:uiform-dropdown-draw glgui:uiform-dropdown-input)

;; -------------
;; list

(define (glgui:uiform-list-draw x y w . args)
  (let* ((h 48)
         (id (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (fnt (uiget 'fnt))
         (entries (glgui:uiform-arg args 'entries '()))
         (noentries (length entries))
         (defcolor (uiget 'color-default))
         (selcolor (uiget 'color-select))
         (selection (xxget loc id #f)))
     (let loop ((es (reverse entries))(dy 0))
       (if (= (length es) 0) dy (begin
         (if (uiget 'sanemap) (begin
           (glgui:draw-box (+ x (* w 0.1)) (+ y dy 1) (* w 0.8) (- h 2) 
             (if (and selection (string=? (car es) selection)) selcolor defcolor))
           (glgui:draw-text-center x (+ y dy) w h (car es) fnt White)
         ))
         (loop (cdr es)(+ dy h)))))
  ))

(define (glgui:uiform-list-input type x y . args)
  (let* ((id (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (entries (glgui:uiform-arg args 'entries #f))
         (node-y (uiget 'node-y))
         (node-height (uiget 'node-height))
         (n (length entries))
         (idx (fix (* n (/ (- node-y y) node-height)))))
   (if id (begin
     (if (and (>= idx 0) (< idx n)) (begin
       (uiset 'nodemap '())
       (xxset loc id (list-ref entries idx))
     ))
 ))))

(uiform-register 'list glgui:uiform-list-draw glgui:uiform-list-input)

;; -------------
;; checklist

(define (glgui:uiform-checklist-draw x y w . args)
  (let* ((h 48)
         (id (glgui:uiform-arg args 'id #f))
         (idmerged (string-append (if (string? id) id (symbol->string id)) ":merged"))
         (loc (glgui:uiform-arg args 'location 'db))
         (fnt (uiget 'fnt))
         (defaultentries (glgui:uiform-arg args 'default '()))
         (actualentries (xxget loc id '()))
         (mergedentries (let loop ((es (append defaultentries actualentries))(res '()))
           (if (= (length es) 0) res (loop (cdr es) (append res (if (member (car es) res) '() (list (car es))))))))
         (mergedselections (let loop ((es mergedentries)(res '()))
           (if (= (length es) 0) res (loop (cdr es) (append res (if (member (car es) actualentries) '(#t) '(#f)))))))
         (noentries (length mergedentries))
         (boxcolor (uiget 'color-default)))
     (uiset idmerged mergedentries)
     (let loop ((es (reverse mergedentries))(ss (reverse mergedselections))(dy 0))
       (if (= (length es) 0) dy (begin
         (if (uiget 'sanemap)
           (let ((bx (+ x (* w 0.1) (* h 0.1)))
                 (by (+ y dy (* h 0.1)))
                 (bw (* h 0.8))
                 (bh (* h 0.8)))
             (glgui:draw-box (+ x (* w 0.1)) (+ y dy 1) (* w 0.8) (- h 2) boxcolor)
             (glgui:draw-box bx by bw bh boxcolor)
             (if (car ss) (glgui:draw-pixmap-center bx by bw bh check.img White))
             (glgui:draw-text-center x (+ y dy) w h (car es) fnt White)))
         (loop (cdr es)(cdr ss)(+ dy h)))))
  ))

(define (glgui:uiform-checklist-input type x y . args)
  (let* ((id (glgui:uiform-arg args 'id #f))
         (radio (glgui:uiform-arg args 'radio #f))
         (idmerged (string-append (if (string? id) id (symbol->string id)) ":merged"))
         (loc (glgui:uiform-arg args 'location 'db))
         (entries (uiget idmerged))  ;; identifier in widget holds all entries
         (actual (xxget loc id '()))  ;; identifier in database holds actual entries
         (node-y (uiget 'node-y))
         (node-height (uiget 'node-height))
         (n (length entries))
         (idx (fix (* n (/ (- node-y y) node-height)))))
   (if  id (begin
     (if radio (begin (xxset loc id '()) (set! actual '())))
     (if (and (>= idx 0) (< idx n)) 
       (let ((element (list-ref entries idx)))
         (if (member element actual)
           (begin (uiset 'nodemap '()) (xxset loc id (list-delete-item actual element))) ;; (uiform-db-listremove! id element)
           (begin (uiset 'nodemap '()) (xxset loc id (list-insert-item actual element))) ;; (uiform-db-listinsert! id element)
         )
     ))))))

(uiform-register 'checklist glgui:uiform-checklist-draw glgui:uiform-checklist-input)
  
;; -------------
;; uiform graph

(define (glgui:uiform-graph-draw x y w . args)
  (let* ((h (flo (glgui:uiform-arg args 'height 100)))
         (id (glgui:uiform-arg args 'id #f))
         (loc (glgui:uiform-arg args 'location 'db))
         (color (glgui:uiform-arg args 'color White))
         (linewidth (glgui:uiform-arg args 'linewidth 2.))
         (ydatafull (xxget loc id '()))
         (ydatafulllen (length ydatafull))
         (stride (glgui:uiform-arg args 'stride 3))
         (n (fix (/ ydatafulllen stride)))
         (ydata (if (not (null? ydatafull)) (let loop ((i 0)(res '()))
             (if (fx= i n) res
               (loop (fx+ i 1) (append res 
                 (let ((num (list-ref ydatafull (fxmin (fx- ydatafulllen 1) (fix (* i stride))))))
                   (if (number? num) (list (flo num)) '())))))) '()))
         (ymin (flo (glgui:uiform-arg args 'ymin (if (null? ydata) 0. (apply min ydata)))))
         (ymax (flo (glgui:uiform-arg args 'ymax (if (null? ydata) 1. (apply max ydata)))))
         (ypad (flo (glgui:uiform-arg args 'ypad 0.)))
         (xmin (glgui:uiform-arg args 'xmin x))
         (xmax (glgui:uiform-arg args 'xmax (+ x w)))
         (dx (flo (/ (- xmax xmin) (max 1 (- n 1)))))
         (xydata (let loop ((ys ydata)(tmpx (flo x))(res '()))
           (if (fx= (length ys) 0) res (loop (cdr ys) (fl+ tmpx dx)
             (append res (list (list tmpx (fl+ (flo y) ypad 
                (fl/ (fl* (- h (fl* 2. ypad)) (fl- (car ys) ymin)) (fl- ymax ymin))))))))))
         (trendlabelfnt (uiget 'smlfnt))
         (trendlabel (glgui:uiform-arg args 'trendlabel #f))
         (trendlabelheight (glgui:fontheight trendlabelfnt)) 
         (trendcolor (glgui:uiform-arg args 'trendcolor color))
         (trendfnt (uiget 'bigfnt))
         (trend (glgui:uiform-arg args 'trend #f))
         (trendheight (glgui:fontheight trendfnt)) 
        )
     (if (uiget 'sanemap) (begin
       (glgui:draw-box x y w h (uiget 'color-graph (uiget 'color-header)))
       (glgui:draw-linestrip xydata color linewidth)
       (if (string? trendlabel) (glgui:draw-text-right x (+ y h (- trendlabelheight) -5.) (- w 5.) trendlabelheight trendlabel trendlabelfnt trendcolor))
       (if (string? trend) (glgui:draw-text-right x (+ y 5.) (- w 5.) trendheight trend trendfnt trendcolor))
     ))
     h  
  ))

(uiform-register 'graph glgui:uiform-graph-draw #f)

;; -------------
;; uiform modal

(define (glgui:uiform-modal-draw g wgt)
  (let* ((x (flo (uiget 'x)))
         (y (flo (uiget 'y)))
         (w (flo (uiget 'w)))
         (h (flo (uiget 'h)))
         (fnt (uiget 'fnt))
         (fnth (glgui:fontheight fnt))
         (content (uiget 'modal-content))
         (modal-height (uiget 'modal-height))
         (color-background (uiget 'color-low))
         (color-button (uiget 'color-default))
         (button1str (car (cadr content)))
         (button2str (if (= (length content) 3) (car (caddr content)) #f)))
    (glgui:draw-box x y w h color-background)
    (glgui:draw-box (+ x (* 0.1 w)) (+ y (* 0.5 (- h modal-height))) (* 0.8 w) modal-height Black)
    (let loop ((ss (reverse (string-split-width (car content) (fix (* 0.7 w)) fnt)))
               (ypos (+ y (* 0.5 h))))
      (if (fx> (length ss) 0) (begin
        (glgui:draw-text-center x ypos w fnth (car ss) fnt White)  ;; XX
        (loop (cdr ss) (+ ypos fnth)))))
    (let ((bw (* 0.2 w))
          (bh 48)
          (bx1 (if button2str (+ x (* 0.2 w)) (+ x (* 0.4 w))))
          (bx2 (+ x (* 0.6 w)))
          (by (+ y (* 0.5 (- h modal-height)) (* 0.1 modal-height))))
      (glgui:draw-box bx1 by bw bh color-button)
      (glgui:draw-text-center  bx1 by bw bh button1str fnt White)
      (if button2str (begin 
        (glgui:draw-box bx2 by bw bh color-button)
        (glgui:draw-text-center  bx2 by bw bh button2str fnt White)
      ))
    )
  ))

(define (glgui:uiform-modal-input g wgt type mx my)
  (let* ((x (flo (uiget 'x)))
         (y (flo (uiget 'y)))
         (w (flo (uiget 'w)))
         (h (flo (uiget 'h)))
         (content (uiget 'modal-content))
         (button1action (cadr (cadr content)))
         (button2action (if (= (length content) 3) (cadr (caddr content)) #f))
         (modal-height (uiget 'modal-height))
         (bw (* 0.2 w))
         (bh 48)
         (bx1 (if (= (length content) 3) (+ x (* 0.2 w)) (+ x (* 0.4 w))))
         (bx2 (+ x (* 0.6 w)))
         (by (+ y (* 0.5 (- h modal-height)) (* 0.1 modal-height))))
   (if (fx= type EVENT_BUTTON1UP) (begin
     (if (and (>= mx bx1) (<= mx (+ bx1 bw)) (>= my by) (<= my (+ by bh))) (begin
       (uiset 'modal-action button1action)
       (glgui:uiform-modal-down)
     ))
     (if (and (= (length content) 3) (>= mx bx2) (<= mx (+ bx2 bw)) (>= my by) (<= my (+ by bh))) (begin
       (uiset 'modal-action button2action)
       (glgui:uiform-modal-down)
     ))
   ))
 ))

;; -------------
;; uiform

(define (glgui:uiform-draw g wgt)
  (let* ((keypad-on (uiget 'keypad-on))
         (modal-on (uiget 'modal-on))
         (keypad-height (if keypad-on (uiget 'keypad-height) 0))
         (x (flo (uiget 'x)))
         (y (flo (+ (uiget 'y) keypad-height)))
         (w (flo (uiget 'w)))
         (h (flo (- (uiget 'h) keypad-height)))
         (content-height (uiget 'contenth))
         (header-height (uiget 'headerh))
         (visible-height (- h header-height))
         (fnt (uiget 'fnt))
         (hfnt (uiget 'hdfnt fnt))
         (bfnt (uiget 'btfnt fnt))
         (uiform (uiget 'uiform))
         (nodes (table-ref uiform (uiget 'page) '()))
         (oldmap (uiget 'nodemap))
         (title (car nodes))
         (prv (uiform:eval (cadr nodes)))
         (nxt (uiform:eval (caddr nodes)))
         (nonodes (- (length nodes) 3))
         (ofs (uiget 'offset)))

   (if keypad-on (begin
       (uiset 'h keypad-height)
       (glgui:keypad-draw g wgt)
       (uiset 'h (+ h keypad-height))))

   (glgui:draw-box x (+ y h (- header-height)) w header-height (uiget 'color-header))

   (if (and title fnt)
     (let* ((titlex (fix (+ x (* 0.25 w))))
            (titlew (fix (* 0.5 w)))
            (wrappedtitle (string-split-width title titlew hfnt))
            (titleh (glgui:fontheight hfnt))
            (titley (+ y h (- header-height) (/ (- header-height (* titleh (length wrappedtitle))) 2))))
        (let loop ((titles (reverse wrappedtitle)))
          (if (> (length titles) 0)
            (begin
              (glgui:draw-text-center titlex titley titlew titleh (car titles) hfnt White)
              (set! titley (+ titley titleh))
              (loop (cdr titles)))))))

   ;; Date and time on all but the first page
   (if (not (eq? (uiget 'page) 'main))
     (let* ((dateh (glgui:fontheight fnt))
            (datey (+ y h (- (+ dateh 3)))))
       (glgui:draw-text-left (+ x 3) datey (* 0.95 w) dateh (seconds->string ##now "%Y-%m-%d") fnt White)
       (glgui:draw-text-center (+ x (* 0.25 w)) datey (* 0.5 w) dateh (seconds->string ##now "%H:%M") fnt White)))

   (if (and (list? prv) (> (length prv) 1))
     (let* ((prv-title (car prv))
           (prv-action (cadr prv)))
         (glgui:draw-box x (+ y h (- header-height) (* 0.25 header-height)) (* 0.25 w) (* 0.5 header-height) 
            (uiget 'color-default))
         (glgui:draw-text-center x (+ y h (- header-height)) (/ w 4) header-height prv-title bfnt White)
       (uiset 'prv-action prv-action)
     ))

   (if (and (list? nxt) (> (length nxt) 1))
     (let* ((nxt-title (car nxt))
           (nxt-action (cadr nxt)))
           (glgui:draw-box (+ x (* 0.75 w)) (+ y h (- header-height) (* 0.25 header-height)) (* 0.25 w) (* 0.5 header-height)  
              (uiget 'color-default))
           (glgui:draw-text-center (+ x (/ (* 3 w) 4)) (+ y h (- header-height)) (/ w 4) header-height nxt-title bfnt White)
         (uiset 'nxt-action nxt-action)
     ))

   (glCoreClipPush x y (fl+ x w) (fl+ y h (- header-height)))

   (glCoreColor White)

   ;; Draw each node
   (uiset 'sanemap (not (null? oldmap)))
   (let loop ((idx 0)(y0 (+ y (- (- h header-height) content-height (- ofs))))(totalh 0)(nodemap (if (null? oldmap) '() #f)))
     (if (fx= idx nonodes) (begin
          (uiset 'contenth totalh)
          (if nodemap (uiset 'nodemap nodemap))
          (if (< totalh visible-height) 
            (uiset 'offset 0))
        )
       (let* ((node-noeval (list-ref nodes (+ 3 (- nonodes idx 1))))
              (node (if (procedure? node-noeval) (uiform:evalelement node-noeval) node-noeval))
              (bx x) (bw w) (by y0)
              (bh (let ((in (car (table-ref uiform:elements (car node) (list #f)))))
                (if in (apply in (append (list bx by bw) (cdr node))) 0.)
              ))
              (newnodemap (if nodemap (append 
                (list 
                  ;;(list bh (car node) (glgui:uiform-arg (cdr node) 'action #f))
                  (append (list bh) node) 
                ) nodemap) #f))
             )
         (loop (fx+ idx 1) (+ y0 bh) (+ totalh bh) newnodemap))))

   ;; scroll bar
   (let* ((visible-height (- h header-height))
          (content-height (uiget 'contenth))
          (allvis (>= visible-height content-height)))
     (if (not allvis) 
       (let* ((sw 5.) (sx (+ x w -6.))
              (sh (* visible-height (/ visible-height content-height)))
              (sy (- (+ y visible-height) (if (= sh visible-height) 0. (* (- visible-height sh) (/ ofs (- content-height visible-height)))) sh)))
       (glgui:draw-box sx sy sw sh (color-fade White 0.2))
     )))

   (glCoreClipPop)

   (if modal-on (glgui:uiform-modal-draw g wgt))

))

(define (glgui:uiform-input g wgt type mx my)
  (let* ((keypad-on (uiget 'keypad-on))
         (keypad-height (if keypad-on (uiget 'keypad-height) 0))
         (x (fix (uiget 'x)))
         (y (fix (+ (uiget 'y) keypad-height)))
         (w (fix (uiget 'w)))
         (h (fix (- (uiget 'h) keypad-height)))
         (ofs (flo (uiget 'offset)))
         (old (uiget 'old))
         (oldy (fix (uiget 'oldy)))
         (fsty (fix (uiget 'fsty)))
         (content-height (fix (uiget 'contenth)))
         (header-height (uiget 'headerh))
         (focus (uiget 'focus))
         (focusid (uiget 'focusid #f))
         (focuslocation (uiget 'focuslocation #f))
         (visible-height (- h header-height))
         (drag (uiget 'drag))
         (inside (and (fx> (fix mx) x) (fx< (fix mx) (fx+ x w)) (fx> (fix my) y) (fx< (fix my) (fx+ y h)))))

    (if keypad-on (begin
       (uiset 'h keypad-height)
       (glgui:keypad-input g wgt type mx my)
       (uiset 'h (+ h keypad-height))))

     (cond
       ((and (fx= type EVENT_MOTION) old) ;; drag
          (if (and (> (abs (- my fsty)) 5) (> content-height visible-height))
             (let* ((deltay (- oldy my))
                    (newofs (max 0 (min (- content-height visible-height) (- ofs deltay)))))
               (uiset 'offset newofs)
               (uiset 'oldy my)
               (uiset 'drag #t)
             ))) 
       ((and inside (fx= type EVENT_BUTTON1DOWN)) ;; touch down
         (uiset 'oldy my)
         (uiset 'fsty my)
         (uiset 'old #t)
         (uiset 'drag #f)
       )
       ((and old (fx= type EVENT_BUTTON1UP)) ;; touch release

         (if (and (not drag) inside) (begin

           (if keypad-on (glgui:uiform-keypad-down))

           (if (and (> (- my y) visible-height) (< (- mx x) (/ w 4)))
             (let ((prv-action (uiget 'prv-action)))
               (if prv-action (glgui:uiform-action prv-action))
            ))

           (if (and (> (- my y) visible-height) (> (- mx x) (/ (* 3 w) 4)))
             (let ((nxt-action (uiget 'nxt-action)))
               (if nxt-action (glgui:uiform-action nxt-action))
            ))

           (let ((nodemap (uiget 'nodemap))
                 (nodey   (- (+ y visible-height) my)))
             (let loop ((nodes nodemap)(tmpy 0))
               (if (= (length nodes) 0) #f
                 (let* ((node-height (car (car nodes)))
                        (node-noeval (cdr (car nodes)))
                        (node (if (procedure? node-noeval) (uiform:evalelement node-noeval) node-noeval))
                        (newy (+ tmpy node-height)))
                    (if (and (> nodey 0) (< (+ nodey ofs) newy))
                       (begin 
                         (uiset 'node-y (- (+ y visible-height ofs) tmpy))
                         (uiset 'node-height node-height)
                         (let ((ih (cadr (table-ref uiform:elements (car node) (list #f #f)))))
                           (if ih (apply ih (append (list type mx my) (cdr node))))
                         )
                       )
                      (loop (cdr nodes) newy))))))))
               
         (uiset 'old #f)
         (uiset 'drag #f)
       )
       ((and focus focusid focuslocation (fx= type EVENT_KEYRELEASE))
          (let* ((oldstr0 (xxget focuslocation focusid ""))
                 (oldstr (if oldstr0 oldstr0 ""))
                 (oldstrlen (string-length oldstr)))
            (cond
              ((fx= mx EVENT_KEYENTER) 
                (glgui:uiform-keypad-down))
              ((fx= mx EVENT_KEYBACKSPACE) 
                (if (> oldstrlen 0) 
                  (let ((cb (uiget 'focuskeycb #f))
                        (newstr (substring oldstr 0 (- oldstrlen 1))))
                    (xxset focuslocation focusid (substring oldstr 0 (- oldstrlen 1)))
                    (if cb (cb focuslocation focusid newstr)))))
              (else
                (let ((cb (uiget 'focuskeycb #f))
                        (newstr (string-append oldstr (string (integer->char mx)))))
                    (xxset focuslocation focusid newstr)
                    (if cb (cb focuslocation focusid newstr))))
            )
        ))
       (else (if (not inside) (uiset 'old #f)))
    )
  inside
))


(define (glgui-uiform g x y w h)
  (let ((wgt (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'draw-handle  glgui:uiform-draw
     'input-handle glgui:uiform-input
     'hidden #f
     'nodes '()
     'back-node #f
     'next-node #f
     'offset 0.
     'fxoffset 0
     'current -1
     'focus #t
     'old  #f
     'drag #f
     'autohidebar #f    ;; Hide scrollbar when list not long enough to need scrolling
     'oldy  0
     'fsty  0
     'bordercolor #f
     'contenth 0.
     'headerh 128.
     'nodemap '()
     'page 'main
     'keypad-height 350
     'keypad-shift 0
     'keypad-on #f
     'modal-height 200
     ;; -------------
     ;; colors
     'color-low     (color-fade Black 0.5)
     'color-default (color-fade White 0.3)
     'color-header  (color-fade Black 0.3)
     'color-select  (color-fade White 0.5)
     'color-high    (color-fade White 0.7)
     ;; -------------
     ;; keypad widget
     'toggle #f
     'shift #f
     'keypad keypad:default
     'armed #f
     'highlight #f 
     'floatinghighlight #f 
     'hideonreturn #f
     'removehighlight #t
     'bgcolor (color-fade Black 0.3)
     'btcolor (color-fade White 0.3)
     'fgcolor White
     'rounded #f
     ;; -------------
   )))
   (set! uiform:g g)
   (set! uiform:wgt wgt)
   wgt))

(define (sa-database->file t file)
  (with-output-to-file 
    (string-append (system-directory) (system-pathseparator) file) 
    (lambda () (pretty-print (table->list t)))))

(define (sa-file->database file)
  (list->table (with-input-from-file 
    (string-append (system-directory) (system-pathseparator) file) 
    (lambda () (read)))))

;; eof
