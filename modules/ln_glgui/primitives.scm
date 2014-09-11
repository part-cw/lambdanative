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
;; geometric primitives


;; ---------
;; lines
;; this is for completeness. Line drawing is very inefficient on some platforms. Use textures instead.

(define (glgui:draw-line x1 y1 x2 y2 color)
  (glCoreColor color)
  (glCoreBegin GL_LINES)
  (glCoreVertex2f (flo x1) (flo y1))
  (glCoreVertex2f (flo x2) (flo y2))
  (glCoreEnd)
)

(define (glgui:draw-linestrip data color)
  (glCoreColor color)
  (glCoreBegin GL_LINE_STRIP)
  (for-each (lambda (d) (glCoreVertex2f (flo (car d)) (flo (cadr d)))) data)
  (glCoreEnd))

;; ----------
;; box

(define glgui:box (glCoreTextureCreate 4 4 (make-u8vector 16 #xff)))

(define (glgui:draw-box x y w h c)
  (if (list? c)
    (glCoreTextureGradientDraw x y w h glgui:box 0.1 0.1 .9 .9 0. c)
    (begin
      (glCoreColor c)
      ;; prevent fuzzy edges
      (glCoreTextureDraw x y w h glgui:box 0.1 0.1 .9 .9 0.)))
)

;; ----------
;; rounded box

;; Minimum size rounded box can be is 13 by 13 since corners are each 6 by 6 
(define (glgui:draw-rounded-box x y w h c)
  (if (not (list? c)) (glCoreColor c))
  
  ;; If box is large in either dimensions, then use separate textures for the corners
  (if (or (> w 50) (> h 50))
    (if (list? c)
      ;; Draw with gradients or without, round coordinates
      (let ((rx (fix x))
            (ry (fix y))
            (rw (fix w))
            (rh (fix h)))
        ;; Left side - corners are a solid colour
        (glCoreColor (caddr c))
        (glCoreTextureDraw rx ry 6 7 glgui_rounded_corner.raw .1 .1 .9 .9 0.)
        ;; Only use left side top and bottom colours
        (glCoreTextureGradientDraw rx (+ ry 6) 6 (- rh 12) glgui:box .1 .1 .9 .9 0. (list (car c) (car c) (caddr c) (caddr c)))
        (glCoreColor (car c))
        (glCoreTextureDraw rx (+ ry (- rh 7)) 6 7 glgui_rounded_corner.raw .1 .9 .9 .1 0.)
        ;; Middle
        (glCoreTextureGradientDraw (+ rx 5.5) (+ ry 0.3) (- rw 11) (- rh 0.6) glgui:box .1 .1 .9 .9 0. c)
        ;; Right side
        (glCoreColor (cadddr c))
        (glCoreTextureDraw (+ rx (- rw 6)) ry 6 7 glgui_rounded_corner.raw .9 .1 .1 .9 0.)
        ;; Only use right side top and bottom colours
        (glCoreTextureGradientDraw (+ rx (- rw 6)) (+ ry 6) 6 (- rh 12) glgui:box .1 .1 .9 .9 0. (list (cadr c) (cadr c) (cadddr c) (caddr c)))
        (glCoreColor (cadr c))
        (glCoreTextureDraw (+ rx (- rw 6)) (+ ry (- rh 7)) 6 7 glgui_rounded_corner.raw .9 .9 .1 .1 0.))
      (let ((rx (fix x))
            (ry (fix y))
            (rw (fix w))
            (rh (fix h)))
        ;; Left side
        (glCoreTextureDraw rx ry 6 7 glgui_rounded_corner.raw .1 .1 .9 .9 0.)
        (glCoreTextureDraw rx (+ ry 6) 6 (- rh 12) glgui:box .1 .1 .9 .9 0.)
        (glCoreTextureDraw rx (+ ry (- rh 7)) 6 7 glgui_rounded_corner.raw .1 .9 .9 .1 0.)
        ;; Middle
        (glCoreTextureDraw (+ rx 5.5) (+ ry 0.3) (- rw 11) (- rh 0.6) glgui:box .1 .1 .9 .9 0.)
        ;; Right side
        (glCoreTextureDraw (+ rx (- rw 6)) ry 6 7 glgui_rounded_corner.raw .9 .1 .1 .9 0.)
        (glCoreTextureDraw (+ rx (- rw 6)) (+ ry 6) 6 (- rh 12) glgui:box .1 .1 .9 .9 0.)
        (glCoreTextureDraw (+ rx (- rw 6)) (+ ry (- rh 7)) 6 7 glgui_rounded_corner.raw .9 .9 .1 .1 0.)))
   
    ;; Otherwise if box is small then use single texture, with or without colour being a gradient
    (if (list? c)
      (glCoreTextureGradientDraw x y w h glgui_rounded_box.raw 0.01 0.01 .99 .99 0. c)
      (glCoreTextureDraw x y w h glgui_rounded_box.raw 0.01 0.01 .99 .99 0.)))
)

;; ----------
;; pixmaps (aka texture lists)

(define (glgui:draw-pixmap-center x y w h img color)
  (let* ((sw (car img)) 
         (sh (cadr img))
         (x0 (+ x (/ (- w sw) 2)))
         (y0 (+ y (/ (- h sh) 2))))
    (glCoreColor color)
    (apply glCoreTextureDraw (append (list x0 y0) img (list 0.)))))

(define (glgui:draw-pixmap-left x y w h img color)
  (let* ((sh (cadr img))
         (y0 (+ y (/ (- h sh) 2))))
    (glCoreColor color)
    (apply glCoreTextureDraw (append (list x y0) img (list 0.)))))

(define (glgui:draw-pixmap-right x y w h img color)
  (let* ((sw (car img))
         (sh (cadr img))
         (y0 (+ y (/ (- h sh) 2)))
         (x0 (+ x w (- sw))))
    (glCoreColor color)
    (apply glCoreTextureDraw (append (list x0 y0) img (list 0.)))))

(define (glgui:draw-pixmap-stretch x y w h img color)
  (glCoreColor color)
  (apply glCoreTextureDraw (append (list x y w h) (cddr img) (list 0.))))

;; ----------
;; strings (aka pixmap lists)

(define glgui:image-w car)
(define glgui:image-h cadr)
(define glgui:glyph-image cadr)

;; support both legacy latex and new truetype rendering
;; eventually this should be stripped out for better performance
(define (glgui:glyph-offsetx g) 
  (if (fx= (length g) 5) (list-ref g 2) 0))
(define (glgui:glyph-offsety g)
  (if (fx= (length g) 5) (list-ref g 4) 
     (glgui:image-h (glgui:glyph-image g))))
(define (glgui:glyph-advancex g)
  (if (fx= (length g) 5) (list-ref g 3)
    (glgui:image-w (glgui:glyph-image g))))

(define glgui:string->glyphs #f)
(define glgui:glyphs->string #f)

(define (glgui-utf8-set! flag) 
  (if flag (begin
    (set! glgui:string->glyphs utf8string->unicode)
    (set! glgui:glyphs->string unicode->utf8string)
  ) (begin 
    (set! glgui:string->glyphs (lambda (s) (map char->integer (string->list s))))
    (set! glgui:glyphs->string (lambda (l) (list->string (map integer->char l))))
  )))

(glgui-utf8-set! #t)

(define (glgui:renderstring x y txt fnt color)
  (glCoreColor color)
  (let loop ((x0 (flo x))(cs (glgui:string->glyphs txt)))
    (if (fx> (length cs) 0)
      (let* ((charcode (car cs))
             (g (assoc charcode fnt))
             (i (if g (glgui:glyph-image g) #f))
             (gh (if i (flo (glgui:image-h i)) 0.))
             (gox (if g (flo (glgui:glyph-offsetx g)) 0.))
             (goy (if g (flo (glgui:glyph-offsety g)) 0.))
             (gax (if g (flo (glgui:glyph-advancex g)) 0.)))
          (if (and i (not (fx= charcode 32)))
            (apply glCoreTextureDraw (append (list (fl+ x0 gox) (fl+ (flo y) goy (fl- gh))) i (list 0.))))
          (loop (fl+ x0 gax) (cdr cs))))))

(define (glgui:fontheight fnt)
  (let* ((g (assoc 0 fnt))
         (i (if g (glgui:glyph-image g) #f))
         (h (if i (glgui:image-h i) 
           (cadr (cadr (car fnt)))))) h))

(define (glgui:stringheight txt fnt)
  (let loop ((above 0.)(below 0.)(cs (glgui:string->glyphs txt)))
    (if (fx= (length cs) 0) (list above below)
      (let* ((g (assoc (car cs) fnt))
             (i (if g (glgui:glyph-image g) #f))
             (gh (if i (flo (glgui:image-h i)) 0.))
             (goy (if g (flo (glgui:glyph-offsety g)) 0.)))
        (loop (flmax above goy)  (flmin below (fl- goy gh)) (cdr cs))))))

(define (glgui:stringwidth txt fnt)
  (let loop ((x 0.)(cs (glgui:string->glyphs txt)))
    (if (fx= (length cs) 0) (fix (ceiling x))
      (let* ((glyph (assoc (car cs) fnt))
             (ax (if glyph (flo (glgui:glyph-advancex glyph)) 0.)))
        (loop (fl+ x ax) (cdr cs))))))

(define (glgui:stringcliplist w0 txtlst fnt)
  (let ((w (flo w0)))
    (let loop ((x 0.)(cs txtlst)(rs '()))
      (if (or (fx= (length cs) 0) (fl> x w)) rs 
        (let* ((glyph (assoc (car cs) fnt))
               (ax (if glyph (flo (glgui:glyph-advancex glyph)) 0.)))
          (loop (fl+ x ax) (cdr cs) (append rs (if (fl<= (fl+ x ax) w) (list (car cs)) '()))))))))

(define (glgui:stringclipright w txt fnt)
  (glgui:glyphs->string (glgui:stringcliplist w (glgui:string->glyphs txt) fnt)))

(define (glgui:stringclipleft w txt fnt)
  (glgui:glyphs->string (reverse (glgui:stringcliplist w (reverse (glgui:string->glyphs txt)) fnt))))

(define (glgui:draw-text-left x y w h label fnt color)
  (let* ((strw (flo (glgui:stringwidth label fnt)))
         (strh (map flo (glgui:stringheight (string-append label "|") fnt)))
         (centery (fl+ (flo y) (fl/ (if (fl> (flo h) 0.) (flo h) (fl- (car strh) (cadr strh))) 2.) 
                       (fl- (fl/ (fl+ (car strh) (cadr strh)) 2.)))))
    (glgui:renderstring x centery
       (if (fl> strw (flo w)) (glgui:stringclipright w label fnt) label) fnt color)))

(define (glgui:draw-text-right x y w h label fnt color)
  (let* ((strw (flo (glgui:stringwidth label fnt)))
         (strh (map flo (glgui:stringheight (string-append label "|") fnt)))
         (centery (fl+ (flo y) (fl/ (if (fl> (flo h) 0.) (flo h) (fl- (car strh) (cadr strh))) 2.) 
           (fl- (fl/ (fl+ (car strh) (cadr strh)) 2.)))))
    (glgui:renderstring (if (fl> strw (flo w)) x (fl+ (flo x) (flo w) (fl- strw))) centery
       (if (fl> strw (flo w)) (glgui:stringclipleft w label fnt) label) fnt color)))

(define (glgui:draw-text-center x y w h label fnt color . clipright)
  (let* ((strw (flo (glgui:stringwidth label fnt)))
         (strh (map flo (glgui:stringheight (string-append label "|") fnt)))
         (centery (fl+ (flo y) (fl/ (if (fl> (flo h) 0.) (flo h) (fl- (car strh) (cadr strh))) 2.) 
                               (fl- (fl/ (fl+ (car strh) (cadr strh)) 2.))))
         (clipper (if (and (fx= (length clipright) 1) (car clipright)) 
           glgui:stringclipright glgui:stringclipleft)))
    (glgui:renderstring (if (fl> strw (flo w)) x (fl+ (flo x) (fl/ (fl- (flo w) strw) 2.))) centery
       (if (fl> strw (flo w)) (clipper w label fnt) label) fnt color)))

(define (string-split-width str w fnt)
  (if (string-contains str "\n")  
    ;; If there is a new line in the text, call this procedure for each line - section separated by new lines
    (let lloop ((lines (string-split str #\newline)) (strlist (list)))
      (if (fx= (length lines) 0) strlist
         ;; Get results of using this procedure on the line
         (let ((linelist (string-split-width (car lines) w fnt)))
           ;; Handle an empty list (two new lines in a row) by replacing with a list with an empty string
           (lloop (cdr lines) (append strlist (if (fx= (length linelist) 0) '("") linelist))))))
    ;; If no new line, proceed normally - determine words on each line
    (let ((strsplit (string-split str #\ )))
      (let loop ((i 0) (strlist (list)) (newstr "") (newstr_len 0))
        (if (fx= i (length strsplit))
          (if (fx> newstr_len 0) (append strlist (list newstr)) strlist)
          (let* ((buildstr (string-append (list-ref strsplit i) " "))
                 (buildstr_len (fix (glgui:stringwidth buildstr fnt)))
                 (wrap? (fx> (fx+ newstr_len buildstr_len) (fix w))))
            (loop (fx+ i 1) (append strlist (if wrap? (list newstr) '()))
                (string-append (if wrap? "" newstr) buildstr)
                (fx+ buildstr_len (if wrap? 0 newstr_len)))))))
   ))

(define (string-split-width-rtl str w fnt)
  (if (string-contains str "\n")
    ;; If there is a new line in the text, call this procedure for each line - section separated by new lines
    (let lloop ((lines (string-split str #\newline)) (strlist (list)))
      (if (fx= (length lines) 0) strlist
         ;; Get results of using this procedure on the line
         (let ((linelist (string-split-width-rtl (car lines) w fnt)))
           ;; Handle an empty list (two new lines in a row) by replacing with a list with an empty string
           (lloop (cdr lines) (append strlist (if (fx= (length linelist) 0) '("") linelist))))))
    ;; If no new line, proceed normally - determine words on each line
    (let ((strsplit (string-split str #\ )))
      (let loop ((i (- (length strsplit) 1)) (strlist (list)) (newstr "") (newstr_len 0))
        (if (fx= i -1)
          (if (fx> newstr_len 0) (append strlist (list newstr)) strlist)
                                           ;; Don't add a space, if it is the last word and there is no end space
          (let* ((buildstr (string-append (if (fx= i 0) "" " ") (list-ref strsplit i)))
                 (buildstr_len (fix (glgui:stringwidth buildstr fnt)))
                 (wrap? (fx> (fx+ newstr_len buildstr_len) (fix w))))
            (loop (fx- i 1) (append strlist (if wrap? (list newstr) '()))
                (string-append buildstr (if wrap? "" newstr))
                (fx+ buildstr_len (if wrap? 0 newstr_len)))))))
   ))
  
;; eof
