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

(define (glgui:draw-line x1 y1 x2 y2 color . width)
  (let* ((ht (fl* 0.5 (if (null? width) 1. (flo (car width)))))
         (dx (flo (- x1 x2)))
         (dy (flo (- y1 y2)))
         (dr (flsqrt (fl+ (fl* dx dx) (fl* dy dy))))
         (d1x (if (fl> dr 0.) (fl/ (fl* ht dy) dr) 0.))
         (d1y (if (fl> dr 0.) (fl/ (fl* ht (fl- dx)) dr) 0.))
         (d2x (fl- d1x))
         (d2y (fl- d1y)))
    (glCoreColor color)
    (glCoreBegin GL_TRIANGLES)
    (glCoreVertex2f (flo (+ x1 d1x)) (flo (+ y1 d1y)))
    (glCoreVertex2f (flo (+ x1 d2x)) (flo (+ y1 d2y)))
    (glCoreVertex2f (flo (+ x2 d2x)) (flo (+ y2 d2y)))
    (glCoreVertex2f (flo (+ x2 d2x)) (flo (+ y2 d2y)))
    (glCoreVertex2f (flo (+ x2 d1x)) (flo (+ y2 d1y)))
    (glCoreVertex2f (flo (+ x1 d1x)) (flo (+ y1 d1y)))
    (glCoreEnd)))

(define (glgui:draw-linestrip data color . width)
  (if (and (pair? data) (fx< (length data) 200)) ;; FIXME: pass the length in as parameter
      (let loop ((pts data))
        (or (null? pts) ;; length 0
            (null? (cdr pts)) ;; length 1
            (begin
              (apply glgui:draw-line (append (car pts) (cadr pts) (list color) width))
              (loop (cdr pts)))))))

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
        (glCoreTextureGradientDraw rx (+ ry 6.5) 6 (- rh 13.5) glgui:box .1 .1 .9 .9 0. (list (car c) (car c) (caddr c) (caddr c)))
        (glCoreColor (car c))
        (glCoreTextureDraw rx (+ ry (- rh 7)) 6 7 glgui_rounded_corner.raw .1 .9 .9 .1 0.)
        ;; Middle
        (glCoreTextureGradientDraw (+ rx 6) (+ ry 0.3) (- rw 11.5) (- rh 0.6) glgui:box .1 .1 .9 .9 0. c)
        ;; Right side
        (glCoreColor (cadddr c))
        (glCoreTextureDraw (+ rx (- rw 6)) ry 6 7 glgui_rounded_corner.raw .9 .1 .1 .9 0.)
        ;; Only use right side top and bottom colours
        (glCoreTextureGradientDraw (+ rx (- rw 6)) (+ ry 6.5) 6 (- rh 13.5) glgui:box .1 .1 .9 .9 0. (list (cadr c) (cadr c) (cadddr c) (caddr c)))
        (glCoreColor (cadr c))
        (glCoreTextureDraw (+ rx (- rw 6)) (+ ry (- rh 7)) 6 7 glgui_rounded_corner.raw .9 .9 .1 .1 0.))
      (let ((rx (fix x))
            (ry (fix y))
            (rw (fix w))
            (rh (fix h)))
        ;; Left side
        (glCoreTextureDraw rx ry 6 7 glgui_rounded_corner.raw .1 .1 .9 .9 0.)
        (glCoreTextureDraw rx (+ ry 6.5) 6 (- rh 13.5) glgui:box .1 .1 .9 .9 0.)
        (glCoreTextureDraw rx (+ ry (- rh 7)) 6 7 glgui_rounded_corner.raw .1 .9 .9 .1 0.)
        ;; Middle
        (glCoreTextureDraw (+ rx 6) (+ ry 0.3) (- rw 11.5) (- rh 0.6) glgui:box .1 .1 .9 .9 0.)
        ;; Right side
        (glCoreTextureDraw (+ rx (- rw 6)) ry 6 7 glgui_rounded_corner.raw .9 .1 .1 .9 0.)
        (glCoreTextureDraw (+ rx (- rw 6)) (+ ry 6.5) 6 (- rh 13.5) glgui:box .1 .1 .9 .9 0.)
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

;;draw a pixmap centered inside a box bw x bh with size nw x nh. set nw x h to #f for no scaling or either one for original ratio
(define (glgui:draw-pixmap-center-stretch x y bw bh nw nh img color)
  (let* ((sw (car img))
         (sh (cadr img))
         (scx (if nw (/ nw sw) (if nh (/ nh sh) 1))) ;;scale 
         (scy (if nh (/ nh sh) scx))
         (iw (if nw nw (fix (* sw scx))))
         (ih (if nh nh (fix (* sh scy))))
         (x0 (+ x (* (- bw iw)  0.5)))
         (y0 (+ y (* (- bh ih)  0.5))))
  (glCoreColor color)
  (apply glCoreTextureDraw (append (list x0 y0 iw ih) (cddr img) (list 0.)))))

;; ----------
;; strings (aka pixmap lists)

(define glgui:image-w car)
(define glgui:image-h cadr)
(define glgui:glyph-image cadr)

;; support both legacy latex and new truetype rendering
;; eventually this should be stripped out for better performance
(define (glgui:glyph-offsetx g)
  ;; FIXME: avoid length in tight loops and frequently run code for
  ;; being O(n), if at all possible.
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

;;; BEGIN CODE TRANSITION
;;;
;;; Reducing the computational complexity of font operations improves
;;; rendering time.

(define-type ln-ttf:glyph
  macros: prefix: macro-
  desc ;; for now legacy: (key (texcoord1..4) X Y Z)
  width
  height
  texture
  texcoords ;; generic 4 element vector of flownums
  rect-texcoords ;; 4x2 element f32vector
  ;; order is sorta important here
  offsetx
  advancex
  offsety
  )

(define (ttf:glyph? obj) (macro-ln-ttf:glyph? obj))
(define (ttf:glyph-desc obj) (macro-ln-ttf:glyph-desc obj))
(define (ttf:glyph-width obj) (macro-ln-ttf:glyph-width obj))
(define (ttf:glyph-height obj) (macro-ln-ttf:glyph-height obj))
(define (ttf:glyph-image obj) (macro-ln-ttf:glyph-texture obj))
(define (ttf:glyph-texcoords obj) (macro-ln-ttf:glyph-texcoords obj))
(define (ttf:glyph-rect-texcoords obj) (macro-ln-ttf:glyph-rect-texcoords obj))
(define (ttf:glyph-offsetx obj) (macro-ln-ttf:glyph-offsetx obj))
(define (ttf:glyph-advancex obj) (macro-ln-ttf:glyph-advancex obj))
(define (ttf:glyph-offsety obj) (macro-ln-ttf:glyph-offsety obj))

(define-type ln-ttf:font
  macros: prefix: macro-
  desc ;; for now the legacy description of a font as a assoc-list
  char->desc-table
  )

(define (ln-ttf:font? obj) (macro-ln-ttf:font? obj))

(define find-font/desc)

(define (ln-ttf:font-ref font char) ;; -> glyph
  (cond
    ((macro-ln-ttf:font? font) ;; TBD: leave this as the only case
      (table-ref (macro-ln-ttf:font-char->desc-table font) char #f))
    (else
      (ln-ttf:font-ref (find-font font) char))))

(define (make-ln-ttf:font/desc fnt)
  (let ((font-table (list->table
                     (let ((double-the-key (lambda (x) (cons (car x) x))))
                       ;; TBD: get rid of the doubling - requires changes to call sites
                       (map double-the-key fnt)))))
    (macro-make-ln-ttf:font fnt font-table)))

(define find-font
  ;; TBD consider amount of fonts and decide on font cache data
  ;; structure.  Now assq-list; good for rather short lists, which
  ;; are in line with good user interface design.
  (let ((by-desc '() #;(make-table hash: eq?-hash))
        (assq assq))
    (define (%find-font/desc fnt)
      (let ((hit (assq fnt by-desc) #;(table-ref by-desc fnt #f)))
        (if hit (cdr hit) #; hit
            (let ((font (make-ln-ttf:font/desc fnt)))
              #;(table-set! by-desc fnt font)
              (set! by-desc (cons (cons fnt font) by-desc))
              font))))
    (define (find-font fnt)
      (cond
        ((macro-ln-ttf:font? fnt) ;; new style
          ;; TBD: after transition phase warn if this case is hit.
          fnt)
        ((and (pair? fnt) (pair? (car fnt))) ;; likely a legacy font
          (%find-font/desc fnt))
        (else
          (error "illegal font" fnt))))
    (set! find-font/desc %find-font/desc)
    find-font))

(define (install-font-cache-backward-compatible-override!)
  (let ((assoc.orig assoc))
    ;; Backward compatibility: try to transparently hook into legacy lookups.
    ;; TBD: 2020-08-23 This is an intermediate workaround to be removed ASAP.
    (define (transparent-font-assoc k coll)
      (cond
        ((macro-ln-ttf:font? coll)
          ;; TBD: after experimental phase warn if this case is hit.
          (ln-ttf:font-ref coll k))
       (else
         ;; TBD: after transition phase error out if this case is hit.
         (assoc.orig k coll))))
    (set! assoc transparent-font-assoc)))

(install-font-cache-backward-compatible-override!)
;;; End OF TRANSITIONAL CODE

(define (glgui:renderstring x y txt fnt color)
  (glCoreColor color)
  (let loop ((x0 (flo x)) (cs (glgui:string->glyphs txt)))
    (if (pair? cs)
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
  (cond
   ((macro-ln-ttf:font? fnt) (ttf:glyph-height (MATURITY+1:ln-ttf:font-ref fnt 0)))
   ((find-font fnt) => glgui:fontheight)
   (else ;; MATURITY -1 backward compatible, the old code
    (let* ((g (assoc 0 fnt))
           (i (if g (glgui:glyph-image g) #f))
           (h (if i (glgui:image-h i)
                  (cadr (cadr (car fnt)))))) h))))

(define (glgui:stringheight txt fnt)
  (define font (find-font fnt))
  (let loop ((above 0.) (below 0.) (cs (glgui:string->glyphs txt)))
    (if (null? cs) (list above below)
      (let* ((g (ln-ttf:font-ref font (car cs)))
             (i (if g (glgui:glyph-image g) #f))
             (gh (if i (flo (glgui:image-h i)) 0.))
             (goy (if g (flo (glgui:glyph-offsety g)) 0.)))
        (loop (flmax above goy)  (flmin below (fl- goy gh)) (cdr cs))))))

; returns a fixnum width of the string, rounded up
(define (glgui:stringwidth txt fnt)
  (let ((cs (glgui:string->glyphs txt))
        (fnt (find-font fnt)))
    (let loop ((x 0.) (cs cs))
      (if (null? cs)
          (fix (ceiling x))
          (let* ((key (car cs))
                 (glyph (ln-ttf:font-ref fnt key))
                 (ax (if glyph (flo (glgui:glyph-advancex glyph)) 0.)))
            (loop (fl+ x ax) (cdr cs)))))))

; returns a list of floats widths of the glyphs in the string
(define (glgui:stringwidth-lst txt fnt)
  (define font (find-font fnt))
  (let loop ((cs (glgui:string->glyphs txt)) (ret '()))
    (if (null? cs) ret
      (let* ((glyph (ln-ttf:font-ref font (car cs)))
             (ax (if glyph (flo (glgui:glyph-advancex glyph)) 0.)))
        (loop (cdr cs) (append ret (list ax)))))))

(define (glgui:stringcliplist w0 txtlst fnt)
  (let ((w (flo w0)))
    (let loop ((x 0.) (cs txtlst) (rs '()))
      (if (or (null? cs) (fl> x w)) rs
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

(define (glgui:draw-text-right x y w h label0 fnt color)
  (let* ((strw0 (flo (glgui:stringwidth label0 fnt)))
         (label (if (fl> strw0 (flo w)) (glgui:stringclipleft w label0 fnt) label0))
         (strw (if (fl> strw0 (flo w)) (flo (glgui:stringwidth label fnt)) strw0))
         (strh (map flo (glgui:stringheight (string-append label "|") fnt)))
         (centery (fl+ (flo y) (fl/ (if (fl> (flo h) 0.) (flo h) (fl- (car strh) (cadr strh))) 2.)
           (fl- (fl/ (fl+ (car strh) (cadr strh)) 2.)))))
    (glgui:renderstring (fl+ (flo x) (flo w) (fl- strw)) centery label fnt color)))

(define (glgui:draw-text-center x y w h label fnt color . clipright)
  ;; looks like a case of "premature optimization is the root of all evil"
  (let* ((strw-raw (glgui:stringwidth label fnt)) ;; fixnum
         ;; `strh` list instead of two values (top bottom) relative to baseline
         (strh (map flo (glgui:stringheight (string-append label "|") fnt)))
         (first-char-height (car strh))
         (second-char-height (cadr strh))
         (h-flo (flo h))
         (y-flo (flo y)) ;; just used once (so far) for symmetry
         (centery ;; careful to have floating point only.
          ;; This might be "premature optimization".  The contributes
          ;; next to nothing to the cumulative time
          ;; `draw-text-center` needs.
          (fl+ y-flo
               (fl/ (if (fl> h-flo 0.)
                        h-flo
                        (fl- first-char-height second-char-height))
                    2.)
               (fl- (fl/ (fl+ first-char-height second-char-height) 2.))))
         (clipper
          (let ((clipright (and (pair? clipright) (car clipright))))
            (if clipright glgui:stringclipright glgui:stringclipleft))))
    (let ((strw-flo (flo strw-raw))  ; with of the string
          (w-flo (flo w)))
      (glgui:renderstring
       ;; FIXME: why enforce conversion to to flow in the first place?
       (if (fl> strw-flo w-flo) ;; needs full width
           x ;; start at left otherwise at half of empty space
           (fl+ (flo x) (fl/ (fl- w-flo strw-flo) 2.)))
       centery
       (if (fl> strw-flo w-flo) (clipper w label fnt) label)
       fnt color))))


(define (string-split-width str w fnt)
  (if (string-contains str "\n")
    ;; If there is a new line in the text, call this procedure for each line - section separated by new lines
    (let lloop ((lines (string-split str #\newline)) (strlist (list)))
      (if (null? lines) strlist
         ;; Get results of using this procedure on the line
         (let ((linelist (string-split-width (car lines) w fnt)))
           ;; Handle an empty list (two new lines in a row) by replacing with a list with an empty string
           (lloop (cdr lines) (append strlist (if (null? linelist) '("") linelist))))))
    ;; If no new line, proceed normally - determine words on each line
    (let* ((strsplit (string-split str #\ ))
           (strsplit-len (length strsplit))
           (lastspace (and (fx> (string-length str) 0) (char=? (string-ref str (- (string-length str) 1)) #\ ))))
      (let loop ((i 0) (strlist (list)) (newstr "") (newstr_len 0))
        (if (fx= i strsplit-len)
          (if (fx> newstr_len 0) (append strlist (list newstr)) strlist)
          (let* ((buildstr (string-append (list-ref strsplit i) (if (and (not lastspace) (fx= i (- strsplit-len 1))) "" " ")))
                 (buildstr_len (glgui:stringwidth buildstr fnt))
                 (wrap? (fx> (fx+ newstr_len buildstr_len) (fix w))))
            (if (and wrap? (fx> (fx+ buildstr_len) (fix w)))
              ;; Special case where a single word is too long to fit on a line, cut word up
              (let* ((prevline (if (fx> (string-length newstr) 0) (list newstr) (list)))
                     (cutword (string-split-width-break buildstr (fix w) fnt))
                     (lastindex (- (length cutword) 1))
                     (lines (append prevline (list-head cutword lastindex)))
                     (nextstr (list-ref cutword lastindex)))
                (loop (fx+ i 1) (append strlist lines) nextstr (glgui:stringwidth nextstr fnt)))
              (loop (fx+ i 1) (append strlist (if wrap? (list (if (fx> (string-length newstr) 0) (string-append (substring newstr 0 (fx- (string-length newstr) 1)) "\n") "")) '()))
                  (string-append (if wrap? "" newstr) buildstr)
                  (fx+ buildstr_len (if wrap? 0 newstr_len))))))))
   ))

(define (string-split-width-break str w fnt)
  (let* ((ws (glgui:stringwidth-lst str fnt))
         (stindex (- (length ws) 1)))
    (let loop ((wleft (sum ws)) (i stindex))
       (if (and (fx> (fix (ceiling wleft)) w) (fx> i 0))
         (loop (- wleft (list-ref ws (- i 1))) (- i 1))
         (let* ((buildstr (substring str 0 i))
                (buildstr_len (glgui:stringwidth buildstr fnt))
                (nextstr (substring str i (string-length str)))
                (nextstr_len (glgui:stringwidth nextstr fnt)))
           (append (list buildstr) (if (and (fx> nextstr_len w) (fx> (string-length nextstr) 1)) (string-split-width-break nextstr w fnt) (list nextstr)))))))
)

(define (string-split-width-rtl str w fnt)
  (if (string-contains str "\n")
    ;; If there is a new line in the text, call this procedure for each line - section separated by new lines
    (let lloop ((lines (string-split str #\newline)) (strlist (list)))
      (if (null? lines) strlist
         ;; Get results of using this procedure on the line
         (let ((linelist (string-split-width-rtl (car lines) w fnt)))
           ;; Handle an empty list (two new lines in a row) by replacing with a list with an empty string
           (lloop (cdr lines) (append strlist (if (null? linelist) '("") linelist))))))
    ;; If no new line, proceed normally - determine words on each line
    (let ((strsplit (string-split str #\ )))
      (let loop ((i (- (length strsplit) 1)) (strlist (list)) (newstr "") (newstr_len 0))
        (if (fx= i -1)
          (if (fx> newstr_len 0) (append strlist (list newstr)) strlist)
                                           ;; Don't add a space, if it is the last word and there is no end space
          (let* ((buildstr (string-append (if (fx= i 0) "" " ") (list-ref strsplit i)))
                 (buildstr_len (glgui:stringwidth buildstr fnt))
                 (wrap? (fx> (fx+ newstr_len buildstr_len) (fix w))))
            (if (and wrap? (fx> (fx+ buildstr_len) (fix w)))
              ;; Special case where a single word is too long to fit on a line, cut word up
              (let* ((prevline (if (fx> (string-length newstr) 0) (list newstr) (list)))
                     (cutword (string-split-width-break-rtl buildstr (fix w) fnt))
                     (lastindex (- (length cutword) 1))
                     (lines (append prevline (list-head cutword lastindex)))
                     (nextstr (list-ref cutword lastindex)))
                (loop (fx- i 1) (append strlist lines) nextstr (glgui:stringwidth nextstr fnt)))
              (loop (fx- i 1) (append strlist (if wrap? (list newstr) '()))
                  (string-append buildstr (if wrap? "" newstr))
                  (fx+ buildstr_len (if wrap? 0 newstr_len))))))))
   ))

(define (string-split-width-break-rtl str w fnt)
  (let* ((ws (glgui:stringwidth-lst str fnt))
         (stindex (- (length ws) 1)))
    (let loop ((wright (sum ws)) (i 0))
       (if (and (fx> (fix (ceiling wright)) w) (fx< i stindex))
         (loop (- wright (list-ref ws i)) (+ i 1))
         (let* ((buildstr (substring str i (string-length str)))
                (buildstr_len (glgui:stringwidth buildstr fnt))
                (nextstr (substring str 0 i))
                (nextstr_len (glgui:stringwidth nextstr fnt)))
           (append (list buildstr) (if (and (fx> nextstr_len w) (fx> (string-length nextstr) 1)) (string-split-width-break-rtl nextstr w fnt) (list nextstr)))))))
)

;; eof
