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

;; OpenGl graph backend

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; primitives

(define (glgraph:vrenderstring x y txt fnt color)
  (glCoreColor color)
  (let loop ((y0 (flo y))(cs (glgui:string->glyphs txt)))
    (if (fx> (length cs) 0)
      (let* ((charcode (car cs))
             (g (assoc charcode fnt))
             (i (if g (glgui:glyph-image g) #f))
             (gh (if i (flo (glgui:image-h i)) 0.))
             (gw (if i (flo (glgui:image-w i)) 0.))
             (gox (if g (flo (glgui:glyph-offsetx g)) 0.))
             (goy (if g (flo (glgui:glyph-offsety g)) 0.))
             (gax (if g (flo (glgui:glyph-advancex g)) 0.)))
          (if (and i (not (fx= charcode 32))) (begin
            (glPushMatrix)
            (glTranslatef (fl+ (flo x) (fl- gh goy)) (fl+ y0 gox) 0.)
            (glRotatef 90. 0. 0. 1.)
            (apply glCoreTextureDraw (append (list 0. 0.) i (list 0.)))
            (glPopMatrix) 
          ))
          (loop (fl+ y0 gax) (cdr cs))))))

(define (glgraph:vtext-center x y label fnt color)
  (let* ((strw (flo (glgui:stringwidth label fnt)))
         (strh (map flo (glgui:stringheight (string-append label "|") fnt)))
         (x1 (fl+ (flo x) (fl/ (fl- (car strh) (cadr strh)) 2.)))
         (y1 (fl- (flo y) (fl/ strw 2.))))
    (glgraph:vrenderstring x1 y1 label fnt color)))

(define (glgraph:vtext-bottom x y label fnt color)
  (let* ((strw (flo (glgui:stringwidth label fnt)))
         (strh (map flo (glgui:stringheight (string-append label "|") fnt)))
         (x1 (fl+ (flo x) (fl/ (fl- (car strh) (cadr strh)) 2.)))
         (y1 (fl- (flo y) 0.)))
    (glgraph:vrenderstring x1 y1 label fnt color)))

(define (glgraph:vtext-top x y label fnt color)
  (let* ((strw (flo (glgui:stringwidth label fnt)))
         (strh (map flo (glgui:stringheight (string-append label "|") fnt)))
         (x1 (fl+ (flo x) (fl/ (fl- (car strh) (cadr strh)) 2.)))
         (y1 (fl- (flo y) strw)))
    (glgraph:vrenderstring x1 y1 label fnt color)))

(define (glgraph:triangle x1 y1 x2 y2 x3 y3 color width)
  (glgui:draw-linestrip (list (list x1 y1) (list x2 y2) (list x3 y3) (list x1 y1)) color width))

(define (glgraph:solidtriangle x1 y1 x2 y2 x3 y3 color . dummy)
  (glCoreColor color)
  (_glCoreTextureBind glgui:box)
  (glCoreBegin GL_TRIANGLES)
  (glCoreVertex2f (flo x1) (flo y1) 0.1 0.1)
  (glCoreVertex2f (flo x2) (flo y2) 0.1 0.9)
  (glCoreVertex2f (flo x3) (flo y3) 0.9 0.9)
  (glCoreEnd)
)

(define (glgraph:box x y w h color width)
  (glgui:draw-linestrip (list (list x y) (list x (+ y h)) (list (+ x w) (+ y h)) (list (+ x w) y) (list x y)) color width))

(define (glgraph:solidbox x y w h color . dummy)
  (glCoreColor color)
  (_glCoreTextureBind glgui:box)
  (glCoreBegin GL_TRIANGLE_STRIP)
  (glCoreVertex2f (flo x) (flo (+ y h)) 0.1 0.9)
  (glCoreVertex2f (flo (+ x w)) (flo (+ y h)) 0.9 0.9)
  (glCoreVertex2f (flo x) (flo y) 0.1 0.1)
  (glCoreVertex2f (flo (+ x w)) (flo y) 0.9 0.1)
  (glCoreEnd)
)

(define (glgraph:circle x y r astep color width)
  (let ((pts (let loop ((a 0.)(res '()))
    (if (>= a (* 2. 3.14159)) res
      (loop (+ a astep) (append res (list (list
        (flo (+ x (* r (cos a)))) (flo (+ y (* r (sin a))))))))))))
    (glgui:draw-linestrip pts color width)))

(define (glgraph:solidcircle x y r astep color . dummy)
  (glCoreColor color)
  (_glCoreTextureBind glgui:box)
  (glCoreBegin GL_TRIANGLE_FAN)
  (glCoreVertex2f (flo x) (flo y) 0.5 0.5)
  (let loop ((a 0.))
    (if (< a (* 2. 3.14159)) (begin
      (glCoreVertex2f (flo (+ x (* r (cos a)))) (flo (+ y (* r (sin a))))
                      (flo (+ 0.5 (* 0.4 (cos a)))) (flo (+ 0.5 (* 0.4 (sin a)))))
      (loop (+ a astep)))))
  (glCoreEnd))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define (glgraph:hook g key . values)
  (case key
    ((PRELUDE) #f)
    ((POSTLUDE) #f)
    ((DASH) #f)
    ((LINEWIDTH) 
       (table-set! g 'line-width (car values)))
    ((RGBCOLOR)
      (let* ((bvalues (map (lambda (s) (* 255. s)) values))
             (rd (car bvalues)) (gn (cadr bvalues)) (bl (caddr bvalues)) (al (cadddr bvalues))
             (color  (color-rgba rd gn bl al)))
        (table-set! g 'pen-color color)
    ))
    ((CLOSEPATH)
      (let* ((segments (table-ref g 'path)))
        (if (> (length segments) 1)
          (let* ((s2 (car segments))
                 (s1 (car (reverse segments)))
                 (x1 (caddr s1)) (y1 (cadddr s1))
                 (x2 (car s2))   (y2 (cadr s2)))
            (table-set! g 'path (append segments (list
                     (list x1 y1 x2 y2))))))))
   ((STROKE)
      (let loop ((segments (table-ref g 'path)))
        (if (> (length segments) 0)
          (let* ((segm (car segments))
                 (x1 (list-ref segm 0))
                 (y1 (list-ref segm 1))
                 (x2 (list-ref segm 2))
                 (y2 (list-ref segm 3))
                 (color (table-ref g 'pen-color #f))
                 (line-width (table-ref g 'line-width 1.)))
             (glgui:draw-line (flo x1) (flo y1) (flo x2) (flo y2) color line-width)
             (loop (cdr segments)))))
       (table-set! g 'path '()))
    ((DEVMOVE) #f)
    ((DEVLINE) 
       (let ((oldpath (table-ref g 'path)))
             (table-set! g 'path (append oldpath (list
                (list (table-ref g 'devxpen) (table-ref g 'devypen)
                      (car values) (cadr values)))))))
    ((LINE) 
       (let* ((x1 (list-ref values 0)) (y1 (list-ref values 1))
              (x2 (list-ref values 2)) (y2 (list-ref values 3))
              (dx1 (graphout:curx->devx g x1)) (dy1 (graphout:cury->devy g y1))
              (dx2 (graphout:curx->devx g x2)) (dy2 (graphout:cury->devy g y2))
              (color (table-ref g 'pen-color #f))
              (line-width (table-ref g 'line-width 1.)))
         (glgui:draw-line (flo dx1) (flo dy1) (flo dx2) (flo dy2) color line-width)
       ))
    ((BOX SOLIDBOX)
       (let* ((x1 (list-ref values 0)) (y1 (list-ref values 1))
              (x2 (list-ref values 2)) (y2 (list-ref values 3))
              (dx1 (graphout:curx->devx g x1)) (dy1 (graphout:cury->devy g y1))
              (dx2 (graphout:curx->devx g x2)) (dy2 (graphout:cury->devy g y2))
              (p (case key ((BOX) glgraph:box)
                           ((SOLIDBOX) glgraph:solidbox)))
              (color (table-ref g 'pen-color #f))
              (line-width (table-ref g 'line-width 1.)))
         (p dx1 dy1 (- dx2 dx1) (- dy2 dy1) color line-width)
      ))
    ((TRIANGLE SOLIDTRIANGLE)
       (let* ((x1 (list-ref values 0)) (y1 (list-ref values 1))
              (x2 (list-ref values 2)) (y2 (list-ref values 3))
              (x3 (list-ref values 4)) (y3 (list-ref values 5))
              (dx1 (graphout:curx->devx g x1)) (dy1 (graphout:cury->devy g y1))
              (dx2 (graphout:curx->devx g x2)) (dy2 (graphout:cury->devy g y2))
              (dx3 (graphout:curx->devx g x3)) (dy3 (graphout:cury->devy g y3))
              (p (case key ((TRIANGLE) glgraph:triangle)
                           ((SOLIDTRIANGLE) glgraph:solidtriangle)))
              (color (table-ref g 'pen-color #f))
              (line-width (table-ref g 'line-width 1.)))
         (p dx1 dy1 dx2 dy2 dx3 dy3 color line-width)
      ))
    ((CIRCLE SOLIDCIRCLE)
       (let* ((x (list-ref values 0)) (y (list-ref values 1))
              (devradius (list-ref values 2))
              (dx (graphout:curx->devx g x)) (dy (graphout:cury->devy g y))
              (p (case key ((CIRCLE) glgraph:circle)
                           ((SOLIDCIRCLE) glgraph:solidcircle)))
              (color (table-ref g 'pen-color #f))
              (line-width (table-ref g 'line-width 1.)))
          (p dx dy devradius 0.1 color line-width)))

    ((MARKERCIRCLE MARKERSOLIDCIRCLE)
       (let* ((x (list-ref values 0)) (y (list-ref values 1))
              (dw (list-ref values 2))
              (dx (graphout:curx->devx g x)) (dy (graphout:cury->devy g y))
              (p (case key ((MARKERCIRCLE) glgraph:circle)
                           ((MARKERSOLIDCIRCLE) glgraph:solidcircle)))
              (color (table-ref g 'pen-color #f))
              (line-width (table-ref g 'line-width 1.)))
          (p dx dy (/ dw 2.) 0.1 color line-width)))
    ((MARKERBOX MARKERSOLIDBOX)
       (let* ((x (list-ref values 0)) (y (list-ref values 1))
              (dw (list-ref values 2))
              (dx (graphout:curx->devx g x)) (dy (graphout:cury->devy g y))
              (p (case key ((MARKERBOX) glgraph:box)
                           ((MARKERSOLIDBOX) glgraph:solidbox)))
              (color (table-ref g 'pen-color #f))
              (line-width (table-ref g 'line-width 1.)))
         (p (- dx (/ dw 2.)) (- dy (/ dw 2.)) dw dw color line-width)
       ))
    ((MARKERTRIANGLE MARKERSOLIDTRIANGLE)
       (let* ((x (list-ref values 0)) (y (list-ref values 1))
              (dw (list-ref values 2))
              (dx (graphout:curx->devx g x)) (dy (graphout:cury->devy g y))
              (p (case key ((MARKERTRIANGLE) glgraph:triangle)
                           ((MARKERSOLIDTRIANGLE) glgraph:solidtriangle)))
              (color (table-ref g 'pen-color #f))
              (line-width (table-ref g 'line-width 1.)))
         (p dx dy (+ dx (/ dw 2)) (+ dy dw) (+ dx dw) dy color line-width)
       ))

    ((FONT) #f)
    ((HTEXTLEFT) 
       (let* ((x (car values)) (y (cadr values)) (text (caddr values))
              (dx (graphout:curx->devx g x)) (dy (graphout:cury->devy g y))
              (fnt (table-ref g 'font ascii_12.fnt)) (color (table-ref g 'pen-color Black))
              (tw (glgui:stringwidth text fnt)) 
              (th (glgui:stringheight text fnt)))
         (glgui:draw-text-center dx (- dy (/ (car th) 2.)) tw (apply + th) text fnt color)
    )) 
    ((HTEXTRIGHT)
        (let* ((x (car values)) (y (cadr values)) (text (caddr values))
              (dx (graphout:curx->devx g x)) (dy (graphout:cury->devy g y))
              (fnt (table-ref g 'font ascii_12.fnt)) (color (table-ref g 'pen-color Black))
              (tw (glgui:stringwidth text fnt)) 
              (th (glgui:stringheight text fnt)))
         (glgui:draw-text-center (- dx tw) (- dy (/ (car th) 2.)) tw (apply + th) text fnt color)
    ))
    ((HTEXTCENTER)
        (let* ((x (car values)) (y (cadr values)) (text (caddr values))
              (dx (graphout:curx->devx g x)) (dy (graphout:cury->devy g y))
              (fnt (table-ref g 'font ascii_12.fnt)) (color (table-ref g 'pen-color Black))
              (tw (glgui:stringwidth text fnt)) 
              (th (glgui:stringheight text fnt)))
         (glgui:draw-text-center (- dx (/ tw 2.)) (- dy (/ (car th) 2.)) tw (apply + th) text fnt color)
    ))
    ((VTEXTTOP VTEXTBOTTOM VTEXTCENTER)
      (let* ((x (car values)) (y (cadr values)) (text (caddr values))
             (dx (graphout:curx->devx g x)) (dy (graphout:cury->devy g y))
             (fnt (table-ref g 'font ascii_12.fnt)) (color (table-ref g 'pen-color Black))
             (p (case key ((VTEXTTOP) glgraph:vtext-top)
                          ((VTEXTBOTTOM) glgraph:vtext-bottom)
                          ((VTEXTCENTER) glgraph:vtext-center))))
        (p dx dy text fnt Black)))
    (else (log-error "glgraph: warning: ignoring key=" key))
  ))

;; --------------------
;; graph widget 

(define (glgraph:graph-draw g wgt)
  (let ((x (glgui-widget-get-dyn g wgt 'x))
        (y (glgui-widget-get-dyn g wgt 'y))
        (w (glgui-widget-get-dyn g wgt 'w))
        (h (glgui-widget-get-dyn g wgt 'h))
        (c (glgui-widget-get g wgt 'color))
        (graph (glgui-widget-get g wgt 'graph)))
  (if graph 
    (graph-output graph 'GRAPH_OGL)
)))

(define (glgraph:graph-input g wgt type mx my)
  (let* ((x (glgui-widget-get-dyn g wgt 'x))
         (y (glgui-widget-get-dyn g wgt 'y))
         (w (glgui-widget-get-dyn g wgt 'w))
         (h (glgui-widget-get-dyn g wgt 'h))
         (cb (glgui-widget-get g wgt 'callback))
         (armed (glgui-widget-get g wgt 'armed))
         (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (cond
       ((and (procedure? cb) inside (= type EVENT_BUTTON1DOWN))
          (glgui-widget-set! g wgt 'armed #t))
       ((and armed (procedure? cb) inside (= type EVENT_BUTTON1UP))
          (glgui-widget-set! g wgt 'armed #f)
          (cb g wgt type mx my)))
  inside
))

(define (glgui-graph g x y w h graph)
  (graph-register 'GRAPH_OGL glgraph:hook)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'callback #f
     'hidden #f
     'armed #f
     'draw-handle  glgraph:graph-draw
     'input-handle glgraph:graph-input
     'graph graph
  ))

;;eof
