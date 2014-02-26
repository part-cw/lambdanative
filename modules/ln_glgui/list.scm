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
;; generic list widget

(define (glgui:list-draw g wgt)
  (let* ((x (flo (glgui-widget-get-dyn g wgt 'x)))
         (y (flo (glgui-widget-get-dyn g wgt 'y)))
         (w (flo (glgui-widget-get-dyn g wgt 'w)))
         (h (flo (glgui-widget-get-dyn g wgt 'h)))
         (lst (glgui-widget-get g wgt 'list))
         (bg1 (glgui-widget-get g wgt 'bgcol1))
         (bg2 (glgui-widget-get g wgt 'bgcol2))
         (bdc (glgui-widget-get g wgt 'bordercolor))
         (dh (flo (glgui-widget-get g wgt 'dh)))
         (n (fix (floor (/ h dh))))
         (flofs (flo (glgui-widget-get g wgt 'offset)))
         (ofs (fix flofs))
         (deltay (fl* (fl- flofs (flo ofs)) dh))
         (cur (fix (glgui-widget-get g wgt 'current)))
         (autohide (glgui-widget-get g wgt 'autohidebar))
         (nlist (length lst))
         (allvis (fx<= nlist n))     ;; Determine if all items visible to see if we need scrollbar
         (showscroll? (or (not autohide) (not allvis))))

   (glCoreClipPush x y (fl+ x w) (fl+ y h))

   (glCoreColor White)
   
   ;; Draw a border around the component and within items if colour specified
   ;; Note that this adds one pixel around the coordinates set
   (if bdc (glgui:draw-box (fl- x 1.) (fl- y 1.) (fl+ w 2.) (fl+ h 2.) bdc))

   ;; Draw each item in the list
   (let ((nadj (if (fl= deltay 0.) n (fx+ n 1)))
         (hadj (if (fl= deltay 0.) 0. (fl- dh)))
         (iadj (if (fl= deltay 0.) 0 -1)))
     (let loop ((i iadj)(y0 (fl+ y (fl/ (modulo h dh) 2.) deltay hadj)))
       (if (fx< i nadj)
         (let* ((idx (fx+ ofs (fx- n i 1)))
                (filled? (and (fx>= idx 0) (fx< idx (length lst))))
                (selected? (and filled? (fx= idx cur)))
                (entry (if filled? (list-ref lst idx) #f))
                (bgcolor (if (fx= (modulo idx 2) 0) bg1 bg2))
                (bx x) (by (fl+ y0 1.)) 
                (bw (if showscroll? (fl- w 8.) w)) (bh (fl- dh 2.0))
                )
           ;; Only draw background rectangle for entry, if there is a background colour
           (if bgcolor (glgui:draw-box bx by bw bh bgcolor))
           (if (and filled? (procedure? entry))
             (entry g wgt bx by bw bh selected?)
           )
           (loop (fx+ i 1) (fl+ y0 dh)))))
    )

   ;; Draw scrollbar if not auto-hidden or scrolling needed
   (if showscroll?
     (let* ((sw 5.) (sx (fl+ x w -6.))
            (sh (if allvis h (fl* h (fl/ (flo n) (flo nlist)))))
            (sy (fl- (fl+ y h)  (if (fl= sh h) 0. (fl* (fl- h sh) (fl/ flofs (fl- (flo nlist) (flo n))))) sh)))
       (glgui:draw-box sx sy sw sh DimGray)
   ))

   (glCoreClipPop)

))

(define (glgui:list-input g wgt type mx my)
  (let* ((x (fix (glgui-widget-get-dyn g wgt 'x)))
         (y (fix (glgui-widget-get-dyn g wgt 'y)))
         (w (fix (glgui-widget-get-dyn g wgt 'w)))
         (h (fix (glgui-widget-get-dyn g wgt 'h)))
         (cb (glgui-widget-get g wgt 'callback))
         (dh (fix (glgui-widget-get g wgt 'dh)))
         (n  (fix (floor (/ h dh))))
         (lst (glgui-widget-get g wgt 'list))
         (val (flo (glgui-widget-get g wgt 'offset)))
         (valmin 0.)
         (valmax (flmax 0. (fl- (flo (length lst)) (flo n))))
         (old (glgui-widget-get g wgt 'old))
         (focus (glgui-widget-get g wgt 'focus))
         (drag (glgui-widget-get g wgt 'drag))
         (oldy (fix (glgui-widget-get g wgt 'oldy)))
         (fsty (fix (glgui-widget-get g wgt 'fsty)))
         (space (fl/ (flo (modulo h dh)) 2.))
         (inside (and (fx> (fix mx) x) (fx< (fix mx) (fx+ x w)) (fx> (fix my) y) (fx< (fix my) (fx+ y h)))))
     (cond
       ((and (fx= type EVENT_MOTION) old) ;; drag
         (if (> (fxabs (fx- (fix my) fsty)) 5) 
           (let* ((deltay (flo (fx- oldy (fix my))))
                  (ofssmall (flmax valmin (flmin valmax (fl- val (fl/ deltay (flo dh))))))
                  (ofslarge (flmax valmin (flmin valmax (fl- val (fl* (fl- valmax valmin) (fl/ deltay (flo h)) 0.5))))))
             (if (fl< (flabs deltay) (fl/ (flo dh) 2.))
               (glgui-widget-set! g wgt 'offset ofssmall)
               (glgui-widget-set! g wgt 'offset (if (fl< deltay 0.) (flmax ofssmall ofslarge) (flmin ofssmall ofslarge)))
             )
             (glgui-widget-set! g wgt 'oldy my)
             (glgui-widget-set! g wgt 'drag #t)
             )))
       ((and inside (fx= type EVENT_BUTTON1DOWN)) ;; touch down
         (glgui-widget-set! g wgt 'oldy my)
         (glgui-widget-set! g wgt 'fsty my)
         (glgui-widget-set! g wgt 'old #t)
         (glgui-widget-set! g wgt 'drag #f)
       )
       ((and old (fx= type EVENT_BUTTON1UP)) ;; touch release
         (if (not (integer? val))
           (glgui-widget-set! g wgt 'offset (flfloor (fl+ val 0.5))) ;; snap to place
         )
         (if (and (not drag) inside cb)
           (let ((cur (fix (fl+ val (fl/ (fl- (fl+ (flo y) (flo h)) (fl+ (flo my) space)) (flo dh)) 0.))))
             (if (and (fx>= cur 0) (fx>= cur (fix val)) (fx< cur (length lst))) (begin
               (glgui-widget-set! g wgt 'current cur)
               (if (procedure? cb) (cb g wgt type mx my))
            ))))
         (glgui-widget-set! g wgt 'old #f)
         (glgui-widget-set! g wgt 'drag #f)
       )
       ((and focus (fx= type EVENT_KEYRELEASE)) ;; key pressed
         (let ((kcb (glgui-widget-get g wgt 'key_callback))
               (index (glgui-widget-get g wgt 'current)))
           (if (and kcb index) (kcb mx index)))
       )
       (else (if (not inside) (glgui-widget-set! g wgt 'old #f)))
    )
  inside
))

(define (glgui:list-update g wgt id val)
  (if (eqv? id 'list)
    ;; When the list content is changed
    (let* ((h (flo (glgui-widget-get-dyn g wgt 'h)))
           (dh (flo (glgui-widget-get g wgt 'dh)))
           (n (fix (floor (/ h dh))))
           (ofs (fix (glgui-widget-get g wgt 'offset)))
           (nlist (length val))
           (maxofs (max 0 (- nlist n))))
      ;; It may be shorter than before, so make sure the offset isn't too high
      ;; Fixes the problem where empty lines appear at the bottom when the list becomes shorter instead of auto-scrolling up
      (if (fx> ofs maxofs)
        (glgui-widget-set! g wgt 'offset maxofs))))
)
  
(define (glgui-list g x y w h dh lst cb . kcb)
  (glgui-widget-add g
     'x x
     'y y
     'w w
     'h h
     'callback cb
     'update-handle glgui:list-update
     'key_callback (if (fx= (length kcb) 1) (car kcb) #f)
     'draw-handle  glgui:list-draw
     'input-handle glgui:list-input 
     'hidden #f
     'list lst
     'offset 0.
     'fxoffset 0
     'current -1
     'focus #t
     'dh dh        ;; height of line
     'old  #f
     'drag #f
     'autohidebar #f    ;; Hide scrollbar when list not long enough to need scrolling
     'oldy  0
     'fsty  0
     'bordercolor #f
     'bgcol1 (color-shade White 0.2)
     'bgcol2 (color-shade White 0.25)
   ))

(define (glgui-list-reset g wgt newlist)
  (glgui-widget-set! g wgt 'list newlist)
  (glgui-widget-set! g wgt 'current -1)
  (glgui-widget-set! g wgt 'offset 0)
)

;; eof
