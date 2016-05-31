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

;; Dropdown box shows a list below or above the selected item.
;; The current popped up box and list widgets and their gui, false if none
(define glgui:dropdownbox:box #f)
(define glgui:dropdownbox:list #f)
(define glgui:dropdownbox:gui #f)

(define (glgui:dropdownbox-draw g wgt)
  (let ((x (glgui-widget-get-dyn g wgt 'x))
        (y (glgui-widget-get-dyn g wgt 'y))
        (w (glgui-widget-get-dyn g wgt 'w))
        (h (glgui-widget-get-dyn g wgt 'h))
        (r (glgui-widget-get g wgt 'rounded))
        (bgcolor (glgui-widget-get-dyn g wgt 'bgcolor))
        (arrowcolor (glgui-widget-get g wgt 'arrowcolor))
        (lst (glgui-widget-get g wgt 'list))
        (cur (fix (glgui-widget-get g wgt 'current))))
    ;; Draw background
    (if r
      (glgui:draw-rounded-box x y w h bgcolor)
      (glgui:draw-box x y w h bgcolor)
    )
    ;; Draw selected item, if any
    (if (fx> cur -1)
      (let ((entry (list-ref lst cur))) (entry g wgt x y w h #f))
    )
    ;; Draw down arrow icon
    (glgui:draw-pixmap-center (+ x (- w 20)) y 16 h glgui_dropdownbox_downarrow.img arrowcolor)
  ))

(define (glgui:dropdownbox-input g wgt type mx my)
   (let* ((x (glgui-widget-get-dyn g wgt 'x)) 
          (y (glgui-widget-get-dyn g wgt 'y))
          (w (glgui-widget-get-dyn g wgt 'w))
          (h (glgui-widget-get-dyn g wgt 'h))
          (armed (glgui-widget-get g wgt 'armed))
          (cb (glgui-widget-get g wgt 'callback))
          (bidir (glgui-widget-get g wgt 'bidir))
          (inside (and (> mx x) (< mx (+ x w)) (> my y) (< my (+ y h)))))
     (cond
       ((and (= type EVENT_BUTTON1DOWN) inside)
          (glgui-widget-set! g wgt 'armed #t))
       ((= type EVENT_BUTTON1UP)
          (if (and armed inside)
            ;; Dynamically create list for choosing items
            (let* ((bgcolor (glgui-widget-get g wgt 'bgcolor))
                   (bordercolor (glgui-widget-get g wgt 'bordercolor))
                   (scrollcolor (glgui-widget-get g wgt 'scrollcolor))
                   (scrollw (glgui-widget-get g wgt 'scrollw))
                   (scrollrounded (glgui-widget-get g wgt 'scrollrounded))
                   (modal (glgui-widget-get g wgt 'modal))
                   (lst (glgui-widget-get g wgt 'list))
                   (cur (glgui-widget-get g wgt 'current))
                   (maxitems (glgui-widget-get g wgt 'maxitems))
                   ;; If max items set to #f by app, then screen boundaries will cut it off instead
                   (lh (* (if maxitems (min (length lst) maxitems) (length lst)) h))
                   (ly0 (- y lh))
                   ;; Place list above box if it would go offscreen
                   (ly (if (< ly0 0) (+ y h) ly0)))
              ;; Determine if list height and location still need to be adjusted - for a long list
              (if (> (+ ly lh) (glgui-height-get))
                ;; Which direction has more space?
                (let ((upper (- (glgui-height-get) (+ y h))))
                  (if (>= y upper)
                    ;; If more space below, put list below
                    (begin
                      (set! ly (modulo y h))
                      (if bidir
                        (let ((vspace (- (glgui-height-get) ly)))
                          ;; If bidirectional, fit just within the screen height
                          (if (> lh vspace)
                            (set! lh (* (floor (/ vspace h)) h))))
                        ;; Otherwise fit below the box
                        (set! lh (- y ly))))
                    ;; Otherwise, put list above (extend below as well if bidirectional)
                    (let ((lh0 (- upper (modulo upper h))))
                      (if bidir
                        ;; Expand downward as well
                        (let ((diff (- lh lh0)))
                          (if (> diff (+ y h))
                            ;; Not enough room for full height
                            (begin
                              (set! ly (modulo y h))
                              (set! lh (+ lh0 h (- y ly))))
                            ;; Full desired height
                            (set! ly (- (+ y h) diff))))
                        ;; Otherwise just show above
                        (set! lh lh0))
                    )
                  )
                ))
              ;; Invisible box covering everything
              (let* ((boxw (glgui-box g 0 0 (glgui-width-get) (glgui-height-get) (color-fade Black 0)))
                     ;; list callback - get selected item when clicked, delete box and list
                     (listcb
                       (lambda (g w ltype lmx lmy)
                         (glgui-widget-set! g wgt 'current (glgui-widget-get g w 'current))
                         (glgui-widget-delete g boxw)
                         (glgui-widget-delete g w)
                         (set! glgui:dropdownbox:box #f)
                         (set! glgui:dropdownbox:list #f)
                         (set! glgui:dropdownbox:gui #f)
                         (if (procedure? cb) (cb g wgt ltype lmx lmy))))
                     (listw (glgui-list g x ly w lh h lst listcb))
                     ;; screen-covering box input handle - delete box and list,
                     ;; return false so that actions taken on other components
                     (boxinput (lambda (g w type mx my)
                         (if (= type EVENT_BUTTON1DOWN)
                            (glgui-widget-set! g w 'armed #t)
                            (if (and (glgui-widget-get g w 'armed) (= type EVENT_BUTTON1UP))
                              (begin
                                (glgui-widget-delete g listw)
                                (glgui-widget-delete g w)
                                (set! glgui:dropdownbox:box #f)
                                (set! glgui:dropdownbox:list #f)
                                (set! glgui:dropdownbox:gui #f))))
                         #f)))
                ;; Set box callback and modal mode
                (glgui-widget-set! g boxw 'input-handle boxinput)
                (glgui-widget-set! g boxw 'modal modal)   
                ;; Set list colors, selected item and modal mode
                (glgui-widget-set! g listw 'autohidebar #t)
                (glgui-widget-set! g listw 'current cur)
                (glgui-widget-set! g listw 'modal modal)
                (glgui-widget-set! g listw 'bgcol1 bgcolor)
                (glgui-widget-set! g listw 'bgcol2 bgcolor)
                (glgui-widget-set! g listw 'bordercolor bordercolor)
                (glgui-widget-set! g listw 'scrollcolor scrollcolor)
                (glgui-widget-set! g listw 'scrollw scrollw)
                (glgui-widget-set! g listw 'scrollrounded scrollrounded)
                ;; Scroll if necessary based on selected item
                (if (> (* cur h) lh)
                  ;; Put selected item just off bottom
                  (glgui-widget-set! g listw 'offset (fix (round (/ (- (* cur h) lh) h)))))
                ;; Remember the popped up components, so apps have access to them
                (set! glgui:dropdownbox:box boxw)
                (set! glgui:dropdownbox:list listw)
                (set! glgui:dropdownbox:gui g))))
          (glgui-widget-set! g wgt 'armed #f))
     )
  inside
))

(define (glgui:dropdown-box-remove-box)
  ;; Remove the popped up dropdown box list and invisible box behind it
  (if (and glgui:dropdownbox:box glgui:dropdownbox:list glgui:dropdownbox:gui) (begin
    (glgui-widget-delete glgui:dropdownbox:gui glgui:dropdownbox:box)
    (glgui-widget-delete glgui:dropdownbox:gui glgui:dropdownbox:list)
    (set! glgui:dropdownbox:box #f)
    (set! glgui:dropdownbox:list #f)
    (set! glgui:dropdownbox:gui #f))
  )
)

(define (glgui-dropdownbox g x y w h lst arrowcolor bgcolor bordercolor)
  (glgui-widget-add g
     'x x
     'y y
     'w w 
     'h h
     'rounded #f
     'callback #f
     'armed #f
     'hidden #f
     'list lst
     'maxitems 5    ;; Max number of items to make visible at once
     'bidir #f      ;; If set to true, may expand in both directions
     'current -1
     'focus #f
     'arrowcolor arrowcolor
     'bgcolor bgcolor
     'bordercolor bordercolor
     'scrollcolor DimGray
     'scrollw 5.
     'scrollrounded #f
     'draw-handle  glgui:dropdownbox-draw
     'input-handle glgui:dropdownbox-input
  ))

;;eof
