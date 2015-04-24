#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
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

(define (glgui-label-local g lx ly lw lh str fnt color . alignl)
  ;; Get the text from the language table
  (let ((text (local-get-text str))
        (alignh (if (fx> (length alignl) 0) (car alignl) #f))
        (alignv (if (fx> (length alignl) 1) (cadr alignl) #f)))
    (if (string=? text "[t]")
      ;; If a texture file instead of a string, use pixmap instead of label
      (let* ((name (eval (with-input-from-string (string-append str (number->string local:index) ".img") (lambda () (read)))))
             (px (if alignh (cond
                              ((fx= alignh GUI_ALIGNCENTER) (+ lx (/ (- lw (car name)) 2.)))
                              ((fx= alignh GUI_ALIGNRIGHT) (+ lx (- lw (car name))))
                              (else lx))
                           lx))
             (py (if alignv (cond
                              ((fx= alignv GUI_ALIGNCENTER) (+ ly (/ (- lh (cadr name)) 2.)))
                              ((fx= alignv GUI_ALIGNTOP) (+ ly (- lh (cadr name))))
                              (else ly))
                            ly))
             (p (glgui-pixmap g px py name)))
        (glgui-widget-set! g p 'color color)
        p)
      ;; Otherwise just use glgui-label-wrapped
      (let* ((linec (length (string-split-width text lw fnt)))
             (lineh (flo (glgui:fontheight fnt)))
             (labh (if alignv
                     (* lineh linec)
                     lh))
             (laby (if alignv (cond
                                ((fx= alignv GUI_ALIGNCENTER) (+ ly (/ (- lh labh) 2.)))
                                ((fx= alignv GUI_ALIGNTOP) (+ ly (- lh labh)))
                                (else ly))
                              ly))
             (label (glgui-label-wrapped g lx laby lw labh text fnt color)))
        (if alignh
          (glgui-widget-set! g label 'align alignh))
        label)))
)

(define (glgui-button-local g bx by bw bh str fnt callback)
  ;; Get the text from the language table
  (let ((text (local-get-text str)))
    (if (string=? text "[t]")
      ;; If a texture file instead of a string, use glgui-button instead of glgui-button-string
      (let ((name (eval (with-input-from-string (string-append str (number->string local:index) ".img") (lambda () (read))))))
        (glgui-button g bx by bw bh name callback))
      ;; Otherwise just use glgui-button-string
      (glgui-button-string g bx by bw bh text fnt callback)))
)

;; eof