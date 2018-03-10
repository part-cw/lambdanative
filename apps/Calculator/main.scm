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

;; This simple calculator app uses the gambit in-fix interpreter
;; It handles fractions, complex numbers, and large fixnums!

(define gui #f)
(define calculator-subdisplay #f)
(define calculator-display #f)

(define keypad `((
  ( (#\A "AC") (#\M "MC") #\C (,delchar ,glgui_keypad_delete.img) )
  ( (#\a "Ans") (#\m "Mem")  (#\p "M+") (#\q "M-") )
  ( #\( #\) (#\S ,sqrt.img) #\/ )
  ( #\7 #\8 #\9 #\* )
  ( #\4 #\5 #\6 #\- )
  ( #\1 #\2 #\3 #\+ )
  ( (#\0 "0" 2.)  #\. (#\= "=" 1. ,DarkOrange))
)))

(define Ans #f)
(define Mem 0)

;; suppress floating point inaccuracies
(define (number->neatstring n)
  (if (not (##flonum? n)) (number->string n)
    (let* ((s (number->string (fl/ (flfloor (fl+ (fl* (flo n) 1.0e10) 0.5)) 1.0e10)))
           (sl (string-length s))
           (b (substring s 0 1))
           (e (substring s (- sl 1) sl)))
      (string-append (if (string=? b ".") "0" "")
         (if (string=? e ".") (substring s 0 (- sl 1)) s)))))

(define (calculator-updatesub)
  (glgui-widget-set! gui calculator-subdisplay 'label
    (string-append "Mem=" (number->neatstring Mem) " Ans=" (if Ans (number->neatstring Ans) "") " ")))

(define (calculator-evaluate)
  (let* ((e (glgui-widget-get gui calculator-display 'label))
         (evalstr (string-append "\\" e ";"))
         (res (with-input-from-string evalstr (lambda ()
           (with-exception-catcher (lambda (e) #f) (lambda () (eval (read))))))))
    (set! Ans (if (eq? res (void)) #f res))
    (glgui-widget-set! gui calculator-display 'label (if Ans (number->neatstring Ans) ""))
    (glgui-widget-set! gui calculator-display 'bgcolor (if Ans #f Red))
    (calculator-updatesub)))

(define (calculator-C)  (glgui-widget-set! gui calculator-display 'label ""))
(define (calculator-AC) (calculator-C) (set! Ans #f) (set! Mem 0) (calculator-updatesub))
(define (calculator-MC) (set! Mem 0) (calculator-updatesub))

(define (calculator-Mem)
  (let ((curstr (glgui-widget-get gui calculator-display 'label)))
    (glgui-widget-set! gui calculator-display 'label (string-append curstr "Mem"))))

(define (calculator-Ans)
  (let ((curstr (glgui-widget-get gui calculator-display 'label)))
    (glgui-widget-set! gui calculator-display 'label (string-append curstr "Ans"))))

(define (calculator-M+) (if Ans (set! Mem (+ Mem Ans))) (calculator-updatesub))
(define (calculator-M-) (if Ans (set! Mem (- Mem Ans))) (calculator-updatesub))

(define (calculator-sqrt)
  (let ((curstr (glgui-widget-get gui calculator-display 'label)))
    (glgui-widget-set! gui calculator-display 'label (string-append curstr "sqrt("))))

;; ------------------

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    (let* ((w (glgui-width-get))
           (h (glgui-height-get)))
      (set! calculator-subdisplay (glgui-label gui 0 (- h 20) w 20 "" calc_14.fnt White))
      (glgui-widget-set! gui calculator-subdisplay 'align GUI_ALIGNRIGHT)
      (set! calculator-display (glgui-label gui 5 (- h 80) (- w 10) 60 "" calc_32.fnt White))
      (glgui-widget-set! gui calculator-display 'align GUI_ALIGNRIGHT)
      (glgui-widget-set! gui calculator-display 'focus #t)
      (let ((wgt (glgui-keypad gui 5 5 (- w 10) (- h 80 5) calc_24.fnt keypad)))
        (glgui-widget-set! gui wgt 'rounded #f)
        (glgui-widget-set! gui wgt 'floatinghighlight #f))
      (calculator-updatesub)
    ))
;; events
  (lambda (t x y)
    (let ((skipevent #f))
      (if (= t EVENT_KEYRELEASE)
        (cond
          ((= x EVENT_KEYESCAPE)     (terminate))
          ((= x (char->integer #\=)) (calculator-evaluate) (set! skipevent #t))
          ((= x (char->integer #\A)) (calculator-AC) (set! skipevent #t))
          ((= x (char->integer #\C)) (calculator-C) (set! skipevent #t))
          ((= x (char->integer #\M)) (calculator-MC) (set! skipevent #t))
          ((= x (char->integer #\p)) (calculator-M+) (set! skipevent #t))
          ((= x (char->integer #\q)) (calculator-M-) (set! skipevent #t))
          ((= x (char->integer #\a)) (calculator-Ans) (set! skipevent #t))
          ((= x (char->integer #\m)) (calculator-Mem) (set! skipevent #t))
          ((= x (char->integer #\S)) (calculator-sqrt) (set! skipevent #t))
        ))
      (if (not skipevent) (glgui-event gui t x y))))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
