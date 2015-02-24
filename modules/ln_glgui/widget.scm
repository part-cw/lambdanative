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
;; guis and widgets 

(define make-gtable make-table)
(define gtable-ref   table-ref)
(define gtable-set!  table-set!)
(define list->gtable list->table)

;;; GUI

(define (glgui-get g id . fb) (gtable-ref g id (if (fx= (length fb) 1) (car fb) #f)))
(define (glgui-set! g id val) (gtable-set! g id val))

;; 20100804: adding offset option
(define (make-glgui . offset)
  (let* ((t (make-gtable init: '()))
         (x0 (if (> (length offset) 0) (car offset) 0))
         (y0 (if (> (length offset) 1) (cadr offset) 0)))
    (glgui-set! t 'widget-list '())
    (glgui-set! t 'widget-count 0)
    (glgui-set! t 'xofs x0)
    (glgui-set! t 'yofs y0)
    (glgui-set! t 'isgui #t)
    t))

;; WIDGET 
(define (glgui-widget-add g . args)
  (let* ((tmplist (let loop ((a args)(r '()))
           (if (fx= (length a) 0) r
             (loop (cdr (cdr a)) (append r 
               (list (cons (car a) (cadr a))))))))
         (entry (list->gtable tmplist)))
     (let ((widget-list (glgui-get g 'widget-list '())))
        (glgui-set! g 'widget-list (append widget-list (list entry))))
     (let ((widget-count (glgui-get g 'widget-count 0)))
       (glgui-set! g 'widget-count (fx+ widget-count 1)))
    entry
  ))

(define (glgui-widget-delete g w)
  (let ((l (glgui-get g 'widget-list))
        (c (glgui-get g 'widget-count)))
    (glgui-set! g 'widget-list 
      (let loop ((ws l)(r '()))
        (if (fx= (length ws) 0) r
           (loop (cdr ws) (append r (if (equal? (car ws) w) '() (list (car ws))))))))
    (glgui-set! g 'widget-count (fx- c 1))
  ))

;; return a value from a widget identifier, like so
;; (glgui-widget-get mygui mybutton 'label)
(define (glgui-widget-get g w id) (gtable-ref w id #f))

(define (glgui-widget-get-def g w id . def) (gtable-ref w id (if (fx= (length def) 1) (car def) #f)))

;; return a dynamic value
;; 20100419: added g w to calling arguments
;;           this allows us to make spiffy changes on the fly!
(define (glgui-widget-get-dyn g w id)
  (let ((val (gtable-ref w id #f)))
    (if (procedure? val) (apply val (list g w (glgui-width-get) (glgui-height-get))) val)))

;; set a widget identifier value, like so
;; (glgui-widget-set! mygui mybutton 'hidden #f)
(define (glgui-widget-set! g w id val)
  (gtable-set! w id val)
  (let ((u (gtable-ref w 'update-handle #f)))
    (if u (u g w id val))) #t)

(define (glgui-widget-clear! g w id)
  (gtable-set! w id))

;; set the same parameter in all widgets
;; for example: (glgui-widget-setglobal! mygui 'focus #f)
(define (glgui-widget-setglobal! g id val)
  (for-each (lambda (w) (glgui-widget-set! g w id val)) (glgui-get g 'widget-list))
)

;; eof
