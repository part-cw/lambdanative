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

;; generate xml/html from sxml
(define (sxml->xml lst . prt)
  (let ((p (if (fx= (length prt) 1) (car prt) (current-output-port))))
    (define (sxml:display x) (display x p))
    (define (sxml:attr lst)
      (let loop ((l lst))
        (if (> (length l) 0) (begin (if (list (car l)) 
            (for-each sxml:display (list " " (car (car l)) "=\"" (cadr (car l)) "\"")))
          (loop (cdr l))))))
    (let loop ((l lst)(tag #f)(count 0))
      (if (= (length l) 0)
        (begin (if (and tag (> count 0))
           (for-each sxml:display (list "</" tag ">"))))
        (let ((subl (car l))
              (subltype ""))
          (cond ((symbol? subl) 
                   (for-each sxml:display (list "<" subl))
                   ;; try to locate attributes
                   (if (and (> (length l) 1) (list? (cadr l)) 
                            (string=? (symbol->string (car (cadr l))) "@"))
                     (sxml:attr (cdr (cadr l)))) 
                   (sxml:display ">"))
                ((list? subl)
                   (set! subltype (symbol->string (car subl)))
                   (cond ((string=? subltype "@") #t)
                         ((string=? subltype "&") (for-each sxml:display  
                            (list "&" (cadr subl) ";")))
                         (else (sxml->xml subl p))))
                ((string? subl) (for-each sxml:display (list subl)))
          )
          (loop (cdr l) (if (symbol? subl) subl tag) 
             (if (symbol? subl) 0 (if (and (list? subl) (string=? subltype "@")) 
                count (+ count 1)))))))))
;; eof