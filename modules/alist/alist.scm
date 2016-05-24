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

;; manipulate association lists

(define (alist-ref alist key . def)
  (if (not (list? alist)) (if (fx= (length def) 1) (car def) #f)
    (let* ((a (assoc key alist)))
      (if (list? a) (cadr a) 
        (if (pair? a) (cdr a)
          (if (fx= (length def) 1) (car def) #f))))))

(define (alist-refstr alist key defstr)
  (let ((res (alist-ref alist key defstr)))
    (if (string? res) res
      (if (number? res) (number->string res) 
        (if (symbol? res) (symbol->string res) defstr)))))

(define (alist-refnum alist key defnum)
  (let* ((res (alist-ref alist key defnum)))
    (if (number? res) res
      (if (string? res) 
         (let ((tmp (string->number res)))
           (if (number? tmp) tmp defnum))
        (if (symbol? res)
          (let ((tmp (string->number (symbol->string res))))
            (if (number? tmp) tmp defnum)) defnum)))))

(define (alist-delkeys alist . keys)
  (define (list-del-item lst item)
    (if (not item) lst (cond
      ((fx= (length lst) 0) lst)
      ((equal? item (car lst)) (cdr lst))
      (else (cons (car lst) (list-del-item (cdr lst) item)))
    )))
  (let loop ((ks keys)(res alist))
    (if (= (length ks) 0) res
      (let ((item (assoc (car ks) res)))
        (loop (cdr keys) (list-del-item res item))))))
   
(define (alist-set alist . keyvals)
  (let loop ((kvs keyvals)(res (if alist alist '())))
    (if (< (length kvs) 2) 
      (append (alist-delkeys res 'timestamp) (list (list 'timestamp (time->seconds (current-time)))))
      (loop (cddr kvs) (append (alist-delkeys res (car kvs)) (list (list (car kvs) (cadr kvs))))))))

(define (write-alist-to-file filename alist)
  (with-output-to-file filename
    (lambda ()
      (write alist))))

(define (read-alist-from-file filename)
  (with-input-from-file filename
    (lambda ()
      (read))))

;; eof
