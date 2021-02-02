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

(display "DemoConsole\n")
(include "/Users/mg/Documents/Programming/lambdanative/modules/generalized-arrays/test-arrays.scm")


(define host "ecem.ece.ubc.ca")
(define path "/~mgorges/target.txt")
(define filename "pattern.txt")
(download-getfile host path filename)
(define host "192.168.31.111")
(download-getfile host path filename)

(define src (make-list 10 "asdü"))
(define (concatenate-large-string-list2 lst)
  (define len 0)
  (for-each (lambda (l) (set! len (fx+ len (string-length l)))) lst)
  (let ((offset 0)
        (result (make-string len)))
    (for-each (lambda (l)
       (for-each (lambda (sl) (string-set! result offset sl) (set! offset (fx+ offset 1))) (string->list l))
    ) lst)
    result
  ))
(define (concatenate-large-string-list lst)
 (let* ((len (let loop ((i 0) (lst lst))
                  (if (null? lst) i
                      (loop
                        (+ i (string-length (car lst)))
                       (cdr lst)))))
          (result (make-string len)))
    (do ((lst lst (cdr lst)) (offset 0 (+ offset (string-length (car lst)))))
          ((null?  lst) result)
       (string-copy! result offset (car lst)))))

(define (concatenate-large-string-list3 lst)
  (let* ((len (do ((i 0 (fx+ i (string-length (car lst))))
                   (lst lst (cdr lst)))
                  ((null? lst) i)))
         (result (make-string len)))
    (do ((lst lst (cdr lst))
         (offset 0 (fx+ offset (string-length (car lst)))))
        ((null? lst) result)
      (string-copy! result offset (car lst)))))

(define (concatenate-large-string-list4 lst)
  (let* ((len (do ((i 0 (fx+ i (string-length (car lst))))
                   (lst lst (cdr lst)))
                  ((null? lst) i)))
         (result (make-string len)))
    (do ((lst lst (cdr lst))
         (offset 0 (fx+ offset (string-length (car lst)))))
        ((null? lst) result)
       (do ((strlst (string->list (car lst)) (cdr strlst))
            (offset2 offset (fx+ offset2 1)))
         ((null? strlst) offset2)
        (string-set! result offset2 (car strlst))))))

(thread-start! (make-thread (lambda ()
(define start (current-time-seconds))
(let loop ((i 0))
  (if (fx= i 100000)
    (for-each display (list "concatenate-large-string-list: " (fl- (current-time-seconds) start) "\n"))
    (begin
      (concatenate-large-string-list src)
      (loop (fx+ i 1))
    )
  ))
)))
(thread-start! (make-thread (lambda ()
(define start (current-time-seconds))
(let loop ((i 0))
  (if (fx= i 100000)
    (for-each display (list "concatenate-large-string-list2: " (fl- (current-time-seconds) start) "\n"))
    (begin
      (concatenate-large-string-list2 src)
      (loop (fx+ i 1))
    )
  ))
)))
(thread-start! (make-thread (lambda ()
(define start (current-time-seconds))
(let loop ((i 0))
  (if (fx= i 100000)
    (for-each display (list "concatenate-large-string-list3: " (fl- (current-time-seconds) start) "\n"))
    (begin
      (concatenate-large-string-list3 src)
      (loop (fx+ i 1))
    )
  ))
)))
(thread-start! (make-thread (lambda ()
(define start (current-time-seconds))
(let loop ((i 0))
  (if (fx= i 100000)
    (for-each display (list "concatenate-large-string-list4: " (fl- (current-time-seconds) start) "\n"))
    (begin
      (concatenate-large-string-list4 src)
      (loop (fx+ i 1))
    )
  ))
)))
(thread-start! (make-thread (lambda ()
(define start (current-time-seconds))
(let loop ((i 0))
  (if (fx= i 100000)
    (for-each display (list "list->utf8string: " (fl- (current-time-seconds) start) "\n"))
    (begin
      (list->utf8string src)
      (loop (fx+ i 1))
    )
  ))
)))

(let loop ()
  (with-exception-catcher (lambda (e)
    (for-each display (list (exception->string e) "\n")) #f)
      (lambda () (##repl-debug)))
  (loop))

;; eof
