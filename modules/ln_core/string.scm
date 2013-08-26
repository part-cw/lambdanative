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
;; misc string manipulation

(define (string-trim str) 
  (if (string? str) (begin
     ;; Take any spaces off the front of the string
     (set! str (let loop ((startstr str))
       (if (and (fx> (string-length startstr) 0) (char=? (string-ref startstr 0) #\space))
          (loop  (substring startstr 1 (string-length startstr)))
          startstr)))
     ;; Take any spaces off the end of the string and return it
     (let loop2 ((endstr str))
       (if (and (fx> (string-length endstr) 0) (char=? (string-ref endstr (- (string-length endstr) 1)) #\space))
          (loop2  (substring endstr 0 (- (string-length endstr) 1)))
          endstr)))
     #f))

(define (string-remove-quotes str) 
  (let ((strlength (string-length str)))
    (if (and 
        (fx>= (string-length str) 2)
        ;; If it starts with quotation marks
        (string=? (substring str 0 1) "\"") 
        ;; and ends with quotation marks
        (string=? (substring str (- strlength 1) strlength) "\""))
      ;; Return string without quotation marks
      (substring str 1 (- strlength 1))
      ;; Otherwise just return copy of same string
      str))
)

(define (string-remove-spaces str)
  (string-remove-char str #\space)
)

(define (string-remove-char str chr)
  (let loop ((ret (list)) (lst (string->list str)))
    (if (not (pair? lst)) 
      (list->string ret)
      (loop (if (char=? (car lst) chr) ret (append ret (list (car lst))))  (cdr lst))
    )
  )
)

(define string-split (lambda (str sep)
  (if (string? str) (call-with-input-string str
     (lambda (p) (read-all p (lambda (p) (read-line p sep))))) #f)))

(define (string-upcase! str)
  (do ((i (- (string-length str) 1) (- i 1)))
      ((< i 0) str)                          
    (string-set! str i (char-upcase (string-ref str i)))))

(define (string-upcase str)
  (string-upcase! (string-copy str)))

(define (string-downcase! str)
  (do ((i (- (string-length str) 1) (- i 1)))
      ((< i 0) str)                          
    (string-set! str i (char-downcase (string-ref str i)))))

(define (string-downcase str)
  (string-downcase! (string-copy str)))

(define (string-capitalize! str)        ; "hello" -> "Hello"
  (let ((non-first-alpha #f)            ; "hELLO" -> "Hello"
        (str-len (string-length str)))  ; "*hello" -> "*Hello"
    (do ((i 0 (+ i 1)))                 ; "hello you" -> "Hello You"
        ((= i str-len) str)
      (let ((c (string-ref str i)))
        (if (char-alphabetic? c)
            (if non-first-alpha
                (string-set! str i (char-downcase c))
                (begin
                  (set! non-first-alpha #t)
                  (string-set! str i (char-upcase c))))
            (set! non-first-alpha #f))))))

(define (string-capitalize str)
  (string-capitalize! (string-copy str)))

(define string-explode (lambda (str seplst)
  (let loop ((strlst (string->list str))(tmp "")(res '()))
    (if (= (length strlst) 0) (append res 
      (if (> (string-length tmp) 0) (list tmp) '()))
      (let ((chop? (member (car strlst) seplst)))
        (loop (cdr strlst) (if chop? "" (string-append tmp (string (car strlst))))
          (if chop? (append res (list tmp) 
             (list (string (car strlst)))) res)))))))


(define (string-index str a-char cmp)
  (let loop ((pos 0)) (cond
      ((>= pos (string-length str)) #f)
      ((cmp a-char (string-ref str pos)) pos)
      (else (loop (fx+ pos 1))))))

(define (string:contains str pattern cmp)
  (let* ((pat-len (string-length pattern))
         (search-span (- (string-length str) pat-len))
         (c1 (if (zero? pat-len) #f (string-ref pattern 0)))
         (c2 (if (<= pat-len 1) #f (string-ref pattern 1))))
    (cond
     ((not c1) 0)    
     ((not c2) (string-index str c1 cmp))
     (else (let outer ((pos 0))
          (cond
	    ((> pos search-span) #f)
            ((not (cmp c1 (string-ref str pos)))
                (outer (+ 1 pos)))	
            ((not (cmp c2 (string-ref str (+ 1 pos))))
                (outer (+ 1 pos)))
            (else (let inner ((i-pat 2) (i-str (+ 2 pos)))
               (if (>= i-pat pat-len) pos 
                  (if (cmp (string-ref pattern i-pat) (string-ref str i-str))
                        (inner (+ 1 i-pat) (+ 1 i-str))
                        (outer (+ 1 pos))))))))))))	

(define (string-contains str pattern) (string:contains str pattern char=?))

(define (string-contains-ci str pattern) (string:contains str pattern char-ci=?))

(define (string-count str pattern)
  ;; Each time the pattern is found take a substring until no longer found
  (let loop ((curstr str) (count 0))
    (let ((index (string:contains curstr pattern char=?)))
      ;; If pattern still found in string
      (if index
          ;; If pattern starts at the end of the string - then last occurence
          (if (fx= index (- (string-length curstr) 1)) 
              (+ count 1)
              (loop (substring curstr (+ index 1) (string-length curstr)) (+ count 1)))
          count)))
)

(define (string-mapconcat sequence separator . proc)
  (if (fx> (length sequence) 0)
    (let* ((p (if (fx= (length proc) 1) (car proc) (lambda (x) x)))
           (rev (reverse
             (map (lambda (item)
                    (let ((val (p item)))
                      (cond 
                        ((string? val) val)
                        ((char? val) (make-string 1 val))
                        (else (with-output-to-string "" (lambda () (write val)))))))
                  (cond 
                    ((list? sequence) sequence)
                    ((vector? sequence) (vector->list sequence))
                    ((string? sequence) (string->list sequence))
                    (else #f))))))
      (apply string-append
         (let loop ((s (cdr rev))
                    (acc (list (car rev))))
           (if (null? s) acc (loop 
                               (cdr s) 
                               (cons (car s) (cons separator acc)))))))
    ;; If an empty list, then return an empty string
    "")
)

(define (string-replace-char str oldchr newchr)
  (if (and (string? str) (char? oldchr) (char? newchr))
    (let loop ((i 0) (newstr (string-copy str)))
      (if (fx= i (string-length str)) newstr
        (loop (+ i 1) (begin (if (char=? (string-ref str i) oldchr) (string-set! newstr i newchr)) newstr))
      ))
    str))

(define (string-replace-substring str searchstr replacestr)
  (let ((searchstrlen (string-length searchstr)))
    (if (and (string? str) (string? searchstr) 
          (string? replacestr) (>= (string-length str) (string-length searchstr)))
      (let loop ((substr str)(res ""))
        (if (= (string-length substr) 0) res
          (let ((match? (and (>= (string-length substr) searchstrlen)
                          (string=? (substring substr 0 searchstrlen) searchstr))))
            (loop (substring substr (if match? searchstrlen 1) (string-length substr))
              (string-append res (if match? replacestr (substring substr 0 1))))))) str)))

(define (string-split-into-two str)
  
  ;; First trim the string
  (set! str (string-trim str))
  
  ;; Handle the empty string by just returning two empty strings - one for each line
  (if (fx= (string-length str) 0)
    (list "" "")
      
    ;; Keep track of first and second line as lists of words
    (let loop ((first (string-split str #\space)) (second (list)) (bestw (string-length str)))
      (let* ((moveindex (- (length first) 1))
             ;; Move word from first line to second
             (newfirst (list-head first moveindex))
             (newsecond (append (list (list-ref first moveindex)) second))
             ;; Get new character count width needed for the two lines
             (neww (max (string-length (string-mapconcat newfirst " ")) (string-length (string-mapconcat newsecond " ")))))
        (if (< neww bestw)
          ;; If still better than the last, try moving another word
          (loop newfirst newsecond neww)
          ;; Otherwise the last combination was best, recombine words of each line and return
          (list (string-mapconcat first " ") (string-mapconcat second " "))))))
)

;; @deffn {procedure} string-split-width str width font
;;   Returns a modified version of string str which is wrapped 
;;   to fit into a window of width w using the fontsize obtained from font
;; --> Moved to glgui/glgui-primitives to prevent warning in Console apps.

;; eof
