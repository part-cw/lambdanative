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

;; misc simplistic utf8 manipulation
;; standard gambit string operations are not utf8 aware

;; -------------------------------
;; character level - the utf8 "char" is a string

(define utf8char? string?)
(define utf8char=? string=?)
(define utf8char-ci=? string-ci=?)

(define (char->utf8char c) (if (char? c) (string c) c))

(define (utf8char->integer c) (car (utf8string->unicode c)))
(define (integer->utf8char i) (unicode->utf8string (list i)))

;; -------------------------------
;; string level

(define utf8string? string?)
(define utf8string=? string=?)
(define utf8string-append string-append)
(define utf8string-copy string-copy) 

(define (utf8string-length src) (length (utf8string->unicode src)))

(define (utf8string-ref s idx) (integer->utf8char (list-ref (utf8string->unicode s) idx)))

(define (utf8string . cs) 
  (let ((utf8cs (map char->utf8char cs)))
   (apply string-append utf8cs)))

(define (make-utf8string n . c) 
  (let ((utf8c (char->utf8char (if (fx= (length c) 1) (car c) " "))))
    (let loop ((i 0)(s ""))
      (if (fx= i n) s 
        (loop (fx+ i 1) (string-append s utf8c))))))

(define (utf8string->list s) (map integer->utf8char (utf8string->unicode s)))

(define (list->utf8string l) (apply string-append l))

(define (utf8substring s ofs len)
  (list->utf8string (sublist (utf8string->list s) ofs len)))

;; ---------------------------
;; replicate extensions in string.scm

(define utf8string-trim string-trim)

(define utf8string-remove-quotes string-remove-quotes)

(define (utf8string-remove-spaces str) (utf8string-remove-char str #\space))

(define (utf8string-remove-char str chr)
  (let ((utf8chr (char->utf8char chr)))
    (let loop ((ret (list)) (lst (utf8string->list str)))
      (if (not (pair? lst)) (list->utf8string ret)
        (loop (if (utf8char=? (car lst) utf8chr) ret (append ret (list (car lst))))  (cdr lst))
      ))))

(define utf8string-upcase! string-upcase!)
(define utf8string-upcase string-upcase)

(define utf8string-downcase! string-downcase!)
(define utf8string-downcase string-downcase)

(define utf8string-capitalize! string-capitalize!)
(define utf8string-capitalize string-capitalize)

(define utf8string-explode (lambda (str seplst)
  (let ((utf8seplst (map char->utf8char seplst)))
    (let loop ((strlst (utf8string->list str))(tmp "")(res '()))
      (if (= (length strlst) 0) (append res 
        (if (> (utf8string-length tmp) 0) (list tmp) '()))
        (let ((chop? (member (car strlst) utf8seplst)))
          (loop (cdr strlst) (if chop? "" (utf8string-append tmp (utf8string (car strlst))))
            (if chop? (append res (list tmp) 
               (list (utf8string (car strlst)))) res))))))))

(define (utf8string-split str sep)
  (let ((utf8sep (char->utf8char sep)))
    (let loop ((cs (utf8string->list str))(subres "")(res '()))
      (if (= (length cs) 0) (if (> (utf8string-length subres) 0) 
        (append res (list subres)) res)
        (let* ((c (car cs))
               (split? (utf8char=? c utf8sep)))
          (loop (cdr cs) (if split? "" (utf8string-append subres c))
            (if split? (append res (list subres)) res)))))))

(define (utf8string-index str a-char cmp)
  (let ((utf8a-char (char->utf8char a-char)))
    (let loop ((pos 0)) (cond
        ((>= pos (utf8string-length str)) #f)
        ((cmp utf8a-char (utf8string-ref str pos)) pos)
        (else (loop (fx+ pos 1)))))))

(define (utf8string:contains str pattern cmp)
  (let* ((pat-len (utf8string-length pattern))
         (search-span (- (utf8string-length str) pat-len))
         (c1 (if (zero? pat-len) #f (utf8string-ref pattern 0)))
         (c2 (if (<= pat-len 1) #f (utf8string-ref pattern 1))))
    (cond
     ((not c1) 0)    
     ((not c2) (utf8string-index str c1 cmp))
     (else (let outer ((pos 0))
          (cond
	    ((> pos search-span) #f)
            ((not (cmp c1 (utf8string-ref str pos)))
                (outer (+ 1 pos)))	
            ((not (cmp c2 (utf8string-ref str (+ 1 pos))))
                (outer (+ 1 pos)))
            (else (let inner ((i-pat 2) (i-str (+ 2 pos)))
               (if (>= i-pat pat-len) pos 
                  (if (cmp (utf8string-ref pattern i-pat) (utf8string-ref str i-str))
                        (inner (+ 1 i-pat) (+ 1 i-str))
                        (outer (+ 1 pos))))))))))))	

(define (utf8string-contains str pattern) (utf8string:contains str pattern utf8char=?))
(define (utf8string-contains-ci str pattern) (utf8string:contains str pattern utf8char-ci=?))

(define utf8string-count string-count)
(define utf8string-mapconcat string-mapconcat)

(define (utf8string-replace-char str oldchr newchr)
  (let ((utf8oldchr (char->utf8char oldchr))
        (utf8newchr (char->utf8char newchr)))
    (let loop ((oldcs (utf8string->list str))(newcs '()))
      (if (= (length oldcs) 0) (list->utf8string newcs)
        (loop (cdr oldcs) (append newcs  
          (list (if (utf8char=? (car oldcs) utf8oldchr) utf8newchr (car oldcs)))))))))

(define utf8string-replace-substring string-replace-substring)

(define (utf8string-split-into-two str)
  (set! str (utf8string-trim str))
  (if (fx= (utf8string-length str) 0)
    (list "" "")
    (let loop ((first (utf8string-split str #\space)) (second (list)) (bestw (utf8string-length str)))
      (let* ((moveindex (- (length first) 1))
             (newfirst (list-head first moveindex))
             (newsecond (append (list (list-ref first moveindex)) second))
             (neww (max (utf8string-length (utf8string-mapconcat newfirst " ")) 
               (utf8string-length (utf8string-mapconcat newsecond " ")))))
        (if (< neww bestw)
          (loop newfirst newsecond neww)
          (list (utf8string-mapconcat first " ") (utf8string-mapconcat second " ")))))))

;; -------------------------------
;; unicode<->utf8 translation
;; adopted from http://ccm.sherry.jp/cleite/ (public domain)

(define (utf8string->unicode src)
  (define (decode l)
    (if (null? l) l
      (cond ((fx= 0 (bitwise-and (car l) #x80))
               (cons (car l)
                     (decode (list-tail l 1))))
            ((fx= #xC0 (bitwise-and (car l) #xE0))
               (cons (bitwise-ior (arithmetic-shift (bitwise-and (list-ref l 0) #x1F) 6)
                                  (bitwise-and (list-ref l 1) #x3F))
                     (decode (list-tail l 2))))
            ((fx= #xE0 (bitwise-and (car l) #xF0))
                (cons (bitwise-ior (arithmetic-shift (bitwise-and (list-ref l 0) #x0F) 12)
                                   (arithmetic-shift (bitwise-and (list-ref l 1) #x3F)  6)
                                   (bitwise-and (list-ref l 2) #x3F))
                      (decode (list-tail l 3))))
            ((fx= #xF0 (bitwise-and (car l) #xF8))
                (cons (bitwise-ior (arithmetic-shift (bitwise-and (list-ref l 0) #x07) 18)
                                   (arithmetic-shift (bitwise-and (list-ref l 1) #x3F) 12)
                                   (arithmetic-shift (bitwise-and (list-ref l 2) #x3F)  6)
                                   (bitwise-and (list-ref l 3) #x3F))
                      (decode (list-tail l 4))))
            ((fx= #xF8 (bitwise-and (car l) #xFC))
                (cons (bitwise-ior (arithmetic-shift (bitwise-and (list-ref l 0) #x03) 24)
                                   (arithmetic-shift (bitwise-and (list-ref l 1) #x3F) 18)
                                   (arithmetic-shift (bitwise-and (list-ref l 2) #x3F) 12)
                                   (arithmetic-shift (bitwise-and (list-ref l 3) #x3F)  6)
                                   (bitwise-and (list-ref l 4) #x3F))
                      (decode (list-tail l 5))))
            ((fx= #xFC (bitwise-and (car l) #xFE))
                (cons (bitwise-ior (arithmetic-shift (bitwise-and (list-ref l 0) #x01) 30)
                                   (arithmetic-shift (bitwise-and (list-ref l 1) #x3F) 24)
                                   (arithmetic-shift (bitwise-and (list-ref l 2) #x3F) 18)
                                   (arithmetic-shift (bitwise-and (list-ref l 3) #x3F) 12)
                                   (arithmetic-shift (bitwise-and (list-ref l 4) #x3F)  6)
                                   (bitwise-and (list-ref l 5) #x3F))
                      (decode (list-tail l 6)))))))
     (decode (map char->integer (string->list src))))

(define (unicode->utf8string src)
  (cond ((integer? src)
         (let ((c src))
           (cond ((fx<= c #x7F)
                  (string (integer->char (bitwise-and c #x7F))))
                 ((fx<= c #x07FF)
                  (string (integer->char (bitwise-ior #xC0 (bitwise-and #x1F (arithmetic-shift c -6))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F c)))))
                 ((fx<= c #xffff)
                  (string (integer->char (bitwise-ior #xE0 (bitwise-and #x0F (arithmetic-shift c -12))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c  -6))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F c)))))
                 ((fx<= c #x1fffff)
                  (string (integer->char (bitwise-ior #xF0 (bitwise-and #x07 (arithmetic-shift c -18))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c -12))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c  -6))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F c)))))
                 ((fx<= c #x3ffffff)
                  (string (integer->char (bitwise-ior #xF8 (bitwise-and #x03 (arithmetic-shift c -24))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c -18))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c -12))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c  -6))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F c)))))
                 ((fx<= c #x7fffffff)
                  (string (integer->char (bitwise-ior #xFC (bitwise-and #x01 (arithmetic-shift c -30))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c -24))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c -18))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c -12))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F (arithmetic-shift c  -6))))
                          (integer->char (bitwise-ior #x80 (bitwise-and #x3F c)))))
                 (else (log-error "unicode->utf8string: illegal format")))
           ))
        (else (apply string-append (map unicode->utf8string src)))
  ))

;; eof
