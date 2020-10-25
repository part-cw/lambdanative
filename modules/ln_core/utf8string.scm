#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2020, University of British Columbia
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
    (let loop ((i 0) (s ""))
      (if (fx= i n) s
        (loop (fx+ i 1) (string-append s utf8c))))))

(define (utf8string->list s) (map integer->utf8char (utf8string->unicode s)))

(define (list->utf8string lst)
  (define len 0)
  (for-each (lambda (l) (set! len (fx+ len (string-length l)))) lst)
  (let ((offset 0)
        (result (make-string len)))
    (for-each (lambda (l)
       (string-copy! result offset l)
       (set! offset (fx+ offset (string-length l)))
    ) lst)
    result
  ))

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

(define utf8string->unicode:on-encoding-error
  (let ((handler #f))
    (define (utf8string->unicode:on-encoding-error-replace str idx) #xfffd)
    (define (utf8string->unicode:on-encoding-error-raise-error str idx)
      (error "UTF-8 char encoding error" str idx))
    (define (utf8string->unicode:on-encoding-error-log+replace str idx)
      (log-error "UTF-8 char encoding error" str idx)
      #xfffd)
    (define (select key)
      (set!
       handler
       (if (procedure? key)
           key
           (case key
             ((replace #f) utf8string->unicode:on-encoding-error-replace)
             ((log+replace) utf8string->unicode:on-encoding-error-log+replace)
             (else utf8string->unicode:on-encoding-error-raise-error)))))
    (select 'error)
    (lambda args
      (if (null? args) handler (select (car args))))))

(define (utf8string->unicode str #!optional (encoding-error #t))
  (define (on-encoding-error i)
    (cond
     ((not encoding-error) #xfffd) ;; deliver replacement charater
     ((procedure? encoding-error) (encoding-error str i)) ;; delegate to caller
     (else ((utf8string->unicode:on-encoding-error) str i)))) ;; delegate to registered
  (let next-char ((i 0))
    (if (fx>= i (string-length str)) '()
        (let* ((c (string-ref str i))
               (c1 (char->integer c)))
                (receive (size m1)
                    (cond
                     ((fx< c1 #x80) (values 1 #x7f))
                     ((fx< c1 #xe0) (values 2 #x0f))
                     ((fx< c1 #xf0) (values 3 #x0f))
                     ((fx< c1 #xf8) (values 4 #x07))
                     ((fx< c1 #xfc) (values 5 #x03))
                     (else (values 6 #x01)))
                  (if (fx= size 1)
                      (cons c1 (next-char (fx+ i 1)))
                      (let ((limit (fx+ i size)))
                        (let subc ((j (fx+ i 1)) (r (bitwise-and c1 m1)))
                          (cond
                           ((fx>= j limit) (cons r (next-char limit)))
                           ((fx>= j (string-length str)) (list (on-encoding-error j)))
                           (else
                            (let ((cc (char->integer (string-ref str j))))
                              (if (or (fx< cc #x80) (fx>= cc #xc0))
                                  (let ((r (on-encoding-error j))) ;; 1st force handling
                                    (cons r (next-char j)))
                                  (subc
                                   (fx+ j 1)
                                   (bitwise-ior
                                    (arithmetic-shift r 6)
                                    (bitwise-and cc #x3F)))))))))))))))

(define (utf8string->unicode/fallback str #!key (fallback #f))
  ;; A version, which replaces string causing convertion erros with a
  ;; hopefuly safe fallback.
  (let ((use-fallback (list 0))) ;; just a well known value
    (with-exception-catcher
     (lambda (exn)
       (if (eq? exn use-fallback)
           (case fallback
             ((#f) (map char->integer (string->list str)))
             (else (fallback str)))
           (raise exn)))
     (lambda () (utf8string->unicode str (lambda (str i) (raise use-fallback)))))))

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
        (else (list->utf8string (map unicode->utf8string src)))
  ))

;;u8vectors that need unicode conversion
(define (u8vector->utf8string vec)
  (unicode->utf8string (vector->list (u8vector->unicode-vector (subu8vector vec 0
    (let loop ((i 0))
      (if (or (fx= i (u8vector-length vec)) (fx= (u8vector-ref vec i) 0)) i
      (loop (+ i 1)))
    ))))))

;; eof
