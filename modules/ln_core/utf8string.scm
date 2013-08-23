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
;; misc utf8 manipulation

(define (utf8string-length src)
  (length (utf8string->unicode src)))

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
                 ((fx<= x #x7fffffff)
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
