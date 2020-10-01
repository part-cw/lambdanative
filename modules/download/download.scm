#|
lnHealth - Health related apps using the LambdaNative framework
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

;; Download module - download a file using httpsclient

(define download:buf (##still-copy (make-u8vector 1024)))
(define download:data (u8vector))
(define download:datachunk 50000)
(define download:datalen 0)

;; Helper function to split return string into header and body
(define (download:split-headerbody str)
  (let ((pos (string-contains str "\r\n\r\n")))
    (if pos (list (substring str 0 pos) (substring str (+ pos 4) (string-length str))) (list str (list)))
  ))

;; Helper function that returns a returned vector split into header and body
;; and changes the header into a string
(define (download:split-headerbody-vector vctr)
  (let* ((lst (u8vector->list vctr))
         (lst-length (length lst)))
    (let loop ((count 0))
      (if (fx> (- lst-length count) 4)
        (if (and (fx= (list-ref lst count) 13) (fx= (list-ref lst (+ count 1)) 10) (fx= (list-ref lst (+ count 2)) 13) (fx= (list-ref lst (+ count 3)) 10))
          (list (u8vector->string (subu8vector vctr 0 count)) (list->u8vector (list-tail lst (+ count 4))))
          (loop (+ count 1))
        )
        (list (u8vector->string vctr) #f)
      )
    )
  ))

(define (download:data-clear!)
  (set! download:datalen 0)
  (u8vector-shrink! download:data 0))

(define (download:data->string)
  (let ((str (u8vector->string (subu8vector download:data 0 download:datalen))))
    (download:data-clear!)
    str))

(define (download:data->u8vector)
  (let ((v (subu8vector download:data 0 download:datalen)))
    (download:data-clear!)
    v))

(define (download:data-append! v)
  (let ((lv (u8vector-length v))
        (la (u8vector-length download:data)))
    (if (> (+ download:datalen lv) la)
      (set! download:data (u8vector-append download:data (make-u8vector (max lv download:datachunk)))))
    (subu8vector-move! v 0 lv download:data download:datalen)
    (set! download:datalen (+ download:datalen lv))))

(define (download-list host folder)
  (let ((ret (httpsclient-open host)))
    (if (> ret 0)
      (let* ((request (string-append "GET " folder "?F=0 HTTP/1.0\r\nHost: " host "\r\n\r\n"))
             (status  (httpsclient-send (string->u8vector request))))
        (let loop ((n 1) (output (u8vector)))
          (if (fx<= n 0)
           (begin
             (httpsclient-close)
             (let ((res (download:split-headerbody (u8vector->string output))))
                (if (and (string? (car res)) (fx> (string-length (car res)) 12)
                         (or (string=? (substring (car res) 9 12) "201")
                             (string=? (substring (car res) 9 12) "200")))
                  (let* ((s (cadr res))
                         (s1 (substring s (fx+ (string-contains-ci s "ul") 3) (fx- (string-contains-ci s "/ul") 1)))
                         (s2 (string-replace-substring (string-replace-substring s1 "<li>" "") "</li>" ","))
                         (s3 (string-replace-substring (string-replace-substring s2 "<a href=\"" "") "</a>" ""))
                         (s4 (string-replace-substring (string-replace-substring s3 "\n" "") "\"> " ",")))
                    (list-remove-duplicates (cddr (string-split s4 #\,)))
                  )
                  #f
               ))
           )
           (let ((count (httpsclient-recv download:buf)))
             (loop count (u8vector-append output (subu8vector download:buf 0 count))))
         )
       )
     )
     #f
   )
 ))

(define (download-getfile host path filename)
  (let ((ret (httpsclient-open host)))
    (if (> ret 0)
      (let* ((request (string-append "GET " path " HTTP/1.0\r\nHost: " host "\r\n\r\n"))
             (status  (httpsclient-send (string->u8vector request))))
        (download:data-clear!)
        (let loop ((n 1))
          (if (fx<= n 0)
            (begin
              (httpsclient-close)
              (let ((fileout (download:split-headerbody-vector (download:data->u8vector))))
                ;; Status is Success, save file
                (if (and (string? (car fileout)) (fx> (string-length (car fileout)) 12)
                         (or (string=? (substring (car fileout) 9 12) "201")
                             (string=? (substring (car fileout) 9 12) "200")))
                  (let ((fh (open-output-file (string-append (system-directory) (system-pathseparator) filename))))
                    (write-subu8vector (cadr fileout) 0 (u8vector-length (cadr fileout)) fh)
                    (close-output-port fh)
                    #t
                  )
                  ;; Status is Redirect, send new request
                  (if (and (string? (car fileout)) (fx> (string-length (car fileout)) 12)
                           (string=? (substring (car fileout) 9 12) "302"))
                    (let* ((location-start (string-contains (car fileout) "Location: "))
                           (location-rest (substring (car fileout) location-start (string-length (car fileout))))
                           (location-stop (string-contains location-rest "\n"))
                           (location-only (substring location-rest 18 (- location-stop 1)))
                           (host (substring location-only 0 (string-contains location-only "/")))
                           (path (substring location-only (string-contains location-only "/") (string-length location-only))))
                      (download-getfile host path filename))
                    ;; Status is unknown, return false
                    #f
                  )
                )
              )
            )
           (let ((count (httpsclient-recv download:buf)))
             (if (or (string=? (system-platform) "android") (string=? (system-platform) "ios")) (thread-sleep! 0.001)) ;;allow GUI to refresh
             (if (> count 0) (download:data-append! (subu8vector download:buf 0 count)))
             (loop count))
          )
        )
      )
      #f
    )
  ))

;; eof
