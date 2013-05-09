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
;; procedures for working with csv files

(define (csv-read file)
  (with-input-from-file file (lambda ()
   ;; When reading in initially split by new line character
   (let loop ((line (read-line (current-input-port) #\newline)) (res '()))
     (if (eof-object? line) res
       ;; When appending to existing lines, split by ^M (from Excel) character and then by commas
       (loop (read-line) (append res (map (lambda (s) (csv:split s)) (string-split line #\x0d)))))))))

(define (csv:split str)
   ;; First, split the line normally on all commas
   (let ((lst (call-with-input-string str
           (lambda (port) (read-all port
              (lambda (p) (read-line p #\,)))))))
     ;; If the line contains quotation marks
     (if (string-contains str "\"")
        ;; If quotation marks, adjust spliting so that no quoted sections are broken up
        (let commaloop ((linelst lst) (newlst '()) (leftover ""))
            (if (fx> (length linelst) 0)
               (let* ((current (car linelst))
                      ;; Is there an odd number of qoutes in the current string
                      (oddq (odd? (string-count current "\"")))
                      ;; Is there any leftover text from the previous strings
                      (cont (fx> (string-length leftover) 0)))
                  (if oddq
                     ;; Odd number of quotes between two commas
                     (if cont
                        ;; Combine the current string with the leftovers from between last commas
                        (commaloop (cdr linelst) (append newlst (list (string-append leftover "," current))) "")
                        ;; OR Start new leftovers to be combined with later strings
                        (commaloop (cdr linelst) newlst current))
                     ;; Even number of quotes between two commas
                     (if cont
                        ;; Add to leftovers between last commas - still to combine with later strings
                        (commaloop (cdr linelst) newlst (string-append leftover "," current))
                        ;; Normal - even number of quotes and no leftovers
                        (commaloop (cdr linelst) (append newlst (list current)) ""))))
               ;; Finished the line
               (begin
                  ;; If still some leftover - odd number of quotes on whole line - just add to end
                  (if (fx> (string-length leftover) 0)
                     (append newlst (list leftover))
                     newlst))))
        ;; If no quotation marks on line - just return line as is
        lst)))

(define (csv-write file output)
  (let ((fh (open-output-file file)))
    (let lineloop ((res output))
      ;; If more lines
      (if (fx> (length res) 0)
        (begin 
          (let cellloop ((line (car res)))
          ;; If no more cells on line, add new line
          (if (fx= (length line) 0) (display "\n" fh)
            ;; Otherwise add current cell with comma following
            (begin (display (car line) fh) (display "," fh) 
              (cellloop (cdr line)))))
          (lineloop (cdr res)))
      )
    )
    (close-output-port fh)
  )
)

;; eof
