#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
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

;; Test input and output
(include "csv-test-input.scm")

;; Useful procedures for working with csv files.

(define (csv-read file)
  (with-input-from-file file (lambda ()
    ;; When reading in initially split by new line character
    (let loop ((line (read-line (current-input-port) #\newline)) (res '()))
      (if (eof-object? line)
        ;; Then split the cells within each row, but keep track of where commas or new lines are actually inside quotations, so shouldn't apply
        (let sploop ((rows res) (leftover #f) (leftovercount 0) (finalrows '()))
          (if (fx> (length rows) 0)
            (let* ((row (car rows))
                   (quotecount (+ leftovercount (string-count row "\""))))
              (if (odd? quotecount)
                ;; If the number of quotation marks on the line (combined with any from leftover unfinished lines before)
                ;; is odd then the line is unfinished, so combine with later lines
                (sploop (cdr rows) (if leftover (string-append leftover "\n" row) row) quotecount finalrows)
                ;; Otherwise this is a complete row, so just add to final output after splitting the row
                (sploop (cdr rows) #f 0 (append finalrows (list (csv:split (if leftover (string-append leftover "\n" row) row)))))
              )
            )
            ;; Once done all rows, return the final output, add last row if necessary
            (if leftover
              (append finalrows (list (csv:split leftover)))
              finalrows
            )
          )
        )
        ;; First, just make list of strings (one string per potential row) by breaking input on #\newline and #\x0d
        (loop (read-line) (append res (let ((rowsplit (string-split line #\x0d)))
                                        (if (fx= (length rowsplit) 0) (list "") rowsplit))))
      )
    )
  ))
)

(define (csv:split str)
  ;; First, split the line normally on all commas
  (let ((lst (call-with-input-string str (lambda (port) (read-all port (lambda (p) (read-line p #\,)))))))
    ;; If the line contains quotation marks
    (if (string-contains str "\"")
      ;; If quotation marks, adjust spliting so that no quoted sections are broken up
      (let commaloop ((linelst lst) (newlst '()) (leftover ""))
        (if (fx> (length linelst) 0)
          (let* ((current (car linelst))
                 (oddq (odd? (string-count current "\""))) ;; Is there an odd number of qoutes in the current string
                 (cont (fx> (string-length leftover) 0))) ;; Is there any leftover text from the previous strings
            (if oddq
              ;; Odd number of quotes between two commas
              (if cont
                ;; Combine the current string with the leftovers from between last commas
                (commaloop (cdr linelst) (append newlst (list (string-append leftover "," current))) "")
                ;; OR Start new leftovers to be combined with later strings
                (commaloop (cdr linelst) newlst current)
              )
              ;; Even number of quotes between two commas
              (if cont
                ;; Add to leftovers between last commas - still to combine with later strings
                (commaloop (cdr linelst) newlst (string-append leftover "," current))
                ;; Normal - even number of quotes and no leftovers
                (commaloop (cdr linelst) (append newlst (list current)) "")
              )
            )
          )
          ;; Finished the line
          (begin
            ;; If still some leftover - odd number of quotes on whole line - just add to end
            (if (fx> (string-length leftover) 0)
              (append newlst (list leftover))
              newlst)
          )
        )
      )
      ;; If no quotation marks on line - just return line as is
      lst
    )
  )
)

(define (csv-write file output)
  (let ((fh (open-output-file file)))
    (let lineloop ((res output))
      ;; If more lines
      (if (fx> (length res) 0) (begin
        (let cellloop ((line (car res)))
        ;; If no more cells on line, add new line
          (if (fx= (length line) 0) (display "\n" fh) (begin
            ;; Otherwise add current cell with comma following
            (display (car line) fh) (display "," fh)
            (cellloop (cdr line))
          ))
        )
        (lineloop (cdr res))
      ))
    )
    (close-output-port fh)
  )
)

;; unit tests
;; -----------------

;; 1. csv-write the given list
;; 2. csv-read it
;; 3. compare 
(define (csv-unit-test-list testlist)
  (let ((f (string-append (system-directory) (system-pathseparator) "csvtest.csv")))
      
      ;; Remove file if it exists
      (if (file-exists? f)
        (delete-file f))
      
      ;; Write and then read
      (csv-write f testlist)
      (let ((output (csv-read f)))
        (delete-file f)
        (equal? testlist output)))
)

;; 1. write the given string to a file
;; 2. csv-read it
;; 3. csv-write it
;; 4. read from the file into a string
;; 5. compare
(define (csv-unit-test-string inputstring outputstring)
  (let ((f (string-append (system-directory) (system-pathseparator) "csvtest.csv")))

    ;; Delete file if it already exists
    (if (file-exists? f)
      (delete-file f))

    (let ((fh (open-output-file f))
          (loadedlist #f)
          (loadedstring ""))

      ;; Output to the file
      (display inputstring fh)
      (close-output-port fh)

      ;; Read, delete the file, and then write
      (set! loadedlist (csv-read f))
      (delete-file f)
      (csv-write f loadedlist)

      ;; Read back as a string, should be no new lines in the file
      (with-input-from-file f (lambda ()
                                (let loop ((line (read-line (current-input-port) #\newline)) (first #t))
                                  (if (not (eof-object? line))
                                    (begin
                                      (set! loadedstring (if first line (string-append loadedstring "\n" line)))
                                      (loop (read-line) #f))))))

      ;; Delete file after
      (delete-file f)

      ;; Do comparison
      (equal? loadedstring outputstring)))
) 
  
;; 1. write the given string to a file
;; 2. csv-read it
;; 3. compare
(define (csv-unit-test-read teststring testlist)
  (let ((f (string-append (system-directory) (system-pathseparator) "csvtest.csv")))
    
     ;; Remove file if it exists
     (if (file-exists? f)
       (delete-file f))
      
     (let ((fh (open-output-file f)))

       ;; Output to the file
       (display teststring fh)
       (close-output-port fh)
    
       ;; Write and then read=
       (let ((output (csv-read f)))
         (delete-file f)
         (equal? testlist output))))
)
  
(unit-test "csv-write-read" "Quotation marks and line feed"
  (lambda ()
    (csv-unit-test-list csv:test_line_feed))
)

(unit-test "csv-write-read" "Quotation marks and carriage return"
  (lambda ()
    (csv-unit-test-string csv:test_carriage_return_input csv:test_carriage_return_output))
)

(unit-test "csv-write-read" "Trend File 1"
  (lambda () 
    (csv-unit-test-string csv:test_trend_file1 csv:test_trend_file1))
)

(unit-test "csv-write-read" "Trend File 2 Write-Read"
  (lambda () 
    (csv-unit-test-list csv:test_trend_list))
)

(unit-test "csv-write-read" "Trend File 2 Read"
  (lambda () 
    (csv-unit-test-read csv:test_trend_file2 csv:test_trend_list))
)
      



;; eof