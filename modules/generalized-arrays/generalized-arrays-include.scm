#|
Copyright 2020 Bradley J Lucier.
All Rights Reserved.

Permission is hereby granted, free of charge,
to any person obtaining a copy of this software
and associated documentation files (the "Software"),
to deal in the Software without restriction,
including without limitation the rights to use, copy,
modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit
persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice
(including the next paragraph) shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF
ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
|#

(cond-expand
 (gambit-c

  ;; These are routines used internally in generalized-arrays.scm
  ;; that are defined in gambit-4_9_2 but not gambc-4_7_9.

  ;; They're in a separate include file because we need them
  ;; at compile time in macros, so this file is also included
  ;; in the macros.

  ;; From SRFI-1
  
  (define (iota count #!optional (start 0) (step 1))
    (let loop ((i count) (result '()))
      (if (> i 0)
          (let ((i (- i 1)))
            (loop i (cons (+ start (* step i)) result)))
          result)))

  (define (take x i)
    (let loop ((probe x)
               (j i)
               (rev-result '()))
      (if (> j 0)
          (loop (if (pair? probe) (cdr probe) (error "take: short list" x i))
                (- j 1)
                (cons (car probe) rev-result))
          (reverse rev-result))))

  (define (drop x i)
    (let loop ((probe x)
               (j i))
      (if (> j 0)
          (loop (if (pair? probe) (cdr probe) (error "drop: short list" x i))
                (- j 1))
          probe)))

  ;; From later Gambits
  
  (define (exact-integer? obj)
    (and (integer? obj)
         (exact? obj)))
  )
 (else))
