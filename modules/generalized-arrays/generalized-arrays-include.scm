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
