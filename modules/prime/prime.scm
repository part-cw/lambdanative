;; prime.scm -- prime number utilities
;; Copyright (c) 2004-2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(##namespace ("prime#"))
(##include "~~lib/gambit#.scm")
(##include "prime#.scm")

;;> Prime and number theoretic utilities.

;;> Returns a pair whose car is the power of 2 in the factorization of
;;> n, and whose cdr is the product of all remaining primes.
(define (factor-twos n)
  (do ((p 0 (+ p 1))
       (r n (arithmetic-shift r -1)))
      ((odd? r) (cons p r))))

;;> Returns the multiplicative inverse of \var{a} modulo \var{b}.
(define (modular-inverse a b)
  (let lp ((a1 a) (b1 b) (x 0) (y 1) (last-x 1) (last-y 0))
    (if (zero? b1)
        (if (negative? last-x) (+ last-x b) last-x)
        (let ((q (quotient a1 b1)))
          (lp b1 (remainder a1 b1)
              (- last-x (* q x)) (- last-y (* q y))
              x y)))))

;;> Returns (remainder (expt a e) m).
(define (modular-expt a e m)
  (let lp ((tmp a) (e e) (res 1))
    (if (zero? e)
        res
        (lp (remainder (* tmp tmp) m)
            (arithmetic-shift e -1)
            (if (odd? e) (remainder (* res tmp) m) res)))))

;;> Returns true iff n and m are coprime.
(define (coprime? n m)
  (= 1 (gcd n m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exact prime testing.

;; All primes under 1000.
(define prime-table
  '#(  2   3   5   7  11  13  17  19  23  29  31  37  41  43  47  53  59
      61  67  71  73  79  83  89  97 101 103 107 109 113 127 131 137 139
     149 151 157 163 167 173 179 181 191 193 197 199 211 223 227 229 233
     239 241 251 257 263 269 271 277 281 283 293 307 311 313 317 331 337
     347 349 353 359 367 373 379 383 389 397 401 409 419 421 431 433 439
     443 449 457 461 463 467 479 487 491 499 503 509 521 523 541 547 557
     563 569 571 577 587 593 599 601 607 613 617 619 631 641 643 647 653
     659 661 673 677 683 691 701 709 719 727 733 739 743 751 757 761 769
     773 787 797 809 811 821 823 827 829 839 853 857 859 863 877 881 883
     887 907 911 919 929 937 941 947 953 967 971 977 983 991 997 ))

;;> Returns true iff \var{n} is definitely prime.  May take an
;;> impossibly long time for large values.
(define (provable-prime? n)
  (if (or (even? n) (<= n 2))
      (= 2 n)
      (let ((limit (inexact->exact (ceiling (sqrt n)))))
        (define (by-twos d)
          (cond ((> d limit) #t)
                ((zero? (remainder n d)) #f)
                (else (by-twos (+ d 2)))))
        (let ((len (vector-length prime-table)))
          (let lp ((i 0))
            (if (>= i len)
                (by-twos (vector-ref prime-table (- len 1)))
                (let ((d (vector-ref prime-table i)))
                  (cond
                   ((> d limit) #t)
                   ((zero? (remainder n d)) #f)
                   (else (lp (+ i 1)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Probable primes.

(define (modular-root-of-one? twos odd a n neg1)
  ;; Returns true iff any (modular-expt a odd*2^i n) for i=0..twos-1
  ;; returns 1 modulo n.
  (let ((b (modular-expt a odd n)))
    (let lp ((i 0) (b b))
      (cond ((or (= b 1) (= b neg1)))  ; in (= b 1) case we could factor
            ((>= i twos) #f)
            (else (lp (+ i 1) (remainder (* b b) n)))))))

;;> Returns true if we can show \var{n} to be composite by finding an
;;> exception to the Miller Rabin lemma.
(define (miller-rabin-composite? n)
  (let* ((neg1 (- n 1))
         (factors (factor-twos neg1))
         (twos (car factors))
         (odd (cdr factors))
         ;; Each iteration of Miller Rabin reduces the odds by 1/4, so
         ;; this is a 1 in 2^40 probability of false positive,
         ;; assuming good randomness from SRFI 27 and no bugs, further
         ;; reduced by preliminary sieving.
         (fixed-limit 16)
         (rand-limit (if (< n 341550071728321) fixed-limit 20)))
    (let try ((i 0))
      (and (< i rand-limit)
           (let ((a (if (< i fixed-limit)
                        (vector-ref prime-table i)
                        (+ (random-integer (- n 3)) 2))))
             (or (not (modular-root-of-one? twos odd a n neg1))
                 (try (+ i 1))))))))

;;> Returns true if \var{n} has a very high probability (enough that
;;> you can assume a false positive will never occur in your lifetime)
;;> of being prime.
(define (probable-prime? n)
  (cond
   ((< n 1) #f)
   (else
    (let ((len (vector-length prime-table)))
      (let lp ((i 0))
        (if (>= i len)
            (not (miller-rabin-composite? n))
            (let ((x (vector-ref prime-table i)))
              (cond
               ((>= x n) (= x n))
               ((zero? (remainder n x)) #f)
               (else (lp (+ i 1)))))))))))

;;> Returns true iff \var{n} is prime.  Uses \scheme{provable-prime?}
;;> for small \var{n}, falling back on \scheme{probable-prime?} for
;;> large values.
(define (prime? n)
  (and (> n 1)
       (if (< n #e1e10)
           (provable-prime? n)
           (probable-prime? n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prime iteration and factorization

;;> Returns the nth prime, with 2 being the 0th prime.
(define (nth-prime i)
  (define (by-twos n j)
    (if (prime? n)
        (if (<= j 0) n (by-twos (+ n 2) (- j 1)))
        (by-twos (+ n 2) j)))
  (let ((len (vector-length prime-table)))
    (if (< i len)
        (vector-ref prime-table i)
        (by-twos (+ 2 (vector-ref prime-table (- len 1))) (- i len)))))

;;> Returns the first prime less than or equal to \var{n}, or #f if
;;> there are no such primes.
(define (prime-below n)
  (and (>= n 3)
       (let lp ((n (if (even? n) (- n 1) n)))
         (if (prime? n) n (lp (- n 2))))))

;;> Returns the first prime greater than or equal to \var{n}.  If the
;;> optional \var{limit} is given and not false, returns \scheme{#f}
;;> if no such primes exist below \var{limit}.
(define (prime-above n . o)
  (let ((limit (and (pair? o) (car o))))
    (let lp ((n (if (even? n) (+ n 1) n)))
      (cond
       ((and limit (>= n limit)) #f)
       ((prime? n) n)
       (else (lp (+ n 2)))))))

;;> Returns the factorization of \var{n} as a monotonically
;;> increasing list of primes.
(define (factor n)
  (cond
   ((negative? n)
    (cons -1 (factor (- n))))
   ((<= n 2)
    (list n))
   (else
    (let lp ((n n)
             (res (list)))
      (cond
       ((even? n)
        (lp (quotient n 2) (cons 2 res)))
       ((= n 1)
        (reverse res))
       (else
        (let lp ((i 3) (n n) (limit (inexact->exact (ceiling (sqrt n)))) (res res))
          (cond
           ((= n 1)
            (reverse res))
           ((> i limit)
            (reverse (cons n res)))
           ((zero? (remainder n i))
            (lp i (quotient n i) limit (cons i res)))
           (else
            (lp (+ i 2) n limit res))))))))))

;;> Returns the Euler totient function, the number of positive
;;> integers less than \var{n} that are relatively prime to \var{n}.
(define (totient n)
  (let ((limit (inexact->exact (ceiling (sqrt n)))))
    (let lp ((i 2) (count 1))
      (cond ((> i limit)
             (if (= count (- i 1))
                 (- n 1)                ; shortcut for prime
                 (let lp ((i i) (count count))
                   (cond ((>= i n) count)
                         ((= 1 (gcd n i)) (lp (+ i 1) (+ count 1)))
                         (else (lp (+ i 1) count))))))
            ((= 1 (gcd n i)) (lp (+ i 1) (+ count 1)))
            (else (lp (+ i 1) count))))))

;;> The aliquot sum s(n), equal to the sum of proper divisors of an
;;> integer n.
(define (aliquot n)
  (let ((limit (+ 1 (quotient n 2))))
    (let lp ((i 2) (sum 1))
      (cond ((> i limit) sum)
            ((zero? (remainder n i)) (lp (+ i 1) (+ sum i)))
            (else (lp (+ i 1) sum))))))

;;> Returns true iff \var{n} is a perfect number, i.e. the sum of its
;;> divisors other than itself equals itself.
(define (perfect? n)
  (and (> n 1) (= n (aliquot n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random prime generation

;;> Returns a random prime between \var{lo}, inclusive, and \var{hi},
;;> exclusive.
(define (random-prime lo hi)
  (if (> lo hi)
      (error "bad range: " lo hi))
  (let ((n (bitwise-ior 1 (+ lo (random-integer (- hi lo))))))
    (or (prime-above n hi)
        (prime-above lo n)
        (error "couldn't find prime between: " lo hi))))

;;> Variant of \scheme{random-prime} which ensures the result is
;;> distinct from \var{p}.
(define (random-prime-distinct-from lo hi p)
  (let ((q (random-prime lo hi)))
    (if (= q p)
        (random-prime-distinct-from lo hi p)
        q)))

;;> Returns a random integer less than \var{n} relatively prime to
;;> \var{n}.
(define (random-coprime n)
  (let ((init (+ 2 (random-integer (- n 1)))))
    (let lp ((m init))
      (cond ((>= m n)
             (let lp ((m (- init 1)))
               (cond
                ((<= m 1) (error "couldn't find coprime to " n))
                ((coprime? n m) m)
                (else (lp (- m 1))))))
            ((coprime? n m) m)
            (else (lp (+ m 1)))))))

;; eof
