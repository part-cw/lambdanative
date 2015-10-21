;; rsa.scm -- RSA public key cryptography library
;; Copyright (c) 2014 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;; modified for LambdaNative 2015

(##namespace ("rsa#"))
(##include "~~lib/gambit#.scm")
(##include "rsa#.scm")

(##namespace ("" modular-expt modular-inverse random-prime
  random-prime-distinct-from string->u8vector
  u8vector->integer integer->u8vector))

(define bytevector->integer u8vector->integer)
(define integer->bytevector integer->u8vector)

(define make-bytevector make-u8vector)
(define bytevector-append u8vector-append)
(define bytevector-copy subu8vector)
(define bytevector-length u8vector-length)
(define bytevector-u8-ref u8vector-ref)
(define bytevector? u8vector?)

(define string->utf8 string->u8vector)

;; The RSA key type.  The public fields are always present, but the
;; private key d may be #f.
(define-record-type Rsa-Key
  (make-rsa-key bits n e d)
  rsa-key?
  (bits rsa-key-bits)
  (n rsa-key-n)         ; public modulus, the product of two primes
  (e rsa-key-e)         ; public exponent, coptime to (totient n)
  (d rsa-key-d))        ; private exponent, the inverse of e mod (totient n)

(define (rsa-key-gen-from-primes bit-length p q . o)
  (define (choose-exponent phi e)
    (cond ((>= e phi) (error "couldn't find an exponent for " p q))
          ((= 1 (gcd e phi)) e)
          (else (choose-exponent phi (+ e 2)))))
  (let* ((n (* p q))
         (phi (* (- p 1) (- q 1)))
         ;; Default to Fermat's number F4, or if too large the number
         ;; 3, as suggested by RFC 1423.  Ensure it's coprime to phi.
         (e (choose-exponent phi (cond ((pair? o) (car o))
                                       ((< 65537 phi) 65537)
                                       (else 3))))
         (d (modular-inverse e phi)))
    (make-rsa-key bit-length n e d)))

(define (rsa-key-gen . o)
  (let* ((bit-length (if (pair? o) (car o) 128))
         (lo (max 3 (expt 2 (- bit-length 1))))
         (hi (expt 2 bit-length))
         (p (random-prime lo hi))
         (q (random-prime-distinct-from lo hi p)))
    (rsa-key-gen-from-primes bit-length p q)))

;;> Returns a copy of the given key with the private key, if any,
;;> removed.
(define (rsa-pub-key priv-key)
  (make-rsa-key (rsa-key-bits priv-key) (rsa-key-n priv-key)
                (rsa-key-e priv-key) #f))

;; From RFC-1423
(define (pkcs1-pad bv)
  (let ((pad (- 8 (modulo (bytevector-length bv) 8))))
    (bytevector-append bv (make-bytevector pad pad))))

(define (pkcs1-unpad bv)
  (let* ((len (bytevector-length bv))
         (pad (bytevector-u8-ref bv (- len 1))))
    (if (not (<= 1 pad 8))
        (error "not pkcs1 padded" bv)
        (bytevector-copy bv 0 (- len pad)))))

;; Actual encryption and decryption are trivially defined as modular
;; exponentiation.

(define (rsa-encrypt-integer pub-key msg)
  (if (>= msg (rsa-key-n pub-key))
      (error "message larger than modulus" msg (rsa-key-n pub-key)))
  (modular-expt msg (rsa-key-e pub-key) (rsa-key-n pub-key)))

(define (rsa-decrypt-integer priv-key cipher)
  (if (>= cipher (rsa-key-n priv-key))
      (error "cipher larger than modulus"))
  (modular-expt cipher (rsa-key-d priv-key) (rsa-key-n priv-key)))

;; Arbitrary messages are encrypted by converting encoded bytevectors
;; to and from integers.
;; TODO: user emsa-pss encoding

(define (convert-plain f key msg)
  (cond
   ((bytevector? msg)
    (integer->bytevector (f key (bytevector->integer (pkcs1-pad msg)))))
   ((string? msg)
    (convert-plain f key (string->utf8 msg)))
   (else
    (f key msg))))

(define (convert-cipher f key cipher)
  (cond
   ((bytevector? cipher)
    (pkcs1-unpad (integer->bytevector (f key (bytevector->integer cipher)))))
   ((string? cipher)
    (convert-cipher f key (string->utf8 cipher)))
   (else
    (f key cipher))))

;; General API can handle integers, bytevectors, or strings which are
;; converted to utf8 bytevectors.

;;> Encrypts \var{msg} for the given public key \var{pub-key}.
;;> \var{msg} may be an integer or bytevector, in which case the
;;> result is of the same type, or a string, in which case the string
;;> is first coerced to a utf8 encoded bytevector.
(define (rsa-encrypt pub-key msg)
  (if (not (rsa-key-e pub-key))
      (error "can't encrypt without a public key" pub-key)
      (convert-plain rsa-encrypt-integer pub-key msg)))

;;> Decrypts \var{cipher} using the given private key \var{priv-key}.
;;> \var{cipher} may be an integer or bytevector, in which case the
;;> result is of the same type, or a string, in which case the string
;;> is first coerced to a utf8 encoded bytevector.
(define (rsa-decrypt priv-key cipher)
  (if (not (rsa-key-d priv-key))
      (error "can't decrypt without a private key" priv-key)
      (convert-cipher rsa-decrypt-integer priv-key cipher)))

;;> Signs \var{msg} using the given private key \var{priv-key}.
(define (rsa-sign priv-key msg)
  (if (not (rsa-key-d priv-key))
      (error "can't sign without a private key" priv-key)
      (convert-plain rsa-decrypt-integer priv-key msg)))

;;> Returns the verified (decrypted) message for the signature \var{sig}.
(define (rsa-verify pub-key sig)
  (if (not (rsa-key-e pub-key))
      (error "can't verify without a public key" pub-key)
      (convert-cipher rsa-encrypt-integer pub-key sig)))

;;> Returns true iff \var{sig} is a valid signature of \var{msg} for
;;> the given public key \var{pub-key}.
(define (rsa-verify? pub-key msg sig)
  (equal? (if (string? msg) (string->utf8 msg) msg)
          (rsa-verify pub-key sig)))

;; eof
