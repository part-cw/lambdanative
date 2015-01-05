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

;; misc sanity checks on formatted strings

(define (sanestring-hours s)
  (pregexp-match "^(?x: [0-9] | 1[0-9] | 2[0-3] )$" s))

(define (sanestring-small-hours-decimal s)
  (pregexp-match "^(?x: [0-9] | 1[0-9] | [0-9].[0-9]+ | 1[0-9].[0-9]+ )$" s))

;; hours HH
(define (sanestring-hh s)
  (pregexp-match "^(?x: [0-1][0-9] | 2[0-3] )$" s))

;; minutes MM
(define (sanestring-mm s)
  (pregexp-match "^(?x: [0-5][0-9] )$" s))

;; time of day HH:MM or H:MM
(define (sanestring-hh:mm s)
  (let* ((hh "(?x: [0-9] | [0-1][0-9] | 2[0-3] )")
         (mm "(?x: [0-5][0-9] )")
         (hh:mm (string-append "^" hh "\\:" mm "$")))
    (pregexp-match hh:mm s)))

;; date of birth YYYY-MM-DD
(define (sanestring-dob s)
  (let* ((yyyy "(?x: 19[1-9][0-9] | 20[0-9][0-9] )")
         (mm   "(?x: 0[0-9] | 1[0-2] )")
         (dd   "(?x: [0-2][0-9] | 3[0-1] )")
         (dob (string-append "^" yyyy "\\-" mm "\\-" dd "$")))
    (if (pregexp-match dob s)
      ;; Check that it is a valid date within each month
      (string=? s (seconds->string (string->seconds s "%Y-%m-%d") "%Y-%m-%d"))
      #f)))

;; ip address (from pregexp example)
(define (sanestring-ipaddr s)
  (let* ((n0-255 "(?x: \\d | \\d\\d  | [01]\\d\\d | 2[0-4]\\d  | 25[0-5]  )")
         (ip-re1 (string-append "^" n0-255 "(?x:" "\\." n0-255 ")"  "{3}" "$"))
         (ip-re (string-append "(?=.*[1-9])" ip-re1)))
    (pregexp-match ip-re s)))

;; canadian postal code
(define (sanestring-capostal s)
  (let* ((part1 "(?x: [a-zA-Z][0-9][a-zA-Z] )")
         (part2 "(?x: [0-9][a-zA-Z][0-9] )")
         (postal (string-append "^" part1 "\\s?" part2 "$")))
    (pregexp-match postal s)))

;; north american phone no (xxx-xxx-xxxx) 
(define (sanestring-naphone s)
  (let* ((npa "(?x: [2-9][0-9][0-9] )")
         (nxx "(?x: [2-9][0-9][0-9] )")  ;; this is not accurate as N11 is invalid
         (xxxx "(?x: [0-9][0-9][0-9][0-9] )")
         (phoneno (string-append "^" npa "\\-" nxx "\\-" xxxx "$")))
    (pregexp-match phoneno s)))

;; email address
(define (sanestring-email s)
  (pregexp-match "([a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-zA-Z0-9]+)" s))

;; human height in cm
(define (sanestring-height-cm s)
  (pregexp-match "^(?x: [3-9][0-9] | 1[0-9][0-9] | 2[0-2][0-9] )$" s))

;; human weight in kg
(define (sanestring-weight-kg s)
  (pregexp-match "^(?x: [2-9] | [1-9][0-9] | [1-2][0-9][0-9] )$" s))

(define (sanestring-alpha s)
  (pregexp-match "^([a-zA-Z\\s]+)$" s))

(define (sanestring-alphanum s)
  (pregexp-match "^([a-zA-Z0-9\\s]+)$" s))

(define (sanestring-num s)
  (pregexp-match "^([0-9]+)$" s))

(define (sanestring-nonempty s)
  (fx> (string-length s) 0))

;; eof     
