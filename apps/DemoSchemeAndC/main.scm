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

;; Example of a few ways of integrating C code with Scheme and pass data between both

;; A simple function in C, which is declared right here
;; Note: The end-of-c-declare has to be on its own line.
(c-declare #<<end-of-c-declare

// prototype of the C side logging call
void log_c(char *);

int pow2 (int x) {
  // The shift by x is equal to the 2^x operation
  log_c("This is an example log entry from C");
  return 1<<x; 
}
end-of-c-declare
)
(define pow2 (c-lambda (int) int "pow2"))

;; A more complicated function, which we include from an outside file
(c-declare #<<end-of-c-declare
#include "rmse.c"
end-of-c-declare
)

;; RMSE function, which takes a list of numbers and a reference value
(define (rmse lst ref)
  ;; The c-lambda function takes three input arguments, which are created below.
  ;; scheme-object is the data type of the object, which is created by transforming 
  ;;   a list into a f64 vector. The cast to double* of the first input argument 
  ;;   [___arg1] is used to pass it to C. (map flo lst) makes sure that all list 
  ;;   elements are floating point as the user may pass fixed point numbers here.
  ;; int is the data type of the length of the list [___arg2]
  ;; double is the data type of the reference value [___arg3]
  ;;
  ;; double is the data type of the result which is obtained from ___result
  ((c-lambda (scheme-object int double) double
    "___result=rmse(___CAST(double*,___BODY_AS(___arg1,___tSUBTYPED)),___arg2,___arg3);")
    (list->f64vector (map flo lst)) (length lst) (flo ref))
)

;; Run the examples
(let ((a 3))
  (for-each display (list "2^" a "=" (pow2 a) "\n"))
  (newline)
)
(let ((a (list 1 2 3 4 3 2 3 -2 -1 -3))
      (b (list 0. 1. 0. 1. 0. 1.))
      (ref 0))
  (for-each display (list "The rmse of " a " with respect to " ref " is " (rmse a ref) "\n"))
  (for-each display (list "The rmse of " a " with respect to its mean ("
                          (flo (mean a)) ") is " (rmse a (mean a)) "\n"))
  (for-each display (list "The rmse of " b " with respect to " ref " is " (rmse b ref) "\n"))
  (for-each display (list "The rmse of " b " with respect to its mean (" 
                          (flo (mean b)) ") is " (rmse b (mean b)) "\n"))
)

;; eof  
