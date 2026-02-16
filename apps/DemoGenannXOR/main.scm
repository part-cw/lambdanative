#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2025, John Agoe
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
(display "Genann XOR Test")(newline)
(set! net (genann-init 2 1 2 1))
;(display net)
(random-seed!)
(genann-randomize net)
;(genann-save net "neural.net")
;(define net2 (genann-copy net))
;(genann-save net2 "neural2.net")
(genann-test)
(newline) (newline) (newline)
;;xor trial training
(display "BEFORE TRAINING")
(newline)
(display "0   0  =>  ")
(display (round (vector-ref (genann-run net (vector 0 0)) 0))) (newline)
(display "0   1  =>  ")
(display (round (vector-ref (genann-run net (vector 0 1)) 0))) (newline)
(display "1   0  =>  ")
(display (round (vector-ref (genann-run net (vector 1 0)) 0))) (newline)
(display "1   1  =>  ")
(display (round (vector-ref (genann-run net (vector 1 1)) 0))) (newline) (newline)
(display "Training using backward propagation for 10000 iterations!...") (newline) (newline)
(let loop ((i 0))
  (when (< i 10000)
    (genann-train net (vector 0 0) (vector 0) .3)
    (genann-train net (vector 0 1) (vector 1) .3)
    (genann-train net (vector 1 0) (vector 1) .3)
    (genann-train net (vector 1 1) (vector 0) .3)
    (loop (+ i 1)))) (newline)
(display "AFTER TRAINING") (newline)
(display "0   0  =>  ")
(display (round (vector-ref (genann-run net (vector 0 0)) 0))) (newline)
(display "0   1  =>  ")
(display (round (vector-ref (genann-run net (vector 0 1)) 0))) (newline)
(display "1   0  =>  ")
(display (round (vector-ref (genann-run net (vector 1 0)) 0))) (newline)
(display "1   1  =>  ")
(display (round (vector-ref (genann-run net (vector 1 1)) 0))) (newline)
(genann-free net)
