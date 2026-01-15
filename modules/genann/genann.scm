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

(c-declare "#include <genann.h>")
(c-declare "#include <stdlib.h>")
(c-declare "#include <time.h>")
(c-declare "#include \"helpers.h\"")

(define genann-init
	(c-lambda (int int int int) (pointer void) "genann_init")
)

(define genann-free
	(c-lambda ((pointer void)) void "genann_free")
)

(define genannRun
	(c-lambda ((pointer void) (pointer double)) (pointer double) "genannRun")
)

(define genannTrain
	(c-lambda ((pointer void) (pointer double) int (pointer double) int double) void "genannTrain")
)

(define arrayPeek
	(c-lambda((pointer double) int) double "peek")
)

(define arrayFree
	(c-lambda ((pointer double)) int "arrayFree")
)

(define newArray
	(c-lambda (int) (pointer double) "newArray")
)

(define array-insert
	(c-lambda ((pointer double) int double) int "arrayInsert")
)

(define genann-save
	(c-lambda ((pointer void) char-string) int "genannSave")
)

(define genann-load
	(c-lambda (char-string) (pointer void) "genannLoad")
)

(define genann-randomize
	(c-lambda ((pointer void)) int "genannRandomize")
)

(define genann-free
	(c-lambda ((pointer void)) int "genannFree")
)

(define genann-copy
	(c-lambda ((pointer void)) (pointer void) "genannCopy")
)

(define show-array
	(c-lambda ((pointer double) int) int "showArray")
)

(define get-n-inputs
	(c-lambda ((pointer void)) int "getNInputs")
)

(define get-n-hidden-layers
	(c-lambda ((pointer void)) int "getNHiddenLayers")
)

(define get-n-hidden
	(c-lambda ((pointer void)) int "getNHidden")
)

(define get-n-outputs
	(c-lambda ((pointer void)) int "getNOutputs")
)

(define get-n-total-weights
	(c-lambda ((pointer void)) int "getNTotalWeights")
)

(define get-n-total-neurons
	(c-lambda ((pointer void)) int "getNTotalNeurons")
)

(define pseudo-get-weights
	(c-lambda ((pointer void)) (pointer double) "getWeights")
)

(define update-weight(lambda(net index value)
	(set! weights (pseudo-get-weights net))
	(array-insert weights index (/ value 1.))
))

(define get-weights(lambda(net)
	(set! weights (pseudo-get-weights net))
	(set! weightsLen (get-n-total-weights net))
	(set! ret (make-vector weightsLen))
	(let loop ((i 0))
		(when (< i weightsLen)
		(vector-set! ret i (arrayPeek weights i))
		(loop (+ i 1))) ret)	
))

(define pseudo-get-output
	(c-lambda ((pointer void)) (pointer double) "getOutput")
)

(define get-output(lambda(net)
	(set! outputs (pseudo-get-output net))
	(set! outputsLen (get-n-total-neurons net))
	(set! ret (make-vector outputsLen))
	(let loop ((i 0))
		(when (< i outputsLen)
		(vector-set! ret i (arrayPeek outputs i))
		(loop (+ i 1))) ret)	
))

(define pseudo-get-delta
	(c-lambda ((pointer void)) (pointer double) "getDelta")
)

(define get-delta(lambda(net)
	(set! delta (pseudo-get-delta net))
	(set! deltaLen (- (get-n-total-neurons net) (get-n-inputs net)))
	(set! ret (make-vector deltaLen))
	(let loop ((i 0))
		(when (< i deltaLen)
		(vector-set! ret i (arrayPeek delta i))
		(loop (+ i 1))) ret)	
))

(define random-seed!
	(c-lambda () int "randomSeed")
)

(define genann-train(lambda(net inputs outputs rate)
	(set! iLen (vector-length inputs))
	(set! inputArr (newArray iLen)) 
	(let loop ((i 0))
		(when (< i iLen)
		(array-insert inputArr i (/ (vector-ref inputs i) 1. ))
		(loop (+ i 1))))
	(set! oLen (vector-length outputs))
	(set! outputArr (newArray oLen))
	(let loop ((i 0))
		(when (< i oLen)
		(array-insert outputArr i (/ (vector-ref outputs i) 1. ))
		(loop (+ i 1))))
	(show-array inputArr iLen)
	(show-array outputArr oLen)
	(genannTrain net inputArr iLen outputArr oLen (/ rate 1.))
	(arrayFree inputArr)
	(arrayFree outputArr)
))

(define genann-run (lambda(net inputs)
	(define len (vector-length inputs))
	(define arr (newArray len))
	(let loop ((i 0))
		(when (< i len)
		(array-insert arr i (/ (vector-ref inputs i) 1.))
		(loop (+ i 1))))
	(set! outputs (genannRun net arr))
	(arrayFree arr)
	(set! outputsLen (get-n-outputs net))
	(set! ret (make-vector outputsLen))
	(let loop ((i 0))
		(when (< i outputsLen)
		(vector-set! ret i (arrayPeek outputs i))
		(loop (+ i 1))) ret)
))

(define train-to-test(lambda(net output)
	(set! len (vector-length output))
	(set! percentage 0)
	(let loop ((i 0))
	(when (< i 100000)
	(genann-train net (vector 0 0) (vector (vector-ref output 0)) .1)
	(genann-train net (vector 0 1) (vector (vector-ref output 1)) .1)
	(genann-train net (vector 1 0) (vector (vector-ref output 2)) .1)
	(genann-train net (vector 1 1) (vector (vector-ref output 3)) .1)
	(loop (+ i 1))))
	(if (= (round (vector-ref (genann-run net (vector 0 0)) 0)) (vector-ref output 0)) (set! percentage (+ percentage 1)))
	(if (= (round (vector-ref (genann-run net (vector 0 1)) 0)) (vector-ref output 1)) (set! percentage (+ percentage 1)))
	(if (= (round (vector-ref (genann-run net (vector 1 0)) 0)) (vector-ref output 2)) (set! percentage (+ percentage 1)))
	(if (= (round (vector-ref (genann-run net (vector 1 1)) 0)) (vector-ref output 3)) (set! percentage (+ percentage 1)))
	(display (round (vector-ref (genann-run net (vector 0 0)) 0))) (newline)
	(display (round (vector-ref (genann-run net (vector 0 1)) 0))) (newline)
	(display (round (vector-ref (genann-run net (vector 1 0)) 0))) (newline)
	(display (round (vector-ref (genann-run net (vector 1 1)) 0))) (newline)
	(display (* (/ percentage len) 100)) (display "% PASS!") (newline)
))

(define genann-test(lambda()
	(define net (genann-init 2 1 2 1))
	(display "--XOR Test--\n")
	(train-to-test net (vector 0. 1. 1. 0.))
	(display "--OR Test--\n")
	(train-to-test net (vector 0. 1. 1. 1.))
	(display "--AND Test--\n")
	(train-to-test net (vector 0. 0. 0. 1.))
	(genann-free net)	
))
