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

(unit-test "fft-1d" "1d FFTs"
  (lambda ()
    (set! part1 (equal? (fft (list 1 1 0 0))
                        '(2.+0.i 1.-1.i 0.+0.i 1.+1.i)))
    (set! part2 (equal? (fft (list 1 0 1 0 1 0 1 0))
                        '(4.+0.i 0.+0.i 0.+0.i 0.+0.i 4.+0.i 0.+0.i 0.+0.i 0.+0.i)))
    (set! part3 (equal? (ifft (list 1 1 0 0))
                        '(.5+0.i .25+.25i 0.+0.i .25-.25i)))
    (set! part4 (equal? (ifft (list 1 0 1 0 1 0 1 0))
                        '(.5+0.i 0.+0.i 0.+0.i 0.+0.i .5+0.i 0.+0.i 0.+0.i 0.+0.i)))
    (and part1 part2 part3 part4)
  ))

(unit-test "fft-2d" "2d FFTs"
  (lambda ()
    (set! part1 (equal? (fft2 (list (list 1 2 3 4 5 6 7 8)(list 1 2 3 4 5 6 7 8)
                                    (list 1 2 3 4 5 6 7 8)(list 1 2 3 4 5 6 7 8)
                                    (list 1 2 3 4 5 6 7 8)(list 1 2 3 4 5 6 7 8)
                                    (list 1 2 3 4 5 6 7 8)(list 1 2 3 4 5 6 7 8)))
                        '((288.+0.i -32.+77.25483703613281i
                           -32.+32.i -32.+13.254833221435547i
                           -32.+0.i  -32.-13.254833221435547i
                           -32.-32.i -32.-77.25483703613281i)
                           (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                            0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                           (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                            0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                           (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                            0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                           (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                            0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                           (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                            0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                           (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                            0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                           (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                            0.+0.i 0.+0.i 0.+0.i 0.+0.i))
                        ))
    (set! part3 (equal? (ifft2 (list (list 1 2 3 4 5 6 7 8)(list 1 2 3 4 5 6 7 8)
                                     (list 1 2 3 4 5 6 7 8)(list 1 2 3 4 5 6 7 8)
                                     (list 1 2 3 4 5 6 7 8)(list 1 2 3 4 5 6 7 8)
                                     (list 1 2 3 4 5 6 7 8)(list 1 2 3 4 5 6 7 8)))
                        '((4.5+0.i   -.5-1.2071068286895752i
                           -.5-.5i  -.5-.20710676908493042i
                           -.5+0.i  -.5+.20710676908493042i
                           -.5+.5i  -.5+1.2071068286895752i)
                          (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                           0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                          (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                           0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                          (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                           0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                          (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                           0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                          (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                           0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                          (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                           0.+0.i 0.+0.i 0.+0.i 0.+0.i)
                          (0.+0.i 0.+0.i 0.+0.i 0.+0.i 
                           0.+0.i 0.+0.i 0.+0.i 0.+0.i))
                       ))
    (and part1 part2)
  ))

;; eof
