#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2015, University of British Columbia
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

;; Trusted timestamps
(include "shasum.scm")
(include "query.scm")
(include "reply.scm")
(include "submit.scm")
(include "verify.scm")

;; perform timestamp and return (shasum tsr) list
(define (timestamp-data filename)
  (if (and (file-exists? filename) (eq? (file-type filename) 'regular)
           (fx> (file-info-size (file-info filename)) 0))
    (let* ((csum (sha512sum filename))
           (tsq (timestamp-tsq-generate-sha512sum csum))
           (tsr (timestamp-tsr-request tsq)))
      (if (and tsq tsr (timestamp-tsr-granted? tsr)
            (fx= (timestamp-tsq-getnonce tsq) (timestamp-tsr-getnonce tsr))
            (equal? (timestamp-tsq-getmessage tsq) (timestamp-tsr-getmessage tsr)))
        (list csum tsr) #f)) #f))

;; perform timestamp and save+return tsr
(define (timestamp-gettimestamp filename)
  (let ((res (timestamp-data filename)))
    (if res (if (timestamp-tsr-save filename (cadr res)) (cadr res) #f) #f)))

;; eof
