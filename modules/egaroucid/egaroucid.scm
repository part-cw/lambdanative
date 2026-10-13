#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2014, University of British Columbia
Copyright (c) 2026, Benson Muite
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

;; bindings for the egaroucid library

(define egaroucid:debuglevel 0)
(define (egaroucid:log level . x)
   (if (>= egaroucid:debuglevel level) (apply log-system (append (list "egaroucid: ") x))))

(c-declare  #<<end-of-c-declare

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <egaroucid.h>

end-of-c-declare
)

(define EGAROUCID_OK ((c-lambda () int "___result = EGAROUCID_OK;")))
(define EGAROUCID_ERROR_INVALID_ARGUMENT ((c-lambda () int "___result = EGAROUCID_ERROR_INVALID_ARGUMENT;")))
(define EGAROUCID_ERROR_NOT_INITIALIZED ((c-lambda () int "___result = EGAROUCID_ERROR_NOT_INITIALIZED;")))
(define EGAROUCID_ERROR_SEARCH_FAILED ((c-lambda () int "___result = EGAROUCID_ERROR_SEARCH_FAILED;")))
(define EGAROUCID_ERROR_UNSUPPORTED ((c-lambda () int "___result = EGAROUCID_ERROR_UNSUPPORTED;")))

(define egaroucid:version (c-lambda () char-string "_egaroucid_version"))
(define egaroucid:init (c-lambda ((pointer void)) int "_egaroucid_global_init"))
(define egaroucid:create (c-lambda ((pointer void)) (pointer_void) "_egaroucid_create"))
(define egaroucid:destroy (c-lambda ((pointer void)) void "_egaroucid_destroy"))
(define egaroucid:search_array (c-lambda ((pointer void) (pointer void) int (pointer void) (pointer void)) int "_egaroucid_search_array"))
(define egaroucid:get-legal-moves (c-lambda ((pointer void) int (pointer void) (pointer void) (pointer void)) int "_egaroucid_get_legal_moves"))
(define egaroucid:get-flipped-discs (c-lambda ((pointer void) int int (pointer void) (pointer void) (pointer void)) int "_egaroucid_get_flipped_discs"))
(define egaroucid:stop (c-lambda ((pointer void)) void "_egaroucid_stop"))

;; eof
