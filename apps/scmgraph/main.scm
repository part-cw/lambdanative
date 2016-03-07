#|
LambdaNative - a cross-platform Scheme framework
Copyright (c) 2009-2016, University of British Columbia
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

;; Scientific Vector Graphs in Scheme

(define scmgraph:debuglevel 10)
(define (scmgraph:log level . x)
   (if (>= scmgraph:debuglevel level)
     (for-each display (append (list "LOG: ") x (list "\n")))))
(define (scmgraph:error . x) (apply scmgraph:log (append (list  0 "**ERROR: ") x)) (exit))

(define (usage)
  (for-each display (list
    "Scheme Graph\n"
    "usage: scmgraph [svg|pdf] <scheme graph specification>\n"
  ))
  (exit))

(define args (let loop ((n 0)(res '()))
  (if (= n (system-cmdargc)) res (loop (+ n 1) (append res (list (system-cmdargv n)))))))

(if (< (length args) 2) (usage))

(define use-pdf (member "pdf" args))
(define use-svg (member "svg" args))
(define use-repl (member "repl" args))

(set! sourcefile (car (reverse args)))

(if (not (file-exists? sourcefile)) (scmgraph:error sourcefile " not found"))

(if (or (< (string-length sourcefile) 5)
      (not (string=? (substring sourcefile
        (- (string-length sourcefile) 3) (string-length sourcefile)) "scm"))
     )
    (scmgraph:error sourcefile " invalid file extension (not .scm)"))

(let* ((strlen (string-length sourcefile))
       (basename (substring sourcefile 0 (- strlen 4))))
  (set! targetfile (string-append basename 
    (if use-svg ".svg" ".pdf")))
)

(scmgraph:log 1 "Scheme Graph")

(scmgraph:log 1 "Loading source graph from " sourcefile "..")
(define graph (eval (with-input-from-file sourcefile (lambda () (read)))))

(define graph-type (cond
  (use-pdf 'GRAPH_PDF)
  (use-svg 'GRAPH_SVG)
  (else 'GRAPH_PDF)))

(scmgraph:log 1 "Rendering graph to " targetfile "..")
(graph-output graph graph-type targetfile)

(if use-repl (begin
(scmgraph:log 1 "Entering debug repl..")
(let loop () 
  (with-exception-catcher (lambda (e) 
    (for-each display (list (exception->string e) "\n")) #f) 
      (lambda () (##repl-debug)))
  (loop))
))

(scmgraph:log 1 "Done.")

;; eof  
