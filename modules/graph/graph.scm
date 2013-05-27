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

;; vector graphing engine
;; this is a reimplementation of "Cgraph: PostScript plotting library in C" in scheme for PDF output
;; Original Source available at http://neurovision.berkeley.edu/software/A_Cgraph.html
;; "The Cgraph Library source code, examples, and documentation are in the public domain."

(include "./graph-ccode.scm")
(include "./graphout-axis.scm")
(include "./graphout-color.scm")
(include "./graphout-coord.scm")
(include "./graphout-core.scm")
(include "./graphout-dispatch.scm")
(include "./graphout-font.scm")
(include "./graphout-frame.scm")
(include "./graphout-label.scm")
(include "./graphout-marker.scm")
(include "./graphout-mesh.scm")
(include "./graphout-origin.scm")
(include "./graphout-relative.scm")
(include "./graphout-xlinear.scm")
(include "./graphout-xlog.scm")
(include "./graphout-ylinear.scm")
(include "./graphout-ylog.scm")

;; output types
(define GRAPH_PDF 1)

;; user coordinates
(define GRAPH_PHYS 0)
(define GRAPH_AXIS 1)

;; marker types
(define GRAPH_OPENCIRCLE 0)
(define GRAPH_SOLIDCIRCLE 1)
(define GRAPH_OPENBOX 2)
(define GRAPH_SOLIDBOX 3)
(define GRAPH_OPENTRIANGLE 4)
(define GRAPH_SOLIDTRIANGLE 5) 

;; internal defs
(define GRAPH:EVEN 2)
(define GRAPH:UP   1)
(define GRAPH:DOWN 0)
(define GRAPH:MINBIT 1)
(define GRAPH:MAXBIT 2048)

;; reset a graph data structure
(define (graph-reset t)
  (table-set! t 'axisenable 1)
  (table-set! t 'axisnumberenable 1)
  (table-set! t 'axisx2dev 72.)
  (table-set! t 'axisxmax 0.)
  (table-set! t 'axisxmax 0.)
  (table-set! t 'axisxmin 0.)
  (table-set! t 'axisy2dev 72.)
  (table-set! t 'axisymin 0.)
  (table-set! t 'coord GRAPH_PHYS)
  (table-set! t 'devxorigo 0.)
  (table-set! t 'devxpen 0.)
  (table-set! t 'devyorigo 0.)
  (table-set! t 'devypen 0.)
  (table-set! t 'fsizex 0)
  (table-set! t 'lastmovex 0.)
  (table-set! t 'lastmovey 0.)
  (table-set! t 'ldecx 0.)
  (table-set! t 'ldecy 0.)
  (table-set! t 'leastx 0.)
  (table-set! t 'leasty 0.)
  (table-set! t 'linnumdist 2)
  (table-set! t 'linnumoff 0)
  (table-set! t 'linticlen 5)
  (table-set! t 'lintnposition 2)
  (table-set! t 'lognumdist 2)
  (table-set! t 'lognumsel 1)
  (table-set! t 'logticlen 5)
  (table-set! t 'logticlen10 8)
  (table-set! t 'logticsel 0)
  (table-set! t 'logtnposition 2)
  (table-set! t 'logx #f)
  (table-set! t 'logy #f)
  (table-set! t 'phys2dev 72.)
  (table-set! t 'physxoffset 0.)
  (table-set! t 'physxsize 0.)
  (table-set! t 'physyoffset 0.)
  (table-set! t 'physysize 0.)
  (table-set! t 'precision 4)
  (table-set! t 'udecx 0.)
  (table-set! t 'udecy 0.)
  (table-set! t 'xcp 0)
  (table-set! t 'xfont 0)
  (table-set! t 'xnumsep 1)
  (table-set! t 'xstring "")
  (table-set! t 'xticks 0.)
  (table-set! t 'xtix 0)
  (table-set! t 'ycp 0)
  (table-set! t 'yfont 0)
  (table-set! t 'ynumsep 1)
  (table-set! t 'yticks 0.)
  (table-set! t 'ytix 0)
  (table-set! t 'path '())
  (table-set! t 'pathstack '())
  (table-set! t 'colorstack '())
  (table-set! t 'p2dcolor Black)
  (table-set! t 'commands (vector))
  (table-set! t 'cmdcount 0)
)

;; clear drawing constructs from a graph
(define (graph-clear t)
  (table-set! t 'path '())
  (table-set! t 'pathstack '())
  (table-set! t 'colorstack '())
  (table-set! t 'commands (vector))
  (table-set! t 'cmdcount 0)
)

;; create a new graph
(define (graph-new w h . xtra)
  (let ((t (make-table init: '()))
        (scale (if (= (length xtra) 1) (car xtra) 1.0)))
    (table-set! t 'isgraph "GRAPH")
    (table-set! t 'title "")
    (graph-reset t)
    (table-set! t 'axisx2dev (* (flo scale) 72.))
    (table-set! t 'axisy2dev (* (flo scale) 72.))
    (table-set! t 'phys2dev (* (flo scale) 72.))
    (table-set! t 'devxmin 0) (table-set! t 'devymin 0) 
    (table-set! t 'devxmax w) (table-set! t 'devymax h) t))

(define (graph? g)
  (if (table? g) (if (string? (table-ref g 'isgraph)) #t #f) #f))

;; general command call
(define (graph:cmd g c . x)
  (let ((old (table-ref g 'commands))
	(cnt (table-ref g 'cmdcount)))
    (if (>= cnt (vector-length old)) (begin (table-set! g 'commands (vector-append old (make-vector 1000 #f)))
					    (apply graph:cmd (append (list g c) x)))
       (begin (vector-set! old cnt (append (list c) x))
	      (table-set! g 'cmdcount (+ cnt 1))))))

;; all graph calls must be declared here 
(define (graph-rmoveto g x y) 			(graph:cmd g '@rmoveto (flo x) (flo y)))
(define (graph-rlineto g x y) 			(graph:cmd g '@rlineto (flo x) (flo y)))
(define (graph-moveto g x y) 			(graph:cmd g '@moveto (flo x) (flo y)))
(define (graph-lineto g x y) 			(graph:cmd g '@lineto (flo x) (flo y)))
(define (graph-closepathstroke g) 		(graph:cmd g '@closepathstroke))
(define (graph-stroke g) 			(graph:cmd g '@stroke))
(define (graph-linewidth g w)			(graph:cmd g '@linewidth w))
(define (graph-dash g t m)			(graph:cmd g '@dash t m))
(define (graph-rorigin g x y)			(graph:cmd g '@rorigin x y))
(define (graph-aorigin g x y)			(graph:cmd g '@aorigin x y))
(define (graph-setcoord g t)			(graph:cmd g '@setcoord t))
(define (graph-linearstyle g p o d t l)  	(graph:cmd g '@linearstyle p o d t l))
(define (graph-logstyle g a b c d e f)  	(graph:cmd g '@logstyle a b c d e f))
(define (graph-axisenable g a n)  		(graph:cmd g '@axisenable a n))
(define (graph-rgbcolor gh r g b) 		(graph:cmd gh '@rgbcolor r g b))
(define (graph-color g c) 			(graph:cmd g '@color c))
(define (graph-xlog g a b c d)			(graph:cmd g '@xlog a b c d))
(define (graph-ylog g a b c d)			(graph:cmd g '@ylog a b c d))
(define (graph-mesh g)				(graph:cmd g '@mesh))
(define (graph-font g fn fs)			(graph:cmd g '@font fn fs))
(define (graph-xlinear g a b c d e f) 		(graph:cmd g '@xlinear a b c d e f))
(define (graph-ylinear g a b c d e f) 		(graph:cmd g '@ylinear a b c d e f))
(define (graph-xaxis g) 			(graph:cmd g '@xaxis)) 
(define (graph-yaxis g) 			(graph:cmd g '@yaxis)) 
(define (graph-xlabel g v) 			(graph:cmd g '@xlabel v))
(define (graph-ylabel g v) 			(graph:cmd g '@ylabel v))
(define (graph-marker g x y t s) 		(graph:cmd g '@marker x y t s))
(define (graph-frame g)				(graph:cmd g '@frame))
(define (graph-htextleft g x y s)		(graph:cmd g '@htextleft x y s))
(define (graph-htextright g x y s)		(graph:cmd g '@htextright x y s))
(define (graph-htextcenter g x y s)		(graph:cmd g '@htextcenter x y s))
(define (graph-vtexttop g x y s)		(graph:cmd g '@vtexttop x y s))
(define (graph-vtextbottom g x y s)		(graph:cmd g '@vtextbottom x y s))
(define (graph-vtextcenter g x y s)		(graph:cmd g '@vtextcenter x y s))
(define (graph-line g x1 y1 x2 y2)		(graph:cmd g '@line x1 y1 x2 y2))
(define (graph-circle g x y r)	                (graph:cmd g '@circle x y r))
(define (graph-solidcircle g x y r)	   	(graph:cmd g '@solidcircle x y r))
(define (graph-box g x1 y1 x2 y2)		(graph:cmd g '@box x1 y1 x2 y2))
(define (graph-solidbox g x1 y1 x2 y2)		(graph:cmd g '@solidbox x1 y1 x2 y2))
(define (graph-triangle g x1 y1 x2 y2 x3 y3)    (graph:cmd g '@triangle x1 y2 x2 y2 x3 y3))
(define (graph-solidtriangle g x1 y1 x2 y2 x3 y3)    (graph:cmd g '@solidtriangle x1 y2 x2 y2 x3 y3))


;; %%%%%%%%%%%%%%%%%%%% MISC OUTPUT GENERATION
;; ------ code that only does parameter setup (device agnostic)...

(define (graphout:axisenable g axisflag numberflag)
  (table-set! g 'axisenable axisflag)
  (table-set! g 'axisnumberenable numberflag))

;; set the axis style 
(define (graphout:linearstyle g p o d t l)
  (table-set! g 'precision (if (< p 0) 0 (if (> p 10) 10 p)))
  (table-set! g 'linnumoff (if (< o 0) 0 (if (> o 19) 19 o)))
  (table-set! g 'linnumdist (if (< d 0) 0 (if (> d 100) 100 d)))
  (table-set! g 'lintnposition (if (< t 0) 0 (if (> t 6) 6 t)))
  (table-set! g 'linticlen (if (< l 0) 0 (if (> l 99) 99 l))))

(define (graphout:logstyle g ns ts nd tp tl tlten)
  (table-set! g 'lognumsel (if (< ns 0) 0 (if (> ns 2047) 2047 ns)))
  (table-set! g 'logticsel (if (< ts 0) 0 (if (> ts 2047) 2047 ts)))
  (table-set! g 'lognumdist (if (< nd 0) 0 (if (> nd 100) 100 nd)))
  (table-set! g 'logtnposition (if (< tp 1) 1 (if (> tp 6) 6 tp)))
  (table-set! g 'logticlen (if (< tl 1) 1 (if (> tl 99) 99 tl)))
  (table-set! g 'logticlen10 (if (< tlten 1) 1 (if (> tlten 99) 99 tlten))))

;; select the coordinate system (points or inches)
(define (graphout:setcoord g cflag)
  (table-set! g 'coord cflag))

;; %%%%%%%%%%%% MAIN OUTPUT PROCESSOR below..    

(define (graph-output g type . xtra)
  (let ((filename (if (= (length xtra) 1) (car xtra) "graph.out")))
  (cond 
     ((= type GRAPH_PDF) (graphout:pdf g filename))
     (else (log-error "graph-output: unsupported output format.")))))

;; eof
