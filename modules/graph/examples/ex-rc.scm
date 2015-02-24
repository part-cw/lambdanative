;; graph example
(define g (graph-new 430 320))

;; set the origin
(graph-aorigin g 1.2 0.9)

;; select the font
(graph-font g "Helvetica" 12)

;; set the style of the log axis
(graph-linearstyle g 4 0 5 1 5)

;; set the style of the log axis
;;(graph-logstyle g #xb #x7ff 4 3 4 8 0)
(graph-logstyle g 2 1022 4 1 7 8)

;; initialize the log axis
;; 4 inches wide, from 100 to 10000 Hz, offset -0.1 inch
(graph-xlog g 4.0 100. 10000. -0.1)

;; initialize the linear axis 
;; 3 inches high, from 0 to 1, offset -0.1 inch, 
(graph-ylinear g 3.0 0.0 1.0 -0.1 0.1 2)

;; draw a domain mesh 
(graph-color g Grey)
(graph-mesh g)

(let* ((R 1.0e6)(C 1e-10)
      (cnorm (lambda (f) (/ 1. (+ 1 (* +1.0i 2. 3.14 f R C))))))
  (graph-color g Red)
  (graph-moveto g 100. (real-part (cnorm 100.)))
  (let loop ((f 100.))
    (if (< f 10000.) (begin
      (graph-lineto g f (real-part (cnorm f)))
      (loop (+ f 10.)))))
  (graph-stroke g)
  (graph-color g Blue)
  (graph-moveto g 100. (- (imag-part (cnorm 100.))))
  (let loop ((f 100.))
    (if (< f 10000.) (begin
      (graph-lineto g f (- (imag-part (cnorm f))))
      (loop (+ f 10.)))))
  (graph-stroke g)
)

(graph-color g Black)

;; draw the x and y axis
(graph-xaxis g)
(graph-yaxis g)

;; draw the axis labels
(graph-ylabel g "Relative capacitance")
(graph-xlabel g "Frequency [Hz]")

;; draw labels to identify real and imaginary curves
(graph-color g Red)
(graph-htextleft g 1000. 0.8 "Real C")
(graph-color g Blue)
(graph-htextright g 400 0.3 "-Imag C")

;; draw a title
(graph-color g Black)
(graph-htextcenter g 1000 1.05 "Example: ex-rc.scm")

(graph-output g GRAPH_PDF "ex-rc.pdf")
(graph-output g GRAPH_SVG "ex-rc.svg")

;;eof
