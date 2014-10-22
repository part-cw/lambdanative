;; graph example
(define g (graph-new 550 320))

;; set the origin
(graph-aorigin g 1.2 0.9)

;; select the font
(graph-font g "Helvetica" 12)

;; set the style of the log axis
(graph-linearstyle g 4 0 5 1 5)

;; initialize the axis
;; 6 inches wide, from -6 to 6, offset 0 inch, tick every 1., label every 2 tick
(graph-xlinear g 3.0 -1. 1. 0. .2 2)

;; initialize the linear axis 
;; 3 inches high, from -0.3 to 1.1, offset 0. inch, tick every .1, label every 2 tick
(graph-ylinear g 3.0 -1. 1. 0. .2 2)

;; draw a domain mesh 
(graph-color g Grey)
(graph-mesh g)

;; draw the text examples
(graph-color g Black)
(graph-line g 0. 0.0 1. -1.)
(graph-color g Blue)
(graph-box g -0.9 -0.8 0.9 0.8)
(graph-color g Red)
(graph-solidbox g -0.8 -0.8 -0.6 -0.6)
(graph-color g Yellow)
(graph-solidcircle g 0.2 0.2 20.)
(graph-color g Green)
(graph-circle g 0.2 0.2 20.)
(graph-color g Orange)
(graph-triangle g -0.8 0.2 -0.5 0.8 -0.2 0.1)
(graph-color g CadetBlue)
(graph-solidtriangle g 0.8 0.2 0.5 0.8 0.2 0.1)

;; test closepath
(graph-color g Red)
(graph-moveto g -0.5 -0.5)
(graph-lineto g 0.5 -0.5)
(graph-lineto g 0.5 0.5)
(graph-lineto g -0.5 0.5)
(graph-closepathstroke g)

(graph-output g GRAPH_PDF "ex-primitives.pdf")
(graph-output g GRAPH_SVG "ex-primitives.svg")

;;eof
