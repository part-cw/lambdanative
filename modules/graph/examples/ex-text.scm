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
(graph-htextleft g 0.2 0. "htextleft")
(graph-htextright g -0.2 0. "htextright")
(graph-htextcenter g 0. -0.8 "htextcenter")
(graph-vtexttop  g 0 -0.2 "vtexttop")
(graph-vtextbottom  g 0 0.2 "vtextbottom")
(graph-vtextcenter g -0.8 0. "vtextcenter")

(graph-output g 'GRAPH_PDF "ex-text.pdf")
(graph-output g 'GRAPH_SVG "ex-text.svg")

;;eof
