(begin

(define g (graph-new 550 320))

;; set the origin
(graph-aorigin g 1.2 0.9)

;; select the font
(graph-font g "Helvetica" 12)

;; set the style of the log axis
(graph-linearstyle g 4 0 5 1 5)

;; initialize the axis
;; 6 inches wide, from -6 to 6, offset 0 inch, tick every 1., label every 2 tick
(graph-xlinear g 6.0 -6. 6. 0. 1. 2)

;; initialize the linear axis 
;; 3 inches high, from -0.3 to 1.1, offset 0. inch, tick every .1, label every 2 tick
(graph-ylinear g 3.0 -0.3 1.1 0.0 0.1 2)

;; draw a domain mesh 
(graph-color g Grey)
(graph-moveto g -6.0 0.) (graph-lineto g 6.0 0.)
(graph-moveto g 0. -0.3) (graph-lineto g 0 1.1)
(graph-stroke g)
(graph-mesh g)

;; draw the sinc function curves
(graph-color g Green)
(graph-moveto g -6. (sinc -6.))
(let loop ((x -6.))
    (if (< x 6.) (begin
      (graph-lineto g x (sinc x))
      (loop (+ x 0.1)))))
(graph-stroke g)
(graph-color g Red)
(graph-moveto g -6. (sinc -6.))
(let loop ((x -6.))
    (if (< x 6.) (begin
      (graph-lineto g x (sinc (* 1.5 x)))
      (loop (+ x 0.1)))))
(graph-stroke g)

(graph-color g Black)

;; draw the x and y axis
(graph-xaxis g)
(graph-yaxis g)

;; draw the axis labels
(graph-ylabel g "Sinc function")
(graph-xlabel g "X coordinate")

;; draw a caption in physical coordinates
(graph-setcoord g GRAPH_PHYS)
(graph-htextcenter g 3. 3.2 "Example: ex-sinc.scm")

g)
