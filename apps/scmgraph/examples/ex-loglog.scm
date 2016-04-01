(begin

;; the data to plot
(define data '((1.95 2.39) (0.99 0.82) (1.21 1.39) (1.97 2.23) (2.25 2.23)
(3.65 4.17) (2.84 2.85) (2.42 2.79) (1.34 0.78) (3.26 3.91) (1.67 2.49)
(3.59 2.84) (2.65 2.85) (1.56 2.55) (1.58 0.95) (2.33 2.07) (1.02 1.63)
(1.93 1.04) (1.32 2.75) (1.82 2.54) (1.2 1.68) (1.78 1.91) (1.4 1.55)
(2.37 1.89) (3.91 3.26) (2.91 2.6) (1.32 1.25) (3.65 3.12) (4.19 3.71)
(2.57 1.74) (3.35 1.99) (4.02 3.96) (4.32 5.13) (1.27 0.9) (2.5 2.59)
(4.21 4.81) (3.12 3.12) (4.17 4.69) (2.6 2.67) (1.56 2.08) (2.6 2.22)
(2.71 2.44) (1.8 1.56) (2.61 0.78) (1.39 1.11) (1.94 2.25) (1.85 1.75)
(0.56 0.99) (1.04 1.56) (2.08 3.12) (2.11 3.16) (3.12 3.04) (2.6 1.56)
(2.35 2.48) (3.11 2.74) (1.56 1.56) (2 2.13) (1.9 1.92) (4.56 4.56) (2.36 2.81)
(2.84 2.84) (3.55 4.26) (1.11 1.56) (6.61 6.61) (2.14 2.94)))

(define g (graph-new 330 290))

;; set the font
(graph-font g "Helvetica" 12)

;; set the origin
(graph-aorigin g 1.1 0.75)

;; set the style of the log axis
(graph-logstyle g #xb #x7ff 4 3 4 8)

;; initialize the log axis
(graph-xlog g 3.0 0.5 10. 0.0)
(graph-ylog g 3. 0.5 10. 0.0)

;; draw a domain mesh 
(graph-color g Grey)
(graph-mesh g)

;; draw some lines
(graph-color g Red)
(graph-linewidth g 0.6)

;; a solid diagonal line
(graph-moveto g 0.5 0.5)
(graph-lineto g 10. 10.)
(graph-stroke g)

;; lines
(define vLmin 0.5) (define vLmax 10.0)
(define vRmin 0.5) (define vRmax 10.0)
(define ratio 1.5)
(graph-moveto g (* vLmin ratio) vRmin)
(graph-lineto g vLmax (/ vRmax ratio))
(graph-moveto g vLmin (* vRmin ratio))
(graph-lineto g (/ vLmax ratio) vRmax)
(graph-stroke g)

(graph-linewidth g 1.0)

;; plot markers here
(let loop ((d data))
  (if (> (length d) 0) (begin
    (graph-color g White)
    (graph-marker g (car (car d)) (cadr (car d)) GRAPH_SOLIDTRIANGLE 10)
    (graph-color g Black)
    (graph-marker g (car (car d)) (cadr (car d)) GRAPH_OPENTRIANGLE 10)
    (loop (cdr d)))))

;; draw an axis frame
(graph-frame g)

;; draw the x and y axis
(graph-xaxis g)
(graph-yaxis g)

;; draw the axis labels
(graph-xlabel g "The X axis label")
(graph-ylabel g "The Y axis label")

g)
