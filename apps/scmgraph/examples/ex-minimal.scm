(begin

;; Translated from CGraph example by Izumi Ohzawa.

(define g (graph-new 216 216))
(graph-aorigin g 1.5 1.5)
(let loop ((i 0))
  (if (<= i 200) 
     (let* ((angle (* 3.14 i 0.01))
           (x (* 1.2 (cos (* 3. angle))))
           (y (* 1.2 (sin (* 5. angle)))))
       (if (= i 0) (graph-moveto g x y) (graph-lineto g x y))
       (loop (+ i 1)))))
(graph-closepathstroke g)

g)
