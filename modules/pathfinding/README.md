This is a simple A* pathfinder for use with Lambdanative.  It depends on [this heap implementation](https://github.com/VincentToups/gambit-heap) or any heap which has the same interface, and should be relatively close to portable R5RS Scheme.

More documentation to come.

Usage
-----

    (load "modules/heap/heap.scm")
    (load "modules/pathfinding/pathfinding.scm")
    
    
    (define g (make-grid 18 18))
    (grid-forbid! g 11 10 12 10 12 11)
    (let loop ((runs (+ 4 (random-integer 5))
               ))
      (if (> runs 0)
         (begin 
           (grid-forbid-run! 
            g
            (random-integer 18)
            (random-integer 18)
            (select-one-randomly 
        (vector grid-north grid-south grid-east grid-west))
            (+ 2 (random-integer 4)))
           (loop (- runs 1)))
         runs))
    (set! path (a*! 0 0 17 17 g))
    (display "Path: " path) (newline)
    (display-grid-and-path g path)
    (newline)

Should print something like:

    (Path:  ((0 0) (0 1) (0 2) (0 3) 
             (1 3) (2 3) (2 4) (2 5) 
             (2 6) (3 6) (4 6) (5 6) 
             (6 6) (7 6) (8 6) (9 6) 
             (9 7) (9 8) (9 9) (9 10) 
             (9 11) (9  12) (9 13) 
             (9 14) (10 14) (11 14) 
             (12 14) (13 14) (14 14) 
             (14 15) (14 16) (15 16) 
             (16 16) (17 16) (17 17)))
    +.............X...
    +....X........X...
    +....X........X...
    +++..X............
    ..+..X............
    ..+..X............
    ..++++++++........
    ..XXXXX..+..X.....
    .........+..X.....
    .........+..X.....
    .........+.XX.....
    .........+..X.....
    .XX......+........
    .......XX+........
    .........++++++...
    ..............+...
    ..............++++
    .................+


Where `+` marks the path, `.` marks empty spaces, and `X` marks walls or otherwise impassable nodes.