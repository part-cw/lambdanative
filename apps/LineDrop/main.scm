
(include "linepatterns.scm")

;; GUI components
(define gui #f)
(define keypad #f)
(define name_label #f)
(define name_field #f)
(define next_label #f)
(define level_label #f)
(define level_number #f)
(define score_label #f)
(define score_number #f)
(define startbutton #f)
(define high_label #f)
(define scorelist #f)
(define playagain #f)
(define gameover #f)

;; Constants about the layout
(define LEFTSIDE 10)
(define BOTTOMSIDE 10)
(define RIGHTSIDE 310)
(define TOPSIDE 340)
(define SQUAREW 30)
(define SQUAREWH 15)
(define SQUAREH 30)
(define SQUAREHH 15)
(define PIECEW 1)
(define PIECEH 3)
(define GRIDW 10)
(define GRIDH 10)

;; Constants about the state of the game
(define NOTPLAYING 0)
(define FALLING 1)
(define MATCHFOUND 2)
(define BREAKING 3)
(define LEVELUP 4)

;; Last time the piece was moved
(define state 0)
(define move_time #f)

;; The list of squares being removed and timings, how fast squares are removed 
(define removesquares #f)
(define colourlines 0.4)
(define removelines 0.8)
;; A flag to be set to trigger checking for an additional path at the next move
(define pathcheck #f)

;; Word to spell out with each level
(define goal_word "{")

;; Goal square line patterns to match to go the next level, actual squares found that do match,
;; and the timing - how long to show matched pattern
(define goal_lines pattern_i)
(define goal_squares_found '())
(define showgoalmatch 0.7)

;; Location of the bottom square of the falling piece in the grid
(define next_lines #f)
(define piece_x #f)
(define piece_y #f)
(define piece_lines (list (list #f #f #f #f) (list #f #f #f #f) (list #f #f #f #f)))

;; All possible line combinations that we want to include in the game, no pieces with just one line
(define line_combos (list (list #t #t #t #t) (list #f #t #t #t) (list #t #f #t #t) (list #t #t #f #t) (list #t #t #t #f)
                         (list #f #f #t #t) (list #t #f #f #t) (list #t #t #f #f) (list #f #t #t #f) (list #f #t #f #t) (list #t #f #t #f)))

;; A list of all the squares that are currently filled. Each piece is an inner list of the x, y coordinates 
;; followed by true/false flags for each direction in a clockwise order of up, right, down, left
(define filled '())

;; The current level and score
(define level 0)
(define score 0)

;; The number of seconds between movements of the piece
(define falltime 1.0)

;; Procedures for getting the lines of a piece
(define (get-up sq) (caddr sq))
(define (get-right sq) (cadddr sq))
(define (get-down sq) (list-ref sq 4))
(define (get-left sq) (list-ref sq 5))

;; Start a new falling piece, use the next piece line patterns unless there isn't one
;; and then create a new next piece for the next spawn
(define (spawn-piece)
  
  ;; If no next_lines yet, game must just have started, create next lines now
  (if (not next_lines)
    (set! next_lines (list (random-lines-selection) (random-lines-selection) (random-lines-selection))))
  
  ;; Start the piece at the top
  (set! piece_x (floor (/ GRIDW 2)))
  (set! piece_y GRIDH)
  (set! move_time ##now)
  
  ;; The next piece becomes the current piece
  (set! piece_lines next_lines)
  
  ;; Get a new piece with random line patterns on each square
  (set! next_lines (list (random-lines-selection) (random-lines-selection) (random-lines-selection)))
  
  (if (get-square piece_x piece_y)
    (game-over))
)

;; Randomly returns a set of lines which is a list of four true and false values
;; This is one of the line_combos
(define (random-lines-selection)
  (list-ref line_combos (random-integer (- (length line_combos) 1)))
)

;; Cycle the line patterns on the squares within the piece
(define (cycle-lines)
   (set! piece_lines (append (cdr piece_lines) (list (car piece_lines))))
)

;; Check for the goal being met. If the goal is found, go to the next level
(define (check-for-goal)
  
;;  (level-up)
  (let ((goalsquares (get-goal-squares)))
    (if (fx> (length goalsquares) 0)
      ;; If the goal has actually been found, mark this so it can be shown
      ;; for a short while before the next level
      ;; add 2 * the number of goal sqaures
      ;; to the score and go to the next level
      (begin
        (set! goal_squares_found goalsquares)
        (set! state LEVELUP))))
)
  
;; Gets the filled squares that match the current goal pattern.
;; Returns these or an empty list if the pattern is not in the filled squares  
(define (get-goal-squares)
  
  ;; Go through all the filled squares and see if they match the goal pattern
  (let loop ((f filled))
    (if (fx> (length f) 0)
      (let ((sq (car f)))
        ;; If the first square is the goal has lines that match this square
        (if (line-match (caar goal_lines) sq)
          (let rloop ((y (cadr sq)) (linerows goal_lines) (psquares (list)))
             (if (fx> (length linerows) 0)
               (let sloop ((x (car sq)) (linesquares (car linerows)) (psq psquares))
                  (if (fx> (length linesquares) 0)
                    (let ((nextsq (get-square x y)))
                       (if (and nextsq (line-match (car linesquares) nextsq))
                         ;; This square is a match, go to the next one
                         (sloop (+ x 1) (cdr linesquares) (append psq (list nextsq)))
                         ;; This square does not match, so go to next first square
                         (loop (cdr f))))
                    ;; After the entire row, go to the next one above it
                    (rloop (+ y 1) (cdr linerows) psq)))
               ;; If it has made it all way here then the whole goal is matched, return the list of squares
               psquares))
          ;; The first square is not a match, go to the next one
          (loop (cdr f))))
      ;; Done going through all squares, return empty list, none mut have matched so return empty list
      (list)))
)
        
;; Returns true if the filledlines contain all the goallines,
;; otherwise return false
(define (line-match goallines filledlines)
  (let ((up (car goallines))
        (right (cadr goallines))
        (down (caddr goallines))
        (left (cadddr goallines)))
    (and (or (not up) (get-up filledlines))
         (or (not right) (get-right filledlines))
         (or (not down) (get-down filledlines))
         (or (not left) (get-left filledlines))))
)

;; Check for the piece touching the bottom or already filled in squares
(define (check-for-contact)
  
  ;; Check if the piece is at the bottom
  (if (fx= piece_y 0)
    ;; The piece has touched down, add it to the filled squares and being a new piece at the top
    (begin
      (set! filled (append filled (list (append (list piece_x piece_y) (car piece_lines))
                                        (append (list piece_x (+ piece_y 1)) (cadr piece_lines))
                                        (append (list piece_x (+ piece_y 2)) (caddr piece_lines)))))
      
      ;; Check for meeting the goal - going to the next level
      (check-for-goal)
      ;; Check for a path across being made
      (check-for-paths)
      ;; Spawn the next piece
      (spawn-piece))
    ;; Otherwise, go through all current filled pieces and see if one is directly under the current piece location
    (let loop ((sq filled))
      (if (fx> (length sq) 0)
        (begin
          (if (and (fx= (caar sq) piece_x) (fx= (cadar sq) (- piece_y 1)))
            ;; The piece has touched down, add it to the filled squares and being a new piece at the top
            (begin
              (set! filled (append filled (list (append (list piece_x piece_y) (car piece_lines))
                                                (append (list piece_x (+ piece_y 1)) (cadr piece_lines))
                                                (append (list piece_x (+ piece_y 2)) (caddr piece_lines)))))
              ;; Check for meeting the goal - going to the next level
              (check-for-goal)
              ;; Check for a path across being made
              (check-for-paths)
              ;; Spawn the next piece
              (spawn-piece)))
          (loop (cdr sq))))))
)

;; Returns the square at the current location or 
;; false if the square is empty (not yet filled by a fallen piece)
(define (get-square x y)
  
  (let loop ((sq filled))
     (if (fx> (length sq) 0)
       (if (and (fx= (caar sq) x) (fx= (cadar sq) y))
         (car sq)
         (loop (cdr sq)))
       #f))
)
  
;; Draw the goal squares
(define (draw-goal-squares)
  
  ;; Only if we are currently playing
  (if (not (fx= state NOTPLAYING))
    (let ((x 240)
          (y (+ TOPSIDE 15)))
      ;; Draw the white background and then the lines on it
      (glgui:draw-box x y (* SQUAREWH (length (car goal_lines))) (* SQUAREHH (length goal_lines)) White)
      
      ;; Draw the lines each of the rows in the goal squares
      (let rloop ((py y) (linerows goal_lines))
         (if (fx> (length linerows) 0)
           (let loop ((px x) (linesquares (car linerows)))
             (if (fx> (length linesquares) 0)
               (begin
                 (glgui:draw-box px py SQUAREWH SQUAREHH Grey)
                 (glgui:draw-box (+ px 1) (+ py 1) (- SQUAREWH 2) (- SQUAREHH 2) White)
                 (apply draw-square-lines-mini (append (list px py) (car linesquares) (list Blue)))
                 (loop (+ px SQUAREWH) (cdr linesquares)))
               (rloop (+ py SQUAREHH) (cdr linerows))))))))
)

;; Draw the next piece above the grid
(define (draw-next-piece)
  
  ;; If there is a current piece
  (if (not (fx= state NOTPLAYING))
    (let ((x 43)
          (y (+ TOPSIDE 15)))
      ;; Draw the white background and then the lines on it
      (glgui:draw-box x y (* SQUAREWH PIECEW) (* SQUAREHH PIECEH) White)
      
      ;; Draw the lines on the next piece
      (let loop ((px x) (py y) (linesets next_lines))
          (if (fx> (length linesets) 0)
            (begin
              (apply draw-square-lines-mini (append (list px py) (car linesets) (list Blue)))
              (loop px (+ py SQUAREHH) (cdr linesets)))))))
)
    

;; Draw the piece that is currently falling
(define (draw-current-piece)
  
  ;; If there is a current piece
  (if piece_x
    (begin
      
      (if (and (fx> piece_y 0) (> (- ##now move_time) falltime))
        (begin
          (set! move_time ##now)
          (set! piece_y (- piece_y 1))
          
          ;; If a path was just removed, check for another one
          (if pathcheck
            (begin
              (check-for-paths)
              (set! pathcheck #f)))))

      (let ((x (+ LEFTSIDE (* piece_x SQUAREW)))
            (y (+ BOTTOMSIDE (* piece_y SQUAREH))))
        ;; Draw background of piece
        (glgui:draw-box x y (min (* SQUAREW PIECEW) (- RIGHTSIDE x)) (min (* SQUAREH PIECEH) (- TOPSIDE y)) White)
        
        ;; Draw the lines on each square of the piece that is onscreen
        (let loop ((px x) (py y) (linesets piece_lines))
          (if (fx> (length linesets) 0)
            (begin
              (if (fx< py TOPSIDE)
                (apply draw-square-lines (append (list px py) (car linesets) (list Blue))))
              (loop px (+ py SQUAREH) (cdr linesets))))))))
)

;; Draws lines in the given directions with the given color on the square at the given coordinates
(define (draw-square-lines x y top right bottom left color)
  
  ;; Draw all lines
  (if top
    ;; Top piece top line
    (glgui:draw-box (- (+ x SQUAREWH) 1) (+ y SQUAREHH) 2 SQUAREHH color))
  (if right
    ;; Top piece right line
    (glgui:draw-box (+ x SQUAREWH) (- (+ y SQUAREHH) 1) SQUAREWH 2 color))
  (if bottom
    ;; Top piece bottom line
    (glgui:draw-box (- (+ x SQUAREWH) 1) y 2 SQUAREHH color))
  (if left
    ;; Top piece left line
    (glgui:draw-box x (- (+ y SQUAREHH) 1) SQUAREWH 2 color))
)

;; Draws the lines of the given square which is part of the goal
;; that was just matched. Determine what part of the goal it is and
;; draw only the lines in the goal in a different colour
(define (draw-goal-square-lines x y sq)
  
        ;; First get the index of this square within the goal squares
  (let* ((index (- (length goal_squares_found) (length (member sq goal_squares_found))))
         ;; Then find the matching goal square lines (may need to go through multiple rows)
         (glines (let rloop ((c index) (rows goal_lines))
                   (if (fx> (length rows) 0)
                     (let loop ((c1 c) (lines (car rows)))
                       (if (fx> (length lines) 0)
                         (if (fx= c1 0)
                           (car lines)
                           (loop (- c1 1) (cdr lines)))
                         (rloop c1 (cdr rows))))
                     ;; Should never got to end of rows, but if we do just return set of falses, so none are coloured in
                     (list #f #f #f #f))))
         (top (get-up sq))
         (right (get-right sq))
         (bottom (get-down sq))
         (left (get-left sq)))
    ;; Draw all lines
    (if top
      ;; Top piece top line
      (glgui:draw-box (- (+ x SQUAREWH) 1) (+ y SQUAREHH) 2 SQUAREHH (if (car glines) Green Blue)))
    (if right
      ;; Top piece right line
      (glgui:draw-box (+ x SQUAREWH) (- (+ y SQUAREHH) 1) SQUAREWH 2 (if (cadr glines) Green Blue)))
    (if bottom
      ;; Top piece bottom line
      (glgui:draw-box (- (+ x SQUAREWH) 1) y 2 SQUAREHH (if (caddr glines) Green Blue)))
    (if left
      ;; Top piece left line
      (glgui:draw-box x (- (+ y SQUAREHH) 1) SQUAREWH 2 (if (cadddr glines) Green Blue))))
)

;; A mini version of the above procedure, all lines are only drawn at half the size
(define (draw-square-lines-mini x y top right bottom left color)
  
  (let ((swq (/ SQUAREWH 2))
        (shq (/ SQUAREHH 2)))
    ;; Draw all lines
    (if top
      ;; Top piece top line
      (glgui:draw-box (- (+ x swq) 1) (+ y shq) 2 shq color))
    (if right
      ;; Top piece right line
      (glgui:draw-box (+ x swq) (- (+ y shq) 1) swq 2 color))
    (if bottom
      ;; Top piece bottom line
      (glgui:draw-box (- (+ x swq) 1) y 2 shq color))
    (if left
      ;; Top piece left line
      (glgui:draw-box x (- (+ y swq) 1) swq 2 color)))
)

;; Draw all the pieces that have already fallen
(define (draw-filled)
  
  ;; Change state of game if necessary
  (cond
     ((and (fx= state MATCHFOUND) (> (- ##now move_time) (* colourlines falltime)))
        (set! state BREAKING))
     ((and (fx= state BREAKING) (> (- ##now move_time) (* removelines falltime)))
        (remove-squares))
     ((and (fx= state LEVELUP) (> (- ##now move_time) (* showgoalmatch falltime)))
        (level-up)))
  
  ;; Draw each of the squares that are filled in
  (let loop ((sq filled))
    (if (fx> (length sq) 0)
      (let* ((x (+ LEFTSIDE (* (caar sq) SQUAREW)))
             (y (+ BOTTOMSIDE (* (cadar sq) SQUAREH)))
             ;; In currently matched set of squares to remove
             (inmatch (and (not (fx= state FALLING)) (member (car sq) removesquares)))
             ;; In current matched goal
             (ingoal (and (not inmatch) (fx= state LEVELUP) (member (car sq) goal_squares_found)))
             ;; Match was just found, draw lines green
             (m (and inmatch (fx= state MATCHFOUND)))
             ;; Now breaking blocks to remove the match, draw square grey
             (b (and inmatch (fx= state BREAKING))))
        (glgui:draw-box x y SQUAREW SQUAREH (if ingoal DarkGreen Grey))
        (glgui:draw-box (+ x 1) (+ y 1) (- SQUAREW 2) (- SQUAREH 2) (if ingoal DarkGreen (if b Grey White)))
        (if (not b)
          (if ingoal
            ;; If in the goal, draw line involved in the match different from those that are not
            (draw-goal-square-lines x y (car sq))
            ;; Only draw lines if not breaking the squares in a match
            (apply draw-square-lines (append (list x y) (cddar sq)
                                             ;; Determine color based on whether current square is part of a recently matched path to be removed
                                             (list (if (or ingoal m)
                                                     Green
                                                     Blue))))))
        (loop (cdr sq)))))
)

;; Go to the next level. Increase the displayed level, go to the next goal
;; and clear the filled in pieces
(define (level-up)
  
  ;; Update the score by counting each square in the goal twice
  (set! score (+ score (* 2 (length goal_squares_found))))
  (glgui-widget-set! gui score_number 'label (number->string score))
  (set! goal_squares_found '())
  (set! state FALLING)
  
  ;; Adjust the falltime, faster at each multiple of 30 points
  (set! falltime (- 1.0 (* (floor (/ score 30.0)) 0.03)))
  
  ;; Increase the level
  (set! level (+ level 1))
  (glgui-widget-set! gui level_number 'label (number->string level))
  
  ;; Clear the board
  (set! filled '())
  (set! piece_x #f)
  (set! piece_y #f)
  (set! next_lines #f)
  
  ;; Set the new goal lines based on the next letter
  (let ((index (modulo (- level 1) (string-length goal_word))))
    (set! goal_lines (get-goal-lines (substring goal_word index (+ index 1)))))
  
  ;; Start the first piece
  (spawn-piece)
)
        
;; End the game
(define (game-over)
  (set! state NOTPLAYING)
  
  ;; No falling piece anymore or filled in area
  (set! piece_x #f)
  (set! piece_y #f)
  (set! next_lines #f)
  (set! filled '())
  
  ;; Hide next label and goal label
  (glgui-widget-set! gui next_label 'hidden #t)
  (glgui-widget-set! gui goal_label 'hidden #t)
  
  ;; Show Game Over message and play again button
  (glgui-widget-set! gui gameover 'hidden #f)
  (glgui-widget-set! gui high_label 'hidden #f)
  (glgui-widget-set! gui scorelist 'hidden #f)
  (glgui-widget-set! gui playagain 'hidden #f)
  
  ;; Add the current score to the high scores, if it qualifies
  (let loop ((scores (get-scores)) (largerscores '()))
    (if (fx> (length scores) 0)
      (if (fx> score (string->number (caar scores)))
        (begin
          ;; If this score bigger than the current one, insert it in the list
          (set! scores (append largerscores (list (list (number->string score) (glgui-widget-get gui name_field 'label))) scores))
          ;; Don't let the list be longer than 5
          (if (fx> (length scores) 5)
            (set! scores (reverse (cdr (reverse scores)))))
          ;; Display the new list
          (update-score-list scores))
        ;; Otherwise go to next score
        (loop (cdr scores) (append largerscores (list (car scores)))))
      ;; If through all entries, check if this can be added to the end
      (if (fx< (length largerscores) 5)
        (begin
          (set! scores (append largerscores (list (list (number->string score) (glgui-widget-get gui name_field 'label)))))
          (update-score-list scores))
        ;; Otherwise just update list anyway
        (update-score-list largerscores))))
)

;; Gets the scores from the csv file
(define (get-scores)
  (let ((path (string-append (system-directory) (system-pathseparator) "scores.csv")))
    (if (file-exists? path)
      ;; If the file exists, read from it
      (csv-read path)
      ;; Otherwise return an empty list
      (list)))
)

;; Sets the scores in the csv file
(define (set-scores scores)
  (let ((path (string-append (system-directory) (system-pathseparator) "scores.csv")))
    (csv-write path scores))
)
  

;; Updates the list of high scores in the database and in the displayed list
(define (update-score-list scores)
  
  ;; Update the file
  (set-scores scores)
  
  ;; Update the displayed list
  (glgui-widget-set! gui scorelist 'list
     (map
        (lambda (entry)
          (lambda (g wgt bx by bw bh selected?)
            (glgui:draw-text-left (+ bx 5) by (- bw 55) bh (cadr entry) ascii_18.fnt White)
            (glgui:draw-text-right (- (+ bx bw) 40) by 35 bh (car entry) ascii_18.fnt White)))
        scores)) 
)
  

;; Removes the squares involved in a path from an edge to another
;; edge line, if there is one and then drops the squares above them into their spots.
;; This is repeated until there are no more paths across.
(define (check-for-paths)
  (let ((path (get-valid-path)))
    
    (if (fx> (length path) 0)
      (begin
        ;; Change the state of the game 
        (set! removesquares path)
        (set! state MATCHFOUND))))
)

;; Removes the squares that are saved to be removed
(define (remove-squares)
  
   ;; Handle deleting the squares and dropping the filled squares in order by column and then by row within this
   (let cloop ((c 0) (p removesquares) (f filled))
     (if (fx< c GRIDW)
       ;; Go through all filled rows of this column
       (let rloop ((r 0) (drop 0) (rp p) (rf f))
         (if (fx< r GRIDH)
           ;; If still within the grid, get the square filling this spot, if any
           (let ((sq (get-square c r)))
             (if sq
               ;; If this square is filled
               (if (member sq rp)
                 ;; If in the path, remove it, drop the squares above
                 (rloop (+ r 1) (+ drop 1) (list-delete-item rp sq) (list-delete-item rf sq))
                 ;; Otherwise, drop it if necessary and go to the next square
                 (if (fx> drop 0)
                   (let ((newsq (drop-square sq drop)))
                     (rloop (+ r 1) drop rp (cons newsq (list-delete-item rf sq))))
                   (rloop (+ r 1) drop rp rf)))
               ;; Otherwise not filled, go to the next column
               (cloop (+ c 1) rp rf)))
           ;; Reached the top of the grid, go to the next column
           (cloop (+ c 1) rp rf)))
       ;; Done all the columns in the grid, set the new filled set of squares
       (set! filled f)))
  
  ;; Add these squares to the score
  (set! score (+ score (length removesquares)))
  (glgui-widget-set! gui score_number 'label (number->string score))
  
  ;; Adjust the falltime, faster at each multiple of 100 points
  (set! falltime (- 1.0 (* (floor (/ score 30.0)) 0.03)))
  
  ;; Clear the remove squares list, back to just falling
  (set! removesqaures #f)
  (set! state FALLING)
  
  ;; But set a flag to check for new paths right away at the next move
  (set! pathcheck #t)
)
  
;; Get any path from edge to another edge
(define (get-valid-path)
  ;; checks if we have already visited current square
  (define (square-repeat-check sq visited)
      (if (member sq visited)
        ;; Return an empty list is a duplicate room
        (list)
        ;; Otherwise return the results of checking the square
        (square-check sq visited)))
        
  ;; checks if current square is against an edge (after at least going to one other square)
  (define (square-check sq visited)
      (if (and (fx> (length visited) 0)
                   ;; Against the left side
               (or (and (fx= (car sq) 0) (get-left sq))
                   ;; Along the bottom
                   (and (fx= (cadr sq) 0) (get-down sq))
                   ;; Against the right side
                   (and (fx= (car sq) (- GRIDW 1)) (get-right sq))))
        ;; Return the list of all squares visited if we found the edge 
        (cons sq visited)                                           
        (square-connection-check (get-connected-squares sq)      ;; passes current connections
                                 (cons sq visited))))            ;; add square to visited
        
  ;; Checks if there are still connected squares to check
  (define (square-connection-check sqs visited)
       (cond ((fx= (length sqs) 0) (list))
             (else
                (let ((firstpath (square-repeat-check (car sqs) visited))
                      (restofpaths (square-connection-check (cdr sqs) visited)))
                  (if (fx> (length firstpath) (length restofpaths)) firstpath restofpaths)))))
    
    
    (let ((olongest (list)))
      ;; Check starting from each square on the left
      (let lloop ((y 0) (longest (list)))
        (if (fx< y GRIDH)
          (let* ((nextsq (get-square 0 y))
                 (nextpath (if (and nextsq (get-left nextsq)) (square-repeat-check nextsq (list)) (list))))
            ;; Use the path that is longest
            (lloop (+ y 1) (if (fx> (length nextpath) (length longest)) nextpath longest)))
          ;; Return the longest path found, will be empty if no path
          (set! olongest longest)))
      ;; Check starting from each square on the bottom
      (let bloop ((x 0) (longest olongest))
        (if (fx< x GRIDW)
          (let* ((nextsq (get-square x 0))
                 (nextpath (if (and nextsq (get-down nextsq)) (square-repeat-check nextsq (list)) (list))))
            ;; Use the path that is longest
            (bloop (+ x 1) (if (fx> (length nextpath) (length longest)) nextpath longest)))
          ;; Return the longest path found, will be empty if no path
          (set! olongest longest)))
      ;; Check starting from each square on the right
      (let rloop ((y 0) (longest olongest))
        (if (fx< y GRIDH)
          (let* ((nextsq (get-square (- GRIDW 1) y))
                 (nextpath (if (and nextsq (get-right nextsq)) (square-repeat-check nextsq (list)) (list))))
            ;; Use the path that is longest
            (rloop (+ y 1) (if (fx> (length nextpath) (length longest)) nextpath longest)))
          ;; Return the longest path found, will be empty if no path
          (set! olongest longest)))
      
      ;; If no path found yet, try corners
      (if (fx= (length olongest) 0)
        (let ((blc (get-square 0 0)))
           (if (and blc (get-left blc) (get-down blc))
             (list blc)
             (let ((brc (get-square (- GRIDW 1) 0)))
               (if (and brc (get-down brc) (get-right brc))
                 (list brc)
                 ;; Otherwise just return an empty list, no path found
                 (list)))))
        ;; Otherwise, return the longest of all paths found
        olongest))
)

;; Get squares connected to this one, if any
(define (get-connected-squares sq)
  
  (let ((x (car sq))
        (y (cadr sq))
        (up (get-up sq))
        (right (get-right sq))
        (down (get-down sq))
        (left (get-left sq))
        (connected (list)))
      
    ;; Check each direction for a connected square
    (if up
      (let ((nextsq (get-square x (+ y 1))))
        (if (and nextsq (get-down nextsq))
          (set! connected (cons nextsq connected)))))
    (if right
      (let ((nextsq (get-square (+ x 1) y)))
        (if (and nextsq (get-left nextsq))
          (set! connected (cons nextsq connected)))))
    (if down
      (let ((nextsq (get-square x (- y 1))))
        (if (and nextsq (get-up nextsq))
          (set! connected (cons nextsq connected)))))
    (if left
      (let ((nextsq (get-square (- x 1) y)))
        (if (and nextsq (get-right nextsq))
          (set! connected (cons nextsq connected)))))
    
    connected)
) 

;; Drops the given square the given number of spaces by adjusting the y coordinate
;; Returns the new square 
(define (drop-square sq drop)
  (append (list (car sq) (- (cadr sq) drop)) (cddr sq))
)

(main
;; initialization
  (lambda (w h)
    (make-window 320 480)
    (glgui-orientation-set! GUI_PORTRAIT)
    (set! gui (make-glgui))
    
   ;; Make the log folder, if not there
   (let ((logfolder (string-append (system-directory) (system-pathseparator) "log")))
     (if (not (file-exists? logfolder))
       (create-directory logfolder)))
    
    (glgui-widget-set! gui (glgui-label gui 50 440 220 40 "LINE DROP" ascii_25.fnt White) 'align GUI_ALIGNCENTER)
    
    (set! next_label (glgui-label gui 20 (+ TOPSIDE 75) 60 22 "NEXT" ascii_18.fnt White))
    (glgui-widget-set! gui next_label 'align GUI_ALIGNCENTER)
    (glgui-widget-set! gui next_label 'hidden #t)
    
    (set! level_label (glgui-label gui 130 (+ TOPSIDE 75) 60 22 "LEVEL" ascii_18.fnt White))
    (set! level_number (glgui-label gui 130 (+ TOPSIDE 53) 60 22 "1" ascii_18.fnt White))
    (set! score_label (glgui-label gui 125 (+ TOPSIDE 27) 70 22 "SCORE" ascii_18.fnt White))
    (set! score_number (glgui-label gui 130 (+ TOPSIDE 5) 60 22 "0" ascii_18.fnt White))
    (glgui-widget-set! gui level_label 'hidden #t)
    (glgui-widget-set! gui level_label 'align GUI_ALIGNCENTER)
    (glgui-widget-set! gui level_number 'hidden #t)
    (glgui-widget-set! gui level_number 'align GUI_ALIGNCENTER)
    (glgui-widget-set! gui score_label 'hidden #t)
    (glgui-widget-set! gui score_label 'align GUI_ALIGNCENTER)
    (glgui-widget-set! gui score_number 'hidden #t)
    (glgui-widget-set! gui score_number 'align GUI_ALIGNCENTER)
    
    (set! goal_label (glgui-label gui 240 (+ TOPSIDE 75) 60 22 "GOAL" ascii_18.fnt White))
    (glgui-widget-set! gui goal_label 'hidden #t)
    
    (glgui-box gui (- LEFTSIDE 2) (- BOTTOMSIDE 2) (+ (- RIGHTSIDE LEFTSIDE) 4) (+ (- TOPSIDE BOTTOMSIDE) 4) White)
    (glgui-box gui LEFTSIDE BOTTOMSIDE (- RIGHTSIDE LEFTSIDE) (- TOPSIDE BOTTOMSIDE) Black)
    
    ;; The name label and field
    (set! name_label (glgui-label gui (+ (/ (- RIGHTSIDE LEFTSIDE 240) 2) LEFTSIDE) (+ (/ (- TOPSIDE BOTTOMSIDE 60) 2) BOTTOMSIDE 100) 240 60 "Enter your name:" ascii_25.fnt White))
    (glgui-widget-set! gui name_label 'align GUI_ALIGNCENTER)
    (set! name_field (glgui-label gui (+ (/ (- RIGHTSIDE LEFTSIDE 295) 2) LEFTSIDE) (+ (/ (- TOPSIDE BOTTOMSIDE 60) 2) BOTTOMSIDE 80) 295 30 "" ascii_25.fnt White))
    (glgui-widget-set! gui name_field 'align GUI_ALIGNCENTER)
    (glgui-widget-set! gui name_field 'focus #t)
    (glgui-widget-set! gui name_field 'aftercharcb
       (lambda (g . x)
         ;; Each time the field is updated, hide the start button if it is empty, show it if it is not - can't play with empty name
         (glgui-widget-set! gui startbutton 'hidden (fx= (string-length (glgui-widget-get gui name_field 'label)) 0))))
    
    ;; Place start button in the center of the grid
    (set! startbutton (glgui-button-string gui (+ (/ (- RIGHTSIDE LEFTSIDE 120) 2) LEFTSIDE) (+ TOPSIDE 25) 120 50 "START" ascii_25.fnt
       (lambda (g . x)
         (set! state FALLING)
         
         ;; Save the name
         (set! goal_word (string-remove-spaces (glgui-widget-get gui name_field 'label)))
         
         ;; Hide the start button and game over message, show the next label
         (glgui-widget-set! gui startbutton 'hidden #t)
         (glgui-widget-set! gui name_label 'hidden #t)
         (glgui-widget-set! gui name_field 'hidden #t)
         (glgui-widget-set! gui name_field 'focus #t)
         (glgui-widget-set! gui keypad 'hidden #t)
         (glgui-widget-set! gui gameover 'hidden #t)
         (glgui-widget-set! gui next_label 'hidden #f)
         (glgui-widget-set! gui level_label 'hidden #f)
         (glgui-widget-set! gui level_number 'hidden #f)
         (glgui-widget-set! gui score_label 'hidden #f)
         (glgui-widget-set! gui score_number 'hidden #f)
         (glgui-widget-set! gui goal_label 'hidden #f)
         (set! level 1)
         (glgui-widget-set! gui level_number 'label "1")
         (set! score 0)
         (glgui-widget-set! gui score_number 'label "0")
         (set! falltime 1.0)
         
         ;; Set the current level letter
         (set! goal_lines (get-goal-lines (substring goal_word 0 1)))
         
         ;; Spawn the first piece
         (spawn-piece))))
    (glgui-widget-set! gui startbutton 'hidden #t)
    
    ;; Place Game Over message in the center of the grid
    (set! gameover (glgui-container gui (+ (/ (- RIGHTSIDE LEFTSIDE 170) 2) LEFTSIDE) (- TOPSIDE 58) 170 50))
    (let ((bg (glgui-box gameover 0 5 170 45 Red))
          (message (glgui-label gameover 0 10 170 30 "GAME OVER!" ascii_25.fnt White)))
       (glgui-widget-set! gameover bg 'rounded #t)
       (glgui-widget-set! gameover message 'align GUI_ALIGNCENTER))
    (glgui-widget-set! gui gameover 'hidden #t)
    
    ;; List of high scores
    (set! high_label (glgui-label gui (+ LEFTSIDE 50) (- TOPSIDE 93) 200 27 "HIGH SCORES" ascii_25.fnt White))
    (glgui-widget-set! gui high_label 'hidden #t)
    (glgui-widget-set! gui high_label 'align GUI_ALIGNCENTER)
    (set! scorelist (glgui-list gui (+ LEFTSIDE 60) (+ BOTTOMSIDE 65) (- RIGHTSIDE LEFTSIDE 120) (- TOPSIDE BOTTOMSIDE 160) (/ (- TOPSIDE BOTTOMSIDE 160) 5) '() #f))
    (glgui-widget-set! gui scorelist 'hidden #t)
    (glgui-widget-set! gui scorelist 'autohidebar #t)
    (glgui-widget-set! gui scorelist 'bgcol1 (color-fade White 0))
    (glgui-widget-set! gui scorelist 'bgcol2 (color-fade White 0))
    
    ;; Play again button
    (set! playagain (glgui-button-string gui (+ (/ (- RIGHTSIDE LEFTSIDE 180) 2) LEFTSIDE) (+ BOTTOMSIDE 8) 180 50 "PLAY AGAIN" ascii_25.fnt
       (lambda (g . x)
         (glgui-widget-set! gui playagain 'hidden #t)
         (glgui-widget-set! gui high_label 'hidden #t)
         (glgui-widget-set! gui scorelist 'hidden #t)
         (glgui-widget-set! gui gameover 'hidden #t)
         (glgui-widget-set! gui startbutton 'hidden #f)
         (glgui-widget-set! gui name_label 'hidden #f)
         (glgui-widget-set! gui name_field 'hidden #f)
         (glgui-widget-set! gui name_field 'focus #t)
         (glgui-widget-set! gui level_label 'hidden #t)
         (glgui-widget-set! gui level_number 'hidden #t)
         (glgui-widget-set! gui score_label 'hidden #t)
         (glgui-widget-set! gui score_number 'hidden #t)
         (glgui-widget-set! gui keypad 'hidden #f))))
    (glgui-widget-set! gui playagain 'hidden #t)
    
    ;; Keypad
    (set! keypad (glgui-keypad gui 0 0 (glgui-width-get) (/ (glgui-width-get) 1.5) ascii_25.fnt))
    (glgui-widget-set! gui keypad 'bgcolor Black)
  )
;; events
  (lambda (t x y) 
    
    ;; Check for the current piece touching down
    (check-for-contact)
    
    (if (= t EVENT_KEYPRESS) (begin 
      
      (cond
        ;; Respond to pressing enter on the keypad to begin
        ((and (= x EVENT_KEYENTER) (not (glgui-widget-get gui keypad 'hidden)) (not (glgui-widget-get gui startbutton 'hidden)))
          ((glgui-widget-get gui startbutton 'callback) gui startbutton))
        ;; Cycle squares (line patterns) on the piece
        ((and piece_x (= x EVENT_KEYUP)) 
          (cycle-lines))                        
        ;; Move the piece left, right, or dow
        ((and (= x EVENT_KEYLEFT) (fx> piece_x 0) (not (get-square (- piece_x 1) piece_y)))
          (set! piece_x (- piece_x 1)))
        ((and (= x EVENT_KEYRIGHT) (fx< piece_x (- GRIDW 1)) (not (get-square (+ piece_x 1) piece_y)))
          (set! piece_x (+ piece_x 1)))
        ((and (= x EVENT_KEYDOWN) (fx> piece_y 0) (not (get-square piece_x (- piece_y 1))))
          (set! piece_y (- piece_y 1))))
                               
      (if (= x EVENT_KEYESCAPE) (terminate))))
    (glgui-event gui t x y)
    
    (draw-goal-squares)
    (draw-next-piece)
    (draw-current-piece)
    (draw-filled))
;; termination
  (lambda () #t)
;; suspend
  (lambda () (glgui-suspend))
;; resume
  (lambda () (glgui-resume))
)

;; eof
