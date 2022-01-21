;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW11-BaveshM.) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))

;; Exercise 2

;; A Content is one of:
;; - Nat
;; - #t
;; and represents either a count of the number of mines surrounding
;; a cell or a mine itself

(define CONTENT-BLANK 0)
(define CONTENT-MINE #t)

;; content-template : Content -> ???
(define (content-template c)
  (cond [(number? c) (... c ...)]
        [(boolean? c) ...]))

(define-struct hidden [con])
(define-struct visible [con])
(define-struct flagged [con])

;; A Cell is one of:
;; - (make-hidden Content)
;; - (make-visible Content)
;; - (make-flagged Content)
;; and represents either a hidden cell, a visible cell, or a flagged cell

(define CELL-H0 (make-hidden 0))
(define CELL-V0 (make-visible 0))
(define CELL-F0 (make-flagged 0))

(define CELL-H1 (make-hidden 1))
(define CELL-V1 (make-visible 1))
(define CELL-F1 (make-flagged 1))

(define CELL-HM (make-hidden #t))
(define CELL-VM (make-visible #t))
(define CELL-FM (make-flagged #t))

;; cell-template : Cell -> ???
(define (cell-template cell)
  (cond [(hidden? cell) (... (hidden-con cell) ...)]
        [(visible? cell) (... (visible-con cell) ...)]
        [(flagged? cell) (... (flagged-con cell) ...)]))

;; Exercise 3

;; A Board is a [List-of [List-of Cell]
;; and represents a grid of cells that make up a game board

(define BOARD-EMPTY '())
(define BOARD-SEMI-EMPTY '(() () () ()))
(define BOARD-2X2-BLANK (make-list 2 (make-list 2 CELL-H0)))
(define BOARD-3X3-MID (list (make-list 3 CELL-H1)
                            (list CELL-H1 CELL-HM CELL-H1)
                            (make-list 3 CELL-H1)))
(define BOARD-LOSE (list (list CELL-VM)))

;; board-template : Board -> ???
(define (board-template b)
  (cond [(empty? b) ...]
        [(cons? b) (... (row-template (first b))
                        (board-template (rest b)) ...)]))

;; row-template : [List-of Cell] -> ???
(define (row-template loc)
  (cond [(empty? loc) ...]
        [(cons? loc) (... (cell-template (first loc))
                          (row-template (rest loc)) ...)]))

;; Exercise 4
(define-struct game [board rev?])
;; A Game is a (make-game Board Boolean)
;; and represents a game of Minesweeper with a board of cells and a flag that is
;; #t if the mouse is revealing cells and #f if it is flagging them

(define GAME-EMPTY (make-game BOARD-EMPTY #t))
(define GAME-2X2-T (make-game BOARD-2X2-BLANK #t))
(define GAME-2X2-F (make-game BOARD-2X2-BLANK #f))
(define GAME-3X3-T (make-game BOARD-3X3-MID #t))
(define GAME-3X3-F (make-game BOARD-3X3-MID #f))
(define GAME-LOSE (make-game BOARD-LOSE #t))

;; game-template : Game -> ???
(define (game-template g)
  (... (board-template (game-board g))
       (game-rev? g) ...))

;; Exercise 5
(require 2htdp/universe)

;; mine-sweeper : Nat Nat -> Game
;; Play the minesweeper game with a square board of the given size and the
;; given number of mines
(define (mine-sweeper size mines)
  (mine-sweeper-from (make-game (generate-mine-board size mines) #t)))

;; mine-sweeper-from : Game -> Game
;; Play the minesweeper game with the given initial game state
(define (mine-sweeper-from g)
  (big-bang g
    [to-draw draw-game]
    [on-mouse change-if-click]
    [on-key change-mouse-state]
    [stop-when game-over? draw-game]))

#|--- STUBS AS REQUESTED BY THE ASSIGNMENT ---|#
;; draw-game : Game -> Image
;; Draw the current state of the game (WARNING: THIS IS CURRENTLY A STUB)
(define (draw-game g) empty-image)

;; change-if-click : Game Number Number MouseEvent -> Game
;; Change the game if the user clicked on a cell (WARNING: THIS IS CURRENTLY A STUB)
;(define (change-if-click g x y me) g)
#|--- END STUBS ---|#

;; There are a lot of board generation functions below. Please note that it was not
;; required that you generate a board in this way. It was fine to pass a game with a constant
;; board to mine-sweeper-from instead.

;; generate-mine-board : Nat Nat -> Board
;; Generates a fixed board of the given size with the given number of mines
;; (all the mines are at the beginning)
(check-expect (generate-mine-board 0 0) '())
(check-error (generate-mine-board 2 7) "Cannot place 7 mines on a 2x2 game board.")
(check-expect
 (generate-mine-board 2 3)
 (list (make-list 2 CELL-HM) (list CELL-HM (make-hidden 3))))
(check-expect (generate-mine-board 2 4) (make-list 2 (make-list 2 CELL-HM)))
(check-expect
 (generate-mine-board 3 4)
 (list (make-list 3 CELL-HM)
       (list CELL-HM (make-hidden 4) (make-hidden 2))
       (list CELL-H1 CELL-H1 CELL-H0)))
(define (generate-mine-board size mines)
  (if (> mines (* size size))
      (error (string-append "Cannot place " (number->string mines)
                            " mines on a " (number->string size) "x" (number->string size)
                            " game board."))
      (local [(define full-mine-rows (if (zero? size) 0 (floor (/ mines size))))
              (define partial-row?
                (and (not (zero? size)) (not (zero? (modulo mines size)))))
              (define mines-in-partial (- mines (* full-mine-rows size))) 
              (define full-zero-rows
                (max 0 (- size (+ full-mine-rows (if partial-row? 1 0)))))]
        (add-counts (append (make-list full-mine-rows (make-list size CELL-HM))
                            (if partial-row?
                                (list (append (make-list mines-in-partial CELL-HM)
                                              (make-list (- size mines-in-partial) CELL-H0)))
                                '())
                            (make-list full-zero-rows (make-list size CELL-H0)))))))

;; add-counts : Board -> Board
;; Add the correct count for each item on the given board
(check-expect (add-counts '()) '())
(check-expect
 (add-counts (list (make-list 3 CELL-H0)
                   (list CELL-H0 CELL-HM CELL-H0)
                   (make-list 3 CELL-H0)))
 BOARD-3X3-MID)
(define (add-counts b)
  (build-list (length b)
              (λ (row) (build-list (length b)
                                   (λ (col) (add-count-to-cell b row col))))))

;; add-count-to-cell : Board Nat Nat -> Cell
;; If the cell at the given location is a mine, leave it alone
;; If it is not a mine, count the mines around it and make that the count for the cell
(check-expect (add-count-to-cell BOARD-3X3-MID 0 0) CELL-H1)
(check-expect
 (add-count-to-cell
  (list (list CELL-HM CELL-HM) (list CELL-HM CELL-H0)) 1 1)
 (make-hidden 3))
(define (add-count-to-cell board row col)
  (local [(define cell (list-ref (list-ref board row) col))
          (define neighbor-posns (get-neighbor-indices row col (length board)))
          (define neighbor-cells
            (map (λ (p) (list-ref (list-ref board (posn-x p)) (posn-y p))) neighbor-posns))]
    (update-cell-count cell (length (filter mine-cell? neighbor-cells)))))

;; get-neighbor-indices : Nat Nat Nat -> [List-of Posn]
;; Get a list of the row/column indices of the neighbors of the cell with the given indices
(check-expect (get-neighbor-indices 0 0 0) '())
(check-expect
 (get-neighbor-indices 0 0 2)
 (list (make-posn 0 1) (make-posn 1 0) (make-posn 1 1)))
(check-expect
 (get-neighbor-indices 2 2 3)
 (list (make-posn 1 1) (make-posn 1 2) (make-posn 2 1)))
(check-expect
 (get-neighbor-indices 1 2 4)
 (list (make-posn 0 1) (make-posn 0 2) (make-posn 0 3)
       (make-posn 1 1) (make-posn 1 3)
       (make-posn 2 1) (make-posn 2 2) (make-posn 2 3)))
(define (get-neighbor-indices row col size)
  (local [(define can-add-left? (> size col 0))
          (define can-add-right? (< col (sub1 size)))
          (define can-add-up? (> size row 0))
          (define can-add-down? (< row (sub1 size)))]
    (append (if (and can-add-left? can-add-up?) (list (make-posn (sub1 row) (sub1 col))) '())
            (if can-add-up? (list (make-posn (sub1 row) col)) '())
            (if (and can-add-right? can-add-up?) (list (make-posn (sub1 row) (add1 col))) '())
            (if can-add-left? (list (make-posn row (sub1 col))) '())
            (if can-add-right? (list (make-posn row (add1 col))) '())
            (if (and can-add-left? can-add-down?) (list (make-posn (add1 row) (sub1 col))) '())
            (if can-add-down? (list (make-posn (add1 row) col)) '())
            (if (and can-add-right? can-add-down?) (list (make-posn (add1 row) (add1 col))) '()))))

;; update-cell-count : Cell Nat -> Cell
;; If this cell is a mine, leave it, otherwise update to the given count
(check-expect (update-cell-count CELL-HM 5) CELL-HM)
(check-expect (update-cell-count (make-visible 7) 2) (make-visible 2))
(check-expect (update-cell-count (make-flagged 0) 3) (make-flagged 3))
(define (update-cell-count cell new-count)
  (local [;; update-contents : Content -> Content
          ;; Update the contents if it is not a mine to be new-count
          (define (update-contents c)
            (if (boolean? c) c new-count))]
    (cond [(hidden? cell)
           (make-hidden (update-contents (hidden-con cell)))]
          [(visible? cell)
           (make-visible (update-contents (visible-con cell)))]
          [(flagged? cell)
           (make-flagged (update-contents (flagged-con cell)))])))

;; mine-cell? : Cell -> Boolean
;; Is this a mine cell?
(check-expect (mine-cell? CELL-HM) #t)
(check-expect (mine-cell? (make-visible 3)) #f)
(check-expect (mine-cell? (make-flagged 0)) #f)
(define (mine-cell? cell)
  (cond [(hidden? cell) (boolean? (hidden-con cell))]
        [(visible? cell) (boolean? (visible-con cell))]
        [(flagged? cell) (boolean? (flagged-con cell))]))

;; Exercise 6
;; change-mouse-state : Game KeyEvent -> Game
;; Change the state of the mouse if the user pressed the space bar
(check-expect (change-mouse-state GAME-EMPTY "x") GAME-EMPTY)
(check-expect (change-mouse-state GAME-2X2-F " ") GAME-2X2-T)
(define (change-mouse-state g key)
  (if (key=? key " ") (make-game (game-board g) (not (game-rev? g))) g))

;; Exercise 7
;; game-over? : Game -> Boolean
;; Did the user either win or lose?
(check-expect (game-over? GAME-LOSE) #t)
(check-expect (game-over? GAME-EMPTY) #t)
(check-expect (game-over? GAME-3X3-T) #f)
(define (game-over? g)
  (or (board-win? (game-board g))
      (board-lose? (game-board g))))

;; board-win? : Board -> Boolean
;; Did the user reveal all the non-mine squares?
(check-expect (board-win? BOARD-EMPTY) #t)
(check-expect (board-win? BOARD-3X3-MID) #f)
(check-expect (board-win? (make-list 2 (make-list 2 CELL-FM))) #t)
(define (board-win? board)
  (local [;; mine-or-visible? : Cell -> Boolean
          ;; Is the given cell either a mine or visible?
          (define (mine-or-visible? cell)
            (cond [(visible? cell) #t]
                  [(hidden?  cell) (boolean? (hidden-con cell))]
                  [(flagged? cell) (boolean? (flagged-con cell))]))]
    (andmap (λ (row) (andmap mine-or-visible? row)) board)))

;; board-lose? : Board -> Boolean
;; Is there any visible mine square on the board?
(check-expect (board-lose? BOARD-3X3-MID) #f)
(check-expect
 (board-lose?
  (list (list (make-hidden #t) (make-visible 0))
        (list (make-flagged #t) (make-visible #t))))
 #t)
(define (board-lose? board)
  (local [;; visible-mine? : Cell -> Boolean
          ;; Is the given cell a mine that is visible?
          (define (visible-mine? cell)
            (and (visible? cell) (boolean? (visible-con cell))))]
    (ormap (λ (row) (ormap visible-mine? row)) board)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Exercise 4
(define MARGIN 50)
(define CELL-WIDTH 50)

(define G1
  (make-game (make-list 3 (make-list 3 CELL-H0)) #t))
(check-expect (change-if-click G1 130 60 "left") G1)
(check-expect (change-if-click G1 3 4 "button-down") G1)
               
;; change-if-click : Game Number Number MouseEvent -> Game
;; Change the game in a plethora of ways according to conditions
(define (change-if-click g x y me)
  (local [(define X-GRID (+ MARGIN (* CELL-WIDTH (length (game-board g)))))
          (define Y-GRID (+ MARGIN (* CELL-WIDTH (length (game-board g)))))
          ;clicked-cell Output: Cell
          ;Evaluates the cursor position click to return the Cell in the Game
          ;converts coordinates to the corresponding cell
          (define clicked-cell
            (local [(define (clicked-cell2 b x y)
                      (local [(define (clicked-y row x)
                                (cond
                                  [(or (empty? row)
                                   (<= x MARGIN)) (first row)]
                                  [(>  x MARGIN) (clicked-y (rest row) (- CELL-WIDTH x))]))]         
                        (cond
                          [(or (empty? b)
                           (<= y MARGIN)) (clicked-y (first b) x)]
                          [(>  y MARGIN)  (clicked-cell2
                                     (rest b) x (- CELL-WIDTH y))])))]
              (clicked-cell2 (game-board g) (- x MARGIN) (- y MARGIN))))
          ;attach: Cell -> Game
          ;attaches a cell in a board to make a Game
          (define (attach cell)
            (local [;attach2: Board Nat Nat Nat Nat -> Board
                    ;ACCUMULATORS: To close in on single cells
                    ;by keeping track of the x and y positions
                    (define (attach2 b x y x-pos y-pos)
                      (local [(define (attach-y row)
                                (cond [(empty? row) '()]
                                      [(cons?  row) (if (and (>= y (- y-pos CELL-WIDTH))
                                                             (<  y (+ y-pos CELL-WIDTH)))
                                                        (cons cell (rest row))
                                                        (cons (first row) (attach-y (rest row))))]))]
                        (cond [(empty? b)  '()]
                              [(cons?  b) (if (and (>= x (- x-pos CELL-WIDTH))
                                                   (<  x (+ x-pos CELL-WIDTH)))
                                              (cons (attach-y (first b)) (rest b))
                                              (cons (first b)
                                                    (attach2
                                                     (rest b) (- CELL-WIDTH x) y x-pos y-pos)))])))]
              (make-game (attach2 cell g x y x y) (game-rev? g))))]
    (local [;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;FUNCTIONS TO EVALUATE LEGAL MOUSE EVENTS/GAME MOVES
            ;beyond-grid? : checks if the user clicked beyond the playable area
            (define beyond-grid?
              (or (and (string=? me "button-down") (or (< x MARGIN)(< y MARGIN)))
                  (and (string=? me "button-down")(> x X-GRID)(> y Y-GRID))))
            ;useless-me? : returns #t if any MouseEvent other than button-down takes place   
            (define useless-me?
              (not (string=? me "button-down")))
            ;clicked-on-visible? : returns true if the mouse clicks on a cell that's visible
            (define clicked-on-visible?
              (visible? clicked-cell))
            ;clicked-flag-reveal? : returns true if the mouse clicks on a cell that's flagged
            ;while the toggle is on reveal (#t)
            (define clicked-flag-reveal?
              (and (flagged? clicked-cell) (game-rev? g)))
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;FUNCTIONS TO IMPLEMENT LEGAL GAME MOVES
            ;make-cell-flagged : Cell -> Cell
            ;make-cell-flagged : changes the cell clicked to flagged
            (define make-cell-flagged              
              (if (hidden? clicked-cell) (make-flagged (hidden-con clicked-cell)) clicked-cell))
            ;remove-flag : Cell -> Cell
            ;remove-flag : removes the flag (cheanges to hidden)
            (define remove-flag
              (if (visible? clicked-cell) (make-hidden (flagged-con clicked-cell)) clicked-cell))
            ;make-visible: Game Nat Nat -> Game
            ;make-visible: makes one or more cells visible upon clicking
            (define make-visible
              (if (hidden? clicked-cell)
              (make-game (make+attach-visible (game-board g) x y) (game-rev? g)) clicked-cell))] 
      (cond
        [(or beyond-grid?
             useless-me?
             clicked-on-visible?
             clicked-flag-reveal?)                             g]
        [(and (hidden? clicked-cell)
              (game-rev? g))                        make-visible]
        [(and (hidden? clicked-cell)
              (not (game-rev? g)))    (attach make-cell-flagged)]
        [(and (flagged? clicked-cell)
              (not (game-rev? g)))    (attach       remove-flag)]))))

; Board Nat Nat -> Board
;make-cell-visible : changes the cell clicked to visible, if =/= 0
;but makes its surrounding cells visible if cell = 0.
(define (make+attach-visible g x y)
  (local
    [;clicked-cell-v: Nat Nat -> Cell
     ;A version of clicked-cell defined above, but takes in flexible x y values
     (define (clicked-cell-v x y)
       (local
         [;clicked-cell2: Board acc acc -> Cell
          ;reduces the list recursively to obtain the cell
          ;ACCUMULATORS: updates the x and positions to 'close' in on the list
          (define (clicked-cell2 g x y)
            (local
              [;Reduces a row to a singular cell
               (define (clicked-y row x)
                 (cond
                   [(or (empty? row)
                    (<= x MARGIN)) (first row)]
                   [(>  x MARGIN) (clicked-y (rest row) (- CELL-WIDTH x))]))]         
              (cond
                [(or (empty? g)
                  (<= y MARGIN)) (clicked-y g x)]
                [(>  y MARGIN) (clicked-cell2
                           (rest g) (- CELL-WIDTH x) y)])))]
         (clicked-cell2 g (- MARGIN x) (- MARGIN y))))
     ;attach-v: Cell Board Nat Nat -> Board
     ;A version of attach defined above, but takes in flexible x y values
     ;to attach the Cell in the board
     (define (attach-v cell g x y)
       (local
         [;attach2: Evaluates the position of the cursor when clicked to
          ;attach the cell.
          ;ACCUMALORTORS: reduces rows and columns to close in on the cell.
          (define (attach2 g x y x-pos y-pos)
            (local [;reduces a row
                    (define (attach-y row)
                      (cond
                        [(empty? row) '()]
                        [(cons?  row) (if (and (>= y (- y-pos CELL-WIDTH))
                                               (<  y (+ y-pos CELL-WIDTH)))
                                          (cons cell (rest row))
                                          (cons (first row) (attach-y (rest row))))]))]
              (cond [(empty? g)  '()]
                    [(cons?  g) (if (and (>= x (- x-pos CELL-WIDTH))
                                         (<  x (+ x-pos CELL-WIDTH)))
                                    (cons (attach-y (first g)) (rest g))
                                    (cons (first g)
                                          (attach2
                                           (rest g) (- CELL-WIDTH x) y x-pos y-pos)))])))]
         (attach2 g x y x y)))]
    (cond
      [(> (hidden-con (clicked-cell-v x y)) 0)
       (attach-v (make-visible (hidden-con (clicked-cell-v x y))) g x y)]
      [else
       (local [;apply: [List-of Posn] -> Board
               ;Applies the set of coordinates to attach it on to the board, recursively
               (define (apply lop)
                 (cond [(empty? lop)
                        (attach-v (make-visible (hidden-con (clicked-cell-v x y))) g x y)]
                       [(cons?  lop)
                        (make+attach-visible
                         (apply (rest lop))
                         (posn-x (first lop)) (posn-y (first lop)))]))]                 
         (apply (list
                 (make-posn (+ CELL-WIDTH x) (+ CELL-WIDTH y))
                 (make-posn (- CELL-WIDTH x) (+ CELL-WIDTH y))
                 (make-posn (+ CELL-WIDTH x) (- CELL-WIDTH y))
                 (make-posn (- CELL-WIDTH x) (- CELL-WIDTH y))
                 (make-posn (+ CELL-WIDTH x)               y)
                 (make-posn (- CELL-WIDTH x)               y)
                 (make-posn               x  (+ CELL-WIDTH y))
                 (make-posn               x  (- CELL-WIDTH y)))))])))


