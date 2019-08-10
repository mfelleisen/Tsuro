#lang racket

;; a data representation for game States, plus basic functions for manipulating them

;; TODO
;; - factor out matrix, in case it needs to be replaced 
;; - add configured tile T for player P: move all players

;; safety
;; - contract for initialize and add?

;; legality checks: 
;; - determine whether the addition of a tile for a player P forces P to commit suicide
;; - determine whether the addition of a tile adds a cyclic path 

(provide
 ;; type State

 #; {-> State}
 state-with-3-players

 #; {Placement0 = [List Configuration PortIndex Index Index]
                where (list c p x y) must satisfy the following conditions: 
                1. (x,y) must describe a position at the periphery of the State 
                2. p must be a port that faces an empty square}

 #; { [Listof Placement0] -> State }
 initialize)


;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/port-alphabetic)
(require htdp/matrix)
(require pict)

(module+ test
  (require rackunit)
  (require racket/gui))

;; ---------------------------------------------------------------------------------------------------
;; data representation of the game state: the board, the players  

(define SIZE 10) ; Tsuro is played on a board with SIZE x SIZE configured tiles

;; the game state itself 
;; ------------------------------------------------------------------
#; {State  = (State Board [Listof Player])}
#; {Player = (player String p x y)}
#; {Board  = [Matrixof square] :: {Index x Index}}
#; {Index  = [0 .. SIZE]}
#; {Square = (U BLANK  ;; an unoccupied, blank square 
                (square Tile PortMap))} ;; a configured tile with connections to neighbors cached 

(struct state  [board players] #:transparent)
(struct player [name port x y] #:transparent)
(struct square [tile map] #:transparent)
(define BLANK #false)

;; the portmap is a fast means for looking up the neighboring squares
;; ------------------------------------------------------------------
#; {PortMap = (Vectorof Connect) :: [Port ->f Connect]}
#; {Connect = (connect Next Next)}
#; {Next    = (U
               WALL      ;; periphery of board 
               OPEN      ;; no neighboring tile yet 
               (next Port Index Index))} ;; points to a port on this or a neigboring square 

(struct connect [internal external] #:transparent)
(struct next [port x y] #:transparent)

(define WALL "wall") 
(define OPEN "open")

;; ---------------------------------------------------------------------------------------------------
;; data examples

(define the-empty-board (build-matrix SIZE SIZE (λ (_i _j) BLANK)))

(match-define `(,port-2 ,port-3 ,port-4) (map index->port '(2 3 4)))

(define player-00 "red")
(define player-02 "white")
(define player-20 "blue")

(define config-00 configuration1)
(define config-02 configuration1)
(define config-20 90configuration2)

(define inits-for-board-3-players
  `((,config-00 ,player-00 ,port-2 0 0)
    (,config-02 ,player-02 ,port-3 0 2)
    (,config-20 ,player-20 ,port-4 2 0)))

(define (state-with-3-players #:with (with #false))
  (define square-00 (square config-00 (create-portmap config-00 0 0)))
  (define square-02 (square config-02 (create-portmap config-02 0 2)))
  (define square-20 (square config-20 (create-portmap config-20 2 0)))
  (define boatd3
    (let* ([m the-empty-board]
           [m (matrix-set m 0 0 square-00)]
           [m (matrix-set m 2 0 square-02)]
           [m (matrix-set m 0 2 square-20)])
      m))
  (define players3 (map (λ (init) (apply player (rest init))) inits-for-board-3-players))
  (define the-board (state boatd3 players3))
  (if with
      (values the-board square-00 square-02 square-20)
      the-board))

(module+ test ;; additional data samples 
  (define-values (state-3-players square-00 square-02 square-20) (state-with-3-players #:with #true))
  (define board-3-players (state-board state-3-players))
  (define config-to-be-added-to-board-with-3 configuration2))

;; ---------------------------------------------------------------------------------------------------
;; adding a tile T for a player P

#; {State Configuration PlayerName -> State}
;; contract:
;; -- p is on (board-players b)
;; -- the neighbor of p is unoccupied
;; assume: legality 
(define (add-tile s c p)
  (match-define (player _ port x y) (find-player s p))
  (define-values (x-new y-new) (looking-at port x y))
  (define nu-board (add-new-square-update-neighbors (state-board s) c x-new y-new))
  (state nu-board (state-players s)))

#; {State PlayerName -> Player}
(define (find-player state p)
  (first (memf (finder p) (state-players state))))

#; {PlayerName -> (Player -> Boolean)}
(define (finder p)
  (compose (curry equal? p) player-name))

(module+ test ;; add-tile 
  
  (define nu-state
    (state
     (add-new-square-update-neighbors board-3-players config-to-be-added-to-board-with-3 1 0)
     (remf (finder player-00) (state-players state-3-players))))
  
  (check-equal? (add-tile state-3-players config-to-be-added-to-board-with-3 player-00) nu-state
                "drive red player off"))

;; ---------------------------------------------------------------------------------------------------
;; updadting the matrix

#; {Board Confguration Index Index -> Matrix}

(define (add-new-square-update-neighbors board config x y)
  (define-values (x-n* y-n*) (neighbors* board x y))
  (for/fold ((m (matrix-set board y x (create-square board config x y)))) ((x-n x-n*) (y-n y-n*))
    (matrix-set m y-n x-n (update-square (matrix-ref m y-n x-n) x-n y-n x y))))

(module+ test
  (define nu-square  (create-square board-3-players config-to-be-added-to-board-with-3 1 0))
  (define nu-board (let* ([m board-3-players]
                          [m (matrix-set m 0 1 nu-square)]
                          [m (matrix-set m 0 0 (update-square square-00 0 0 1 0))]
                          [m (matrix-set m 0 2 (update-square square-20 2 0 1 0))])
                     m))
  
  (check-equal?
   (add-new-square-update-neighbors board-3-players config-to-be-added-to-board-with-3 1 0)
   nu-board))

;; ---------------------------------------------------------------------------------------------------
;; creating and updatiing squares 

#; {Board Configuration Index Index -> square}
;; create a square at (x,y) from configuration with current matrix n*
(define (create-square board config x y)
  (define-values (x-n* y-n*) (neighbors* board x y))
  (define pm
    (for/fold ((pm (create-portmap config x y))) ((x-n x-n*)[y-n y-n*])
      (update-portmap pm x y x-n y-n)))
  (square config pm))

#; {Square Index Index Index Index -> square}
(define (update-square old x-old y-old x-new y-new)
  (square (square-tile old) (update-portmap (square-map old) x-old y-old x-new y-new)))

(module+ test ;; operating on squares
  (check-equal? (update-square square-00 0 0 1 0)
                (square config-00
                        (let* ([pm (square-map square-00)]
                               [pm (update-portmap pm 0 0 1 0)])
                          pm)))

  (check-equal? (create-square board-3-players config-to-be-added-to-board-with-3 1 0)
                (square config-to-be-added-to-board-with-3
                        (let* ([pm (create-portmap config-to-be-added-to-board-with-3 1 0)]
                               [pm (update-portmap pm 1 0 2 0)]
                               [pm (update-portmap pm 1 0 0 0)])
                          pm))))

;; ---------------------------------------------------------------------------------------------------
;; initialize a State from a list of (initial) Placements0

(define (initialize lo-placements)
  (define players (for/list ([p lo-placements]) (apply player (rest p))))
  (define board
    (for/fold ((m the-empty-board)) ((p lo-placements))
      (match-define `(,c ,_  ,_ ,x ,y) p)
      (matrix-set m y x (square c (create-portmap c x y)))))
  (state board players))

(module+ test
  (define board1
    (let* ([square-00 (square configuration1 (create-portmap configuration1 0 0))])
      (matrix-set the-empty-board 0 0 square-00)))
  (check-equal? (initialize `((,configuration1 "x" 2 0 0))) (state board1 `(,(player "x" 2 0 0))))

  (check-equal? (initialize inits-for-board-3-players) (state-with-3-players)))

;; ---------------------------------------------------------------------------------------------------
;; creating a portmap, adding external connects to portmap

#; {PortMap Index Index Index Index -> PortMap}
;; update the external parts of the portmap at (x-pm,y-pm) to connect with neighbors on n at (x-n,y-n)
(define (update-portmap pm x-pm y-pm x-n y-n)
  (for/vector ((c (in-vector pm)) (pi (in-naturals)))
    (define p (index->port pi))
    (define-values (x-look y-look) (looking-at p x-pm y-pm))
    (cond
      [(and (eq? x-look x-n) (eq? y-look y-n))
       (connect (connect-internal c) (next (facing-port p) x-n y-n))]
      [else c])))

(module+ test ;; adding external connections to a portmap 
  (define nu-pm
    (let* ([pm (create-portmap config-to-be-added-to-board-with-3 1 0)]
           [pm (update-portmap pm 1 0 2 0)]
           [pm (update-portmap pm 1 0 0 0)])
      pm))
  (define pm3+
    (let ([v (vector-copy nu-pm)])
      (define 2-external (next (facing-port (index->port 2)) 2 0))
      (define 3-external (next (facing-port (index->port 3)) 2 0))
      (vector-set! v 2 (connect (connect-internal (vector-ref v 2)) 2-external))
      (vector-set! v 3 (connect (connect-internal (vector-ref v 3)) 3-external))
      v))
          
  (check-equal? (update-portmap nu-pm 1 0 2 0) pm3+))

#; {Configuration Index Index -> PortMap }
(define (create-portmap config x y)
  (for/vector ((e (in-list (configuration->list-map config))) (c (in-vector (create-portmap0 x y))))
    (match-define (connect internal external) c)
    (match-define `(,from ,to) e)
    (connect (next to x y) external)))

(define-match-expander ??
  (λ (stx)
    (syntax-case stx ()
      [(_ w) #'(? (curry = (- w 1)))])))

(define (create-portmap0 x y)
  (match* (x y)
    [(0  0)                (west-north)]
    [(0 (?? SIZE))         (west-south)]
    [((?? SIZE) 0)         (east-north)]
    [((?? SIZE) (?? SIZE)) (east-south)]
    [(0  n)                (west-any)  ]
    [((?? SIZE) n)         (east-any)  ]
    [(n 0)                 (any-north) ]
    [(n (?? SIZE))         (any-south) ]
    [(n k)                 (any-any)   ]))

(define ----- (connect WALL WALL))
(define |   | (connect OPEN OPEN))

;; ---------------- ports:   0     1     2     3     4     5     6     7 ---
;;                            north       east        south       west  
[define (west-north) (vector ----- ----- |   | |   | |   | |   | ----- -----)]
[define (west-south) (vector |   | |   | |   | |   | ----- ----- ----- -----)]
[define (east-north) (vector ----- ----- ----- ----- |   | |   | |   | |   |)]
[define (east-south) (vector |   | |   | ----- ----- ----- ----- |   | |   |)]
[define (west-any)   (vector |   | |   | |   | |   | |   | |   | ----- -----)]
[define (east-any)   (vector |   | |   | ----- ----- |   | |   | |   | |   |)]
[define (any-north)  (vector ----- ----- |   | |   | |   | |   | |   | |   |)]
[define (any-south)  (vector |   | |   | |   | |   | ----- ----- |   | |   |)]
[define (any-any)    (vector |   | |   | |   | |   | |   | |   | |   | |   |)]
    
(module+ test ;; creating a basic portmap 
  (define s-1 (- SIZE 1))
  (define ran (λ () (+ (random (- SIZE 2)) 1))) ;; in [1,s-1)

  (check-equal? (create-portmap0 0     0   )  (west-north))
  (check-equal? (create-portmap0 0     s-1)   (west-south))
  (check-equal? (create-portmap0 s-1   0)     (east-north))
  (check-equal? (create-portmap0 s-1   s-1)   (east-south))
  (check-equal? (create-portmap0 0     (ran)) (west-any))
  (check-equal? (create-portmap0 s-1   (ran)) (east-any))
  (check-equal? (create-portmap0 (ran) 0)     (any-north))
  (check-equal? (create-portmap0 (ran) s-1)   (any-south))
  (check-equal? (create-portmap0 (ran) (ran)) (any-any)))

;; ---------------------------------------------------------------------------------------------------
;; determine (occupied) neighbors of the square at (x,y)

#; {Board Index Index -> [Listof [List Index Index]]}
(define (neighbors* board x y)
  (define all `((,x ,(- y 1)) (,x ,(+ y 1)) (,(- x 1) ,y) (,(+ x 1) ,y)))
  (define good-ones
    (filter (match-lambda [`(,x ,y) (and (in-size x) (in-size y) (matrix-ref board y x))]) all))
  (values (map first good-ones) (map second good-ones)))


#; {Nat -> Boolean : Index}
(define (in-size z) (< -1 z SIZE))

#; {Port Nat Nat -> (values Nat Nat)}
(define (looking-at port x y)
  (case (port->index port)
    [(0 1) (values x (- y 1))]
    [(2 3) (values (+ x 1) y)]
    [(4 5) (values x (+ y 1))]
    [(6 7) (values (- x 1) y)]))

;; ---------------------------------------------------------------------------------------------------
;; drawing all tiles in a set into a drawing context

(define PLAYER-SIZE (quotient TILE-SIZE 5)) 

(define INSET  (+ 20 TILE-SIZE))
(define WIDTH  (+ INSET (* 10 TILE-SIZE) INSET))
(define HEIGHT (+ INSET (* 10 TILE-SIZE) INSET))

#; {State (Instanceof DC<%>) -> Pict}
(define (draw-board b dc)
  (match-define (state squares players) b)
  (define board-as-pict
    (let loop ([l (matrix->rectangle squares)][y 0])
      (cond
        [(empty? l) (blank)]
        [else
         (define row (first l))
         (define picts
           (for/list ((n row) (x (in-naturals)))
             (define config   (or (and (not (eq? BLANK n)) (square-tile n)) blank-tile))
             (define pict     (configuration->pict config))
             (define p-on-x-y (is-player-on players x y))
             (cond
               [(boolean? p-on-x-y) pict]
               [else
                (define color (player-name p-on-x-y))
                (define port  (player-port p-on-x-y))
                (add-player pict (jack-o-lantern PLAYER-SIZE color) port)])))
         (vl-append (apply hc-append picts) (loop (rest l) (+ y 1)))])))
  (draw-pict board-as-pict dc INSET INSET))

#; {[Listof Player] Natural Natural -> (U False Player)}
(define (is-player-on players x y)
  (define p (memf (λ (p) (match-define (player _ _ x0 y0) p) (and (= x x0) (= y y0))) players))
  (if (boolean? p) p (first p)))
  

#; {PortIndex Natural Natural -> (values Natural Natural)}
(define (logical-coordinates->geometry port x y)
  (values (* x TILE-SIZE)) (* TILE-SIZE y))

(module+ test ;; show graphical iterations 

  (define (main init-board-3-players)
    (define frame (new frame% [label "hello"][width WIDTH][height HEIGHT]))
  
    (define canvas
      (new canvas%
           [parent frame]
           [paint-callback (λ (e dc) (draw-board (init-board-3-players) dc))]))
  
    (send frame show #t))

  (main state-with-3-players)
  (main (λ () nu-state)))
