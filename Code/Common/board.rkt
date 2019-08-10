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
(require Tsuro/Code/Common/matrix)
(require pict)

(module+ test
  (require rackunit)
  (require racket/gui))

;; ---------------------------------------------------------------------------------------------------
;; data representation of the game state: the board, the players  

(define SIZE 10) ; Tsuro is played on a board with SIZE x SIZE configured tiles

;; the game state itself 
;; ------------------------------------------------------------------
#; {State   = (state Board Player*)}
#; {Player* = [Listof Player]}
#; {Player  = (player PlayerName p x y)}
#; {PlayerName = String}
#; {Board   = [Matrixof square] :: {Index x Index}}
#; {Index   = [0 .. SIZE]}
#; {Square  = (U BLANK                  ;; an unoccupied, blank square 
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
               WALL                      ;; periphery of board 
               OPEN                      ;; no neighboring tile yet 
               (next Port Index Index))} ;; points to a port on this or a neigboring square 

(struct connect [internal external] #:transparent)
(struct next [port x y] #:transparent)

(define WALL "wall") 
(define OPEN "open")

;; ---------------------------------------------------------------------------------------------------
;; data examples

(define the-empty-board (build-matrix SIZE SIZE (λ (_i _j) BLANK)))

(match-define `(,port-2 ,port-3 ,port-4) (map index->port '(2 3 4)))

(define player-red   "red")
(define player-white "white")
(define player-blue  "blue")

(define config-00 configuration1)
(define config-02 configuration1)
(define config-20 90configuration2)

(define inits-for-board-3-players
  `((,config-00 ,player-red   ,port-2 0 0)
    (,config-02 ,player-white ,port-3 0 2)
    (,config-20 ,player-blue  ,port-4 2 0)))

(define 3players (map (λ (init) (apply player (rest init))) inits-for-board-3-players))
(define red-player (first 3players))

(define (state-with-3-players #:with (with #false))
  (define square-00 (square config-00 (create-portmap config-00 0 0)))
  (define square-02 (square config-02 (create-portmap config-02 0 2)))
  (define square-20 (square config-20 (create-portmap config-20 2 0)))
  (define boatd3
    (let* ([m the-empty-board]
           [m (matrix-set m 0 0 square-00)]
           [m (matrix-set m 0 2 square-02)]
           [m (matrix-set m 2 0 square-20)])
      m))
  (define the-board (state boatd3 3players))
  (if with
      (values the-board square-00 square-02 square-20)
      the-board))

(module+ test ;; additional data samples 
  (define-values (state-3-players square-00 square-02 square-20) (state-with-3-players #:with #true))
  (define board-3-players (state-board state-3-players))
  (define config-to-add-to-board-3 configuration2))

;; ---------------------------------------------------------------------------------------------------
;; initialize a State from a list of (initial) Placements0

(define (initialize lo-placements)
  (define players (for/list ([p lo-placements]) (apply player (rest p))))
  (define board
    (for/fold ((m the-empty-board)) ((place lo-placements))
      (match-define `(,config ,_  ,_ ,x ,y) place)
      (matrix-set m x y (square config (create-portmap config x y)))))
  (state board players))

(module+ test
  (define board1
    (let* ([square-00 (square configuration1 (create-portmap configuration1 0 0))])
      (matrix-set the-empty-board 0 0 square-00)))
  (check-equal? (initialize `((,configuration1 "x" 2 0 0))) (state board1 `(,(player "x" 2 0 0))))

  (check-equal? (initialize inits-for-board-3-players) (state-with-3-players)))

;; ---------------------------------------------------------------------------------------------------
;; adding a tile T for a player P

#; {State Configuration PlayerName -> State}
;; contract:
;; -- p is on (board-players b)
;; -- the neighbor of p is unoccupied
;; assume: legality 
(define (add-tile state0 config player-name)
  (match-define  (state board players) state0)
  (match-define  (player _ port x y) (find-player players player-name))
  (define-values (x-new y-new) (looking-at port x y))
  (define nu-board (add-new-square-update-neighbors board config x-new y-new))
  (define-values (moved __eliminated) (move-players nu-board players x-new y-new))
  ;; what to do with eliminated ones 
  (state nu-board moved))

#; {(Listof Player) PlayerName -> Player}
(define (find-player players p)
  (first (memf (finder p) players)))

#; {PlayerName -> (Player -> Boolean)}
(define (finder pn)
  (compose (curry equal? pn) player-name))

(module+ test ;; add-tile 
  (define board+ (add-new-square-update-neighbors board-3-players config-to-add-to-board-3 1 0))
  (define state+ (state board+ (remf (finder player-red) (state-players state-3-players))))
  (check-equal? (add-tile state-3-players config-to-add-to-board-3 player-red) state+
                "drive red player off"))

;; ---------------------------------------------------------------------------------------------------
;; moving players 

#; {Board Player* Index Index -> (values Player* [Listof Player])}
;; move players facing (x,y), detrmine survivors, return those as the first list;
;; the second list are the drop-outs that run into walls 
(define (move-players board players x y)
  (define-values (moved out)
    (for/fold ((moved '()) (out '())) ((p players))
      (match-define  (player name port x-p y-p) p)
      (define-values (x-at y-at) (looking-at port x-p y-p))
      (cond
        [(and (= x-at x) (= y-at y))
         (define p-moved (move-one-player board p))
         (if (out? p-moved)
             (values moved (cons p-moved out))
             (values (cons p-moved moved) out))]
        [else (values (cons p moved) out)])))
  (values (reverse moved) out))

(struct out [player] #:transparent)

#; {Board Player -> (U Player (out Player))}
(define (move-one-player board the-player)
  ;; start player on (port-p, x-p, y-p) that look at an occupied neighboring square
  (match-define (player name port-p x-p y-p) the-player)
  (let move-one-player ([port-p port-p][x-p x-p][y-p y-p])
    (match-define (list port x y external) (move-one-square board port-p x-p y-p))
    (cond
      [(equal? WALL external) (out (player name port x y))]
      [(equal? OPEN external) (player name port x y)]
      [else (move-one-player port x y)])))

#; {Board Port Index Index -> [List Index Index Next]}
(define (move-one-square board port x-p y-p)
  (match-define (square _ map-p)                         (matrix-ref board x-p y-p))
  (match-define (connect _ (next port-in x-next y-next)) (vector-ref map-p (port->index port)))
  (match-define (square _ map-next)                      (matrix-ref board x-next y-next))
  (match-define (connect (next port-out x-out y-out) _)  (vector-ref map-next (port->index port-in)))
  (match-define (connect _ external)                     (vector-ref map-next (port->index port-out)))
  (list port-out x-next y-next external))

(module+ test ;; move player  
  (define red-player (find-player 3players player-red))
  (check-equal? (move-one-square board+ (player-port red-player) 0 0) (list (index->port 1) 1 0 WALL)
                "moved red player 1 step")

  (check-equal? (move-one-player board+ red-player) (out (player player-red (index->port 1) 1 0))
                "move red player all the way"))

;; ---------------------------------------------------------------------------------------------------
;; updadting the board

#; {Board Confguration Index Index -> Matrix}

(define (add-new-square-update-neighbors board config x y)
  (define-values (x-n* y-n*) (neighbors* board x y))
  (for/fold ((m (matrix-set board x y (create-square board config x y)))) ((x-n x-n*) (y-n y-n*))
    (matrix-set m x-n y-n (update-square (matrix-ref m x-n y-n) x-n y-n x y))))

(module+ test
  (define nu-square  (create-square board-3-players config-to-add-to-board-3 1 0))
  (define nu-board (let* ([m board-3-players]
                          [m (matrix-set m 1 0 nu-square)]
                          [m (matrix-set m 0 0 (update-square square-00 0 0 1 0))]
                          [m (matrix-set m 2 0 (update-square square-20 2 0 1 0))])
                     m))
  
  (check-equal?
   (matrix->rectangle 
    (add-new-square-update-neighbors board-3-players config-to-add-to-board-3 1 0))
   (matrix->rectangle 
    nu-board)))

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

  (check-equal? (create-square board-3-players config-to-add-to-board-3 1 0)
                (square config-to-add-to-board-3
                        (let* ([pm (create-portmap config-to-add-to-board-3 1 0)]
                               [pm (update-portmap pm 1 0 2 0)]
                               [pm (update-portmap pm 1 0 0 0)])
                          pm))))

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
    (let* ([pm (create-portmap config-to-add-to-board-3 1 0)]
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
    (filter (match-lambda [`(,x ,y) (and (in-size x) (in-size y) (matrix-ref board x y))]) all))
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

#; {Player* Natural Natural -> (U False Player)}
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

    (void)
    #;
    (send frame show #t))

  (main state-with-3-players)
  (main (λ () state+)))
