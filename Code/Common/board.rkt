#lang racket

;; a data representation for game States, plus basic functions for manipulating them

;; TODO
;; -- catch infinitely looping player 
;; -- JSON de/serialozation 

;; safety
;; - contract for initialize and add?

;; legality checks: 
;; - determine whether the addition of a tile for a player P forces P to commit suicide
;; - determine whether the addition of a tile adds a cyclic path

;; ---------------------------------------------------------------------------------------------------
;; contrat section

(require (only-in Tsuro/Code/Common/port-alphabetic port?))
(require (only-in Tsuro/Code/Common/tiles tile?))

(define SIZE 10) ; Tsuro is played on a board with SIZE x SIZE configured tiles

#; {Nat -> Boolean : Index}
(define (index? z) (< -1 z SIZE))

;; -----------------------------------------------------------------------------
;; placements

#; {Placement0 = [List Tile PlayerName PortIndex Index Index] subjectto}
               
#; { Placement0 -> Boolean : (x,y) describe a square position near the peruphery }
(define (x-and-y-on-periphery? l)
  (match-define (list c name p x y) l)
  (or (= x 0) (= x SIZE) (= y 0) (= y SIZE)))

#; { Placement0 -> Boolean : port element faces inside }
(define (port-facing-inward? l)
  (match-define (list c name p x y) l)
  (define-values (x-look y-look) (looking-at p x y))
  (and (index? x-look) (index? y-look)))

;; contratc combinator
#;
[list/dc
  [t tile?]
  [n string?]
  [p port?]
  [x index?]
  [y index?]
  #;(lambda (l) (match-define (t n p x y) l) ...)
  #:post (x y) (or (= x 0) (= x SIZE) (= y 0) (= y SIZE))
  #:post (p x y) (let-values ([(x-look y-look) (looking-at p x y)]) (and (index? x-look) (index? y-look)))]
(define placement0/c
  (and/c 
   (list/c tile? string? port? index? index?)
   x-and-y-on-periphery?
   port-facing-inward?))

#; { [Listof Placement0] -> Boolean : locations are distinct and not neighboring }
(define (locations-distinct-and-not-neighboring l)
  (define locations (map cdddr l))
  (and (locations-distinct locations) (not-neighboring locations)))

#; { [Listof (List Index Index)] -> Boolean : locations are distinct }
(define (locations-distinct locations)
  (= (length locations) (set-count (apply set locations))))

#; { [Listof (List Index Index)] -> Boolean : locations are not neighboring }
(define (not-neighboring locations)
  (for/and ((loc (in-list locations)))
    (define all-but (remove loc locations))
    (for/and ((n (in-list (apply neighbors* the-empty-board loc))))
      (not (member n all-but)))))

;; contract combinator 
(define placements0*/c
  (and/c (listof placement0/c)
         locations-distinct-and-not-neighboring))

;; -----------------------------------------------------------------------------
;; add tile 
#; { State -> (-> PlayerName Boolean : the player is on the list of players)}
(define ((part-of s) pname)
  (cons? (memf (finder pname) (state-players s))))

;; -----------------------------------------------------------------------------
;; state

#; { State -> Boolean : every player faces an open square }
(define (every-player-faces-open-square s)
  (match-define (state board players) s)
  (for/and ((p players))
    (match-define (player name port x y) p)
    (define-values (x-look y-look) (looking-at port x y))
    (boolean? (matrix-ref board x-look y-look))))

;; for tests of contracts, see below data examples 

(provide
 ;; type State
 ;; all players are on ports that face empty squares on the board 
 state?

 ; (-> state?)
 ;; creates a sample state 
 state-with-3-players
 
 (contract-out 
  [initialize
   ;; creates a state from a list of initial placements 
   (-> placements0*/c (and/c state? every-player-faces-open-square))]
  
  [add-tile
   ;; place a configured tile on the empty square that the player pn neighbors
   (->i ([s state?][c tile?][pn (s) (and/c string? (part-of s))])
        [result (and/c state? every-player-faces-open-square)])]))

;; ---------------------------------------------------------------------------------------------------
(require (except-in Tsuro/Code/Common/tiles tile?))
(require (except-in Tsuro/Code/Common/port-alphabetic port?))
(require Tsuro/Code/Common/matrix)
(require pict)

(module+ test
  (require (submod ".."))
  (require rackunit)
  (require racket/gui))

;; ---------------------------------------------------------------------------------------------------
;; data representation of the game state: the board, the players  

;; the game state itself 
;; ------------------------------------------------------------------
#; {State   = (state Board Player*)}
#; {Player* = [Listof Player]}
#; {Player  = (player PlayerName p x y)}
#; {PlayerName = String}
#; {Board   = [Matrixof square] :: {Index x Index}}
#; {Index   = [0 .. SIZE]}
#; {Square  = (U BLANK                   ;; an unoccupied, blank square 
                 (square Tile PortMap))} ;; a configured tile with connections to neighbors cached 

(struct state  [board players] #:transparent)
(struct player [name port x y] #:transparent)
(struct square [tile map] #:transparent)
(define BLANK #false)

;; a fast means for looking up the "bridges" to neighboring squares
;; ------------------------------------------------------------------
#; {PortMap = (Vectorof Next) :: [Port ->f Next]}
#; {Next    = (U
               WALL                      ;; periphery of board 
               OPEN                      ;; no neighboring tile yet 
               (next Port Index Index))} ;; points to a port on this or a neigboring square 

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

(define tile-00 tile1)
(define tile-02 tile1)
(define tile-20 90tile2)

(define inits-for-state-with-3-players
  `((,tile-00 ,player-red   ,port-2 0 0)
    (,tile-02 ,player-white ,port-3 0 2)
    (,tile-20 ,player-blue  ,port-4 2 0)))

(define 3players (map (λ (init) (apply player (rest init))) inits-for-state-with-3-players))
(define red-player (first 3players))

(define (state-with-3-players #:with (with #false))
  (define square-00 (square tile-00 (create-portmap 0 0)))
  (define square-02 (square tile-02 (create-portmap 0 2)))
  (define square-20 (square tile-20 (create-portmap 2 0)))
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
  (define tile-to-add-to-board-3 tile2)
  (define placement1 (first inits-for-state-with-3-players)))

(module+ test ;; contracts
  (check-true (placement0/c placement1))
  (check-true (placements0*/c inits-for-state-with-3-players))
  (check-true ((part-of (state-with-3-players)) player-red)))

;; ---------------------------------------------------------------------------------------------------
;; initialize a State from a list of (initial) Placements0

(define (initialize lo-placements)
  (define players
    (for/list ([p (in-list lo-placements)])
      (apply player (rest p))))
  (define board
    (for/fold ((m the-empty-board)) ((placement (in-list lo-placements)))
      (match-define `(,tile ,_  ,_ ,x ,y) placement)
      (matrix-set m x y (square tile (create-portmap x y)))))
  (state board players))

(module+ test
  (define board1
    (let* ([square-00 (square tile1 (create-portmap 0 0))])
      (matrix-set the-empty-board 0 0 square-00)))
  (check-exn exn:fail:contract? (λ () (initialize `((,tile1 "x" 2 0 0)))) "port, not index")

  (check-equal? (initialize inits-for-state-with-3-players) (state-with-3-players)))

;; ---------------------------------------------------------------------------------------------------
;; adding a tile T for a player P

(define (add-tile state0 tile player-name)
  (match-define  (state board players) state0)
  (match-define  (player _ port x y) (find-player players player-name))
  (define-values (x-new y-new) (looking-at port x y))
  (define nu-board (add-new-square-update-neighbors board tile x-new y-new))
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
  (define board+ (add-new-square-update-neighbors board-3-players tile-to-add-to-board-3 1 0))
  (define state+ (state board+ (remf (finder player-red) (state-players state-3-players))))
  (check-equal? (add-tile state-3-players tile-to-add-to-board-3 player-red) state+
                "drive red player off"))

;; ---------------------------------------------------------------------------------------------------
;; moving players 

#; {Board Player* Index Index -> (values Player* [Listof Player])}
;; move players facing (x,y), detrmine survivors, return those as the first list;
;; the second list are the drop-outs that run into walls 
(define (move-players board players x y)
  (define-values (moved out)
    (for/fold ((moved '()) (out '())) ((p (in-list players)))
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
;; move player at (x-p, y-p) on port port-p "thru" the tile of the next square
;; ASSUME the square is occupired with a tile 
(define (move-one-square board port x-p y-p)
  (match-define (square tile-p map-p)        (matrix-ref board x-p y-p))
  (match-define (next port-in x-next y-next) (vector-ref map-p (port->index port)))
  (match-define (square tile-next map-next)  (matrix-ref board x-next y-next))
  (define port-out (tile-next port-in))
  (define external (vector-ref map-next (port->index port-out)))
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

(define (add-new-square-update-neighbors board tile x y)
  (define new-square (create-square board tile x y))
  (for*/fold ((m (matrix-set board x y new-square))) ((g (neighbors* board x y)))
    (match-define (list x-n y-n) g)
    (define old-neighbor-square  (matrix-ref m x-n y-n))
    (define updated-neighbor-sq  (update-square old-neighbor-square x-n y-n x y))
    (matrix-set m x-n y-n updated-neighbor-sq)))

(module+ test
  (define nu-square (create-square board-3-players tile-to-add-to-board-3 1 0))
  (define nu-board  (let* ([m board-3-players]
                           [m (matrix-set m 1 0 nu-square)]
                           [m (matrix-set m 0 0 (update-square square-00 0 0 1 0))]
                           [m (matrix-set m 2 0 (update-square square-20 2 0 1 0))])
                      m))
  
  (check-equal?
   (matrix->rectangle 
    (add-new-square-update-neighbors board-3-players tile-to-add-to-board-3 1 0))
   (matrix->rectangle 
    nu-board)))

;; ---------------------------------------------------------------------------------------------------
;; creating and updatiing squares 

#; {Board Tile Index Index -> square}
;; create a square at (x,y) from Tile with current matrix n*
(define (create-square board tile x y)
  (define portmap
    (for*/fold ((portmap (create-portmap x y))) ((g (neighbors* board x y)))
      (apply update-portmap portmap x y g)))
  (square tile portmap))

#; {Square Index Index Index Index -> square}
(define (update-square old x-old y-old x-new y-new)
  (square (square-tile old) (update-portmap (square-map old) x-old y-old x-new y-new)))

(module+ test ;; operating on squares
  (check-equal? (update-square square-00 0 0 1 0)
                (square tile-00 (update-portmap (square-map square-00) 0 0 1 0)))

  (check-equal? (create-square board-3-players tile-to-add-to-board-3 1 0)
                (square tile-to-add-to-board-3
                        (let* ([pm (create-portmap 1 0)]
                               [pm (update-portmap pm 1 0 2 0)]
                               [pm (update-portmap pm 1 0 0 0)])
                          pm))))

;; ---------------------------------------------------------------------------------------------------
;; creating a portmap, adding external connections to portmap

#; {Index Index -> PortMap }
;; create the default portmap for a square 
(define-match-expander ??
  (λ (stx)
    (syntax-case stx ()
      [(_ w) #'(? (curry = (- w 1)))])))

(define (create-portmap x y)
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

;; ---------------- ports:    0   1   2    3    4    5    6    7 ---
;;                            north    east      south     west  
[define (west-north) (vector WALL WALL OPEN OPEN OPEN OPEN WALL WALL)]
[define (west-south) (vector OPEN OPEN OPEN OPEN WALL WALL WALL WALL)]
[define (east-north) (vector WALL WALL WALL WALL OPEN OPEN OPEN OPEN)]
[define (east-south) (vector OPEN OPEN WALL WALL WALL WALL OPEN OPEN)]
[define (west-any)   (vector OPEN OPEN OPEN OPEN OPEN OPEN WALL WALL)]
[define (east-any)   (vector OPEN OPEN WALL WALL OPEN OPEN OPEN OPEN)]
[define (any-north)  (vector WALL WALL OPEN OPEN OPEN OPEN OPEN OPEN)]
[define (any-south)  (vector OPEN OPEN OPEN OPEN WALL WALL OPEN OPEN)]
[define (any-any)    (vector OPEN OPEN OPEN OPEN OPEN OPEN OPEN OPEN)]
    
(module+ test ;; creating a basic portmap 
  (define s-1 (- SIZE 1))
  (define ran (λ () (+ (random (- SIZE 2)) 1))) ;; in [1,s-1)

  (check-equal? (create-portmap 0     0   )  (west-north))
  (check-equal? (create-portmap 0     s-1)   (west-south))
  (check-equal? (create-portmap s-1   0)     (east-north))
  (check-equal? (create-portmap s-1   s-1)   (east-south))
  (check-equal? (create-portmap 0     (ran)) (west-any))
  (check-equal? (create-portmap s-1   (ran)) (east-any))
  (check-equal? (create-portmap (ran) 0)     (any-north))
  (check-equal? (create-portmap (ran) s-1)   (any-south))
  (check-equal? (create-portmap (ran) (ran)) (any-any)))

#; {PortMap Index Index Index Index -> PortMap}
;; update the external parts of the portmap at (x-pm,y-pm) to connect with neighbors on n at (x-n,y-n)
(define (update-portmap portmap x-pm y-pm x-new y-nnew)
  (for/vector ((pi (in-range (vector-length portmap))))
    (define port (index->port pi))
    (define-values (x-look y-look) (looking-at port x-pm y-pm))
    (if (and (eq? x-look x-new) (eq? y-look y-nnew))
        (next (facing-port port) x-new y-nnew)
        (vector-ref portmap pi))))

(module+ test ;; adding external connections to a portmap 
  (define nu-pm
    (let* ([pm (create-portmap 1 0)]
           [pm (update-portmap pm 1 0 2 0)]
           [pm (update-portmap pm 1 0 0 0)])
      pm))
  (define pm3+
    (let ([v (vector-copy nu-pm)])
      (vector-set! v 2 (next (facing-port (index->port 2)) 2 0))
      (vector-set! v 3 (next (facing-port (index->port 3)) 2 0))
      v))
          
  (check-equal? (update-portmap nu-pm 1 0 2 0) pm3+))


;; ---------------------------------------------------------------------------------------------------
;; determine (occupied) neighbors of the square at (x,y)

#; {Board Index Index -> [values [Listof Index] [Listof Index]]}
(define (neighbors* board x y)
  (define all `((,x ,(- y 1)) (,x ,(+ y 1)) (,(- x 1) ,y) (,(+ x 1) ,y)))
  (filter (match-lambda [`(,x ,y) (and (index? x) (index? y) (matrix-ref board x y))]) all))

#; {Port Index Index -> (values Integer Integer)}
(define (looking-at port x y)
  (case (port->direction port)
    [(NORTH) (values x (- y 1))]
    [(EAST)  (values (+ x 1) y)]
    [(SOUTH) (values x (+ y 1))]
    [(WEST)  (values (- x 1) y)]))

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
           (for/list ((n (in-list row)) (x (in-naturals)))
             (define tile   (or (and (not (eq? BLANK n)) (square-tile n)) blank-tile))
             (define pict     (tile->pict tile))
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
