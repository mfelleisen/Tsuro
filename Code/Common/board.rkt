#lang racket

;; a data representation for game boards, plus basic functions for manipulating them

;; TODO
;; - factor out matrix, in case it needs to be replaced 
;; - add configured tile T for player P: move all players

;; safety
;; - contract for initialize and add?

;; legality checks: 
;; - determine whether the addition of a tile for a player P forces P to commit suicide
;; - determine whether the addition of a tile adds a cyclic path 

(provide
 ;; type Board

 #; {-> Board}
 init-board-3-players

 #; {Placement0 = [List Configuration PortIndex Index Index]
                where (list c p x y) must satisfy the following conditions: 
                1. (x,y) must describe a position at the periphery of the board 
                2. p must be a port that faces an empty square}

 #; { [Listof Placement0] -> Board }
 initialize)


;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/port-alphabetic)
(require)
(require htdp/matrix)
(require pict)

(module+ test
  (require rackunit)
  (require racket/gui))

;; ---------------------------------------------------------------------------------------------------
;; data representation of the game board 

(define SIZE 10) ; Tsuro is played on a board with SIZE x SIZE configured tiles

(define WALL 'wall)
(define OPEN 'open)

(define BLANK #false)

#; {Board   = (board Nodes* [Listof Player])}
#; {Player  = (player String p x y)}
#; {Nodes*  = [Matrixof Node] :: {Index x Index}}
#; {Index   = [0 .. SIZE]}
#; {Node    = (U False (node Tile PortMap))}

;; the portmap is a fast means for looking up the neighboring nodes 
#; {PortMap = (Vectorof Connect) :: [Port ->f Connect]}
#; {Connect = (connect Next Next)}
#; {Next    = (U
               WALL      ;; periphery of board 
               OPEN      ;; no neighboring tile yet 
               (next Port Index Index))} ;; points to a port on this tile or a neigboring one 

(struct board [nodes players] #:transparent)
(struct player [name port x y] #:transparent)
(struct node [tile map] #:transparent)
(struct connect [internal external] #:transparent)
(struct next [port x y] #:transparent)

;; ---------------------------------------------------------------------------------------------------
;; data examples

(define the-empty-matrix (build-matrix SIZE SIZE (λ (_i _j) BLANK)))

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

(define (init-board-3-players #:with-nodes (with #false))
  (define node-00 (node config-00 (create-portmap config-00 0 0)))
  (define node-02 (node config-02 (create-portmap config-02 0 2)))
  (define node-20 (node config-20 (create-portmap config-20 2 0)))
  (define matrix3
    (let* ([m the-empty-matrix]
           [m (matrix-set m 0 0 node-00)]
           [m (matrix-set m 2 0 node-02)]
           [m (matrix-set m 0 2 node-20)])
      m))
  (define players3 (map (λ (init) (apply player (rest init))) inits-for-board-3-players))
  (define the-board (board matrix3 players3))
  (if with
      (values the-board node-00 node-02 node-20)
      the-board))

(module+ test ;; additional data samples 
  (define-values (board-3-players node-00 node-02 node-20) (init-board-3-players #:with-nodes #true))
  (define matrix-3-players (board-nodes board-3-players))
  (define config-to-be-added-to-board-with-3 configuration2))

;; ---------------------------------------------------------------------------------------------------
;; adding a tile T for a player P

#; {Board Configuration PlayerName -> Board}
;; contract:
;; -- p is on (board-players b)
;; -- the neighbor of p is unoccupied
;; assume: legality 
(define (add-tile b c p)
  (match-define (player _ port x y) (find-player b p))
  (define-values (x-new y-new) (looking-at port x y))
  (define nu-matrix (add-new-node-update-neighbors (board-nodes b) c x-new y-new))
  (board nu-matrix (board-players b)))

#; {Board PlayerName -> Player}
(define (find-player b p)
  (first (memf (finder p) (board-players b))))

#; {Configuration Index Index -> Portmap}
(define (portmap-for c x-new y-new)
  (for/vector ((e (configuration->list-map c))) (connect (edge-to e) 'foo)))

#; {PlayerName -> (Player -> Boolean)}
(define (finder p)
  (compose (curry equal? p) player-name))

(module+ test ;; add-tile 
  
  (define nu-board
    (board
     (add-new-node-update-neighbors matrix-3-players config-to-be-added-to-board-with-3 1 0)
     (remf (finder player-00) (board-players board-3-players))))
  
  (check-equal? (add-tile board-3-players config-to-be-added-to-board-with-3 player-00) nu-board
                "drive red player off"))

;; ---------------------------------------------------------------------------------------------------
;; updadting the matrix

#; {Matrix Confguration Index Index -> Matrix}

(define (add-new-node-update-neighbors matrix c x y)
  (define-values (x-n* y-n*) (neighbors* matrix x y))
  (for/fold ((m (matrix-set matrix y x (create-node matrix c x y)))) ((x-n x-n*) (y-n y-n*))
    (matrix-set m y-n x-n (update-node (matrix-ref m y-n x-n) x-n y-n x y))))

(module+ test
  (define nu-node    (create-node matrix-3-players config-to-be-added-to-board-with-3 1 0))
  (define nu-matrix  (let* ([m matrix-3-players]
                            [m (matrix-set m 0 1 nu-node)]
                            [m (matrix-set m 0 0 (update-node node-00 0 0 1 0))]
                            [m (matrix-set m 0 2 (update-node node-20 2 0 1 0))])
                       m))
  
  (check-equal?
   (add-new-node-update-neighbors matrix-3-players config-to-be-added-to-board-with-3 1 0)
   nu-matrix))

;; ---------------------------------------------------------------------------------------------------
;; creating and updatiing nodes 

#; {Nodes* Configuration Index Index -> Node}
;; create a node at (x,y) from configuration with current matrix n*
(define (create-node matrix c x y)
  (define-values (x-n* y-n*) (neighbors* matrix x y))
  (define pm
    (for/fold ((pm (create-portmap c x y))) ((x-n x-n*)[y-n y-n*])
      (update-portmap pm x y x-n y-n)))
  (node c pm))

#; {Node Index Index Node Index Index -> Node}
(define (update-node old x-old y-old x-new y-new)
  (node (node-tile old) (update-portmap (node-map old) x-old y-old x-new y-new)))

(module+ test ;; operating on nodes
  (check-equal? (update-node node-00 0 0 1 0)
                (node config-00
                      (let* ([pm (node-map node-00)]
                             [pm (update-portmap pm 0 0 1 0)])
                        pm)))

  (check-equal? (create-node matrix-3-players config-to-be-added-to-board-with-3 1 0)
                (node config-to-be-added-to-board-with-3
                      (let* ([pm (create-portmap config-to-be-added-to-board-with-3 1 0)]
                             [pm (update-portmap pm 1 0 2 0)]
                             [pm (update-portmap pm 1 0 0 0)])
                        pm))))

;; ---------------------------------------------------------------------------------------------------
;; initialize a board from a list of (initial) Placements0

(define (initialize lo-placements)
  (define players (for/list ([p lo-placements]) (apply player (rest p))))
  (define matrix
    (for/fold ((m the-empty-matrix)) ((p lo-placements))
      (match-define `(,c ,_  ,_ ,x ,y) p)
      (matrix-set m y x (node c (create-portmap c x y)))))
  (board matrix players))

(module+ test
  (define matrix1
    (let* ([node-00 (node configuration1 (create-portmap configuration1 0 0))])
      (matrix-set the-empty-matrix 0 0 node-00)))
  (check-equal? (initialize `((,configuration1 "x" 2 0 0))) (board matrix1 `(,(player "x" 2 0 0))))

  (check-equal? (initialize inits-for-board-3-players) (init-board-3-players)))

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

#; { Index Index -> PortMap }
(define (create-portmap c x y)
  (for/vector ((e (in-list (configuration->list-map c))) (c (in-vector (create-portmap0 x y))))
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

#; {Matrix Index Index -> [Listof [List Index Index]]}
(define (neighbors* m x y)
  (define candidates `((,x ,(- y 1)) (,x ,(+ y 1)) (,(- x 1) ,y) (,(+ x 1) ,y)))
  (define good-ones
    (filter (match-lambda [`(,x ,y) (and (in-size x) (in-size y) (matrix-ref m y x))]) candidates))
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

#; {Board (Instanceof DC<%>) -> Pict}
(define (draw-board b dc)
  (match-define (board nodes players) b)
  (define board-as-pict
    (let loop ([l (matrix->rectangle nodes)][y 0])
      (cond
        [(empty? l) (blank)]
        [else
         (define row (first l))
         (define picts
           (for/list ((n row) (x (in-naturals)))
             (define config   (or (and (not (eq? BLANK n)) (node-tile n)) blank-tile))
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

  (main init-board-3-players)
  (main (λ () nu-board)))
