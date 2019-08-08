#lang racket

;; a data representation for game boards, plus basic functions for manipulating them

;; TODO
;; - render a board as a pict
;; - factor out matrix, in case it needs to be replaced 
;; - add configured tile T for player P: find place, revise portmap, move all players

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
(require Tsuro/Code/Lib/or)
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

#; {Board   = (board [Matrixof Node] [Listof Player])}
#; {Player  = (player String p x y)}
#; {Node    = (U False (node Tile PortMap))}
#; {PortMap = (Vectorof Connect) :: [Port ->f Connect]}
#; {Connect = (connect Next Next)}
#; {Next    = (U WALL OPEN (next Port Index Index))}
#; {Index   = [0 .. SIZE]}

(struct board [nodes players] #:transparent)
(struct player [name port x y] #:transparent)
(struct node [tile map] #:transparent)
(struct connect [one two] #:transparent)
(struct next [port x y] #:transparent)

;; ---------------------------------------------------------------------------------------------------
;; data examples

(define matrix0 (build-matrix SIZE SIZE (λ (_i _j) BLANK)))

(match-define `(,p2 ,p3) (map index->port '(2 3)))

(define (init-board-3-players)
  (define matrix3
    (let* ([m matrix0]
           [m (matrix-set m 0 0 (node configuration1 (create-portmap 0 0)))]
           [m (matrix-set m 0 2 (node configuration2 (create-portmap 0 2)))]
           [m (matrix-set m 2 0 (node 90configuration2 (create-portmap 2 0)))])
      m))
  (define players3 `(,(player "red" p2 0 0) ,(player "white" p3 0 2) ,(player "blue" p3 2 0)))
  (board matrix3 players3))

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
             (define config   (or (and n (node-tile n)) blank-tile))
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

(module+ test
  
  (define frame (new frame% [label "hello"][width WIDTH][height HEIGHT]))
  
  (define canvas
    (new canvas%
         [parent frame]
         [paint-callback (λ (e dc) (draw-board (init-board-3-players) dc))]))
  
  (send frame show #t))

;; ---------------------------------------------------------------------------------------------------
;; initialize a board from a list of (initial) Placements0

(define (initialize lo-placements)
  (define players (for/list ([p lo-placements]) (apply player (rest p))))
  (define matrix
    (for/fold ((m matrix0)) ((p lo-placements))
      (match-define `(,c ,_  ,_ ,x ,y) p)
      (matrix-set m x y (node c (create-portmap x y)))))
  (board matrix players))

(module+ test
  (define 00-tile (node configuration1 (create-portmap 0 0)))
  (define matrix1 (matrix-set matrix0 0 0 00-tile))

  (check-equal? (initialize `((,configuration1 "x" 2 0 0))) (board matrix1 `(,(player "x" 2 0 0))))
  
  (define inits2
    `((,configuration1 "red" ,p2 0 0)
      (,configuration2 "white" ,p3 0 2)
      (,90configuration2 "blue" ,p3 2 0)))
  (check-equal? (initialize inits2) (init-board-3-players)))

;; ---------------------------------------------------------------------------------------------------
;; create an initial portmap for the given indicies 
#; { Index Index -> PortMap }

(define-match-expander ??
  (λ (stx)
    (syntax-case stx ()
      [(_ w) #'(? (curry = (- w 1)))])))

(define ----- (connect WALL WALL))
(define |   | (connect OPEN OPEN))

(define (create-portmap x y)
  (match* (x y)
    [(0  0)                west-north]
    [(0 (?? SIZE))         west-south]
    [((?? SIZE) 0)         east-north]
    [((?? SIZE) (?? SIZE)) east-south]
    [(0  n)                west-any  ]
    [((?? SIZE) n)         east-any  ]
    [(n 0)                 any-north ]
    [(n (?? SIZE))         any-south ]
    [(n k)                 any-any   ]))

;; ---------------- ports:   0     1     2     3     4     5     6     7 ---
;;                            north       east        south       west  
[define west-north (vector ----- ----- |   | |   | |   | |   | ----- -----)]
[define west-south (vector |   | |   | |   | |   | ----- ----- ----- -----)]
[define east-north (vector ----- ----- ----- ----- |   | |   | |   | |   |)]
[define east-south (vector |   | |   | ----- ----- ----- ----- |   | |   |)]
[define west-any   (vector |   | |   | |   | |   | |   | |   | ----- -----)]
[define east-any   (vector |   | |   | ----- ----- |   | |   | |   | |   |)]
[define any-north  (vector ----- ----- |   | |   | |   | |   | |   | |   |)]
[define any-south  (vector |   | |   | |   | |   | ----- ----- |   | |   |)]
[define any-any    (vector |   | |   | |   | |   | |   | |   | |   | |   |)]
    
(module+ test
  (define s-1 (- SIZE 1))
  (define ran (λ () (+ (random (- SIZE 2)) 1))) ;; in [1,s-1)

  (check-equal? (create-portmap 0     0   )  west-north)
  (check-equal? (create-portmap 0     s-1)   west-south)
  (check-equal? (create-portmap s-1   0)     east-north)
  (check-equal? (create-portmap s-1   s-1)   east-south)
  (check-equal? (create-portmap 0     (ran)) west-any)
  (check-equal? (create-portmap s-1   (ran)) east-any)
  (check-equal? (create-portmap (ran) 0)     any-north)
  (check-equal? (create-portmap (ran) s-1)   any-south)
  (check-equal? (create-portmap (ran) (ran)) any-any))
