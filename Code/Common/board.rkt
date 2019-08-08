#lang racket

;; a data representation for game boards, plus basic functions for manipulating them

(provide
 ;; type Board 
 init-board-3-players

 #; {Placement0 = [List Configuration PortIndex Index Index]
                where (list c p x y) must satisfy the following conditions: 
                1. (x,y) must describe a position at the periphery of the board 
                2. p must be a port that faces an empty square}

 #; { [Listof Placement0] -> Board }
 initialize)


;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/tiles)
(require htdp/matrix)
(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; data representation of the game board 

(define SIZE 10) ; Tsuro is played on a board with SIZE x SIZE configured tiles

(define WALL 'wall)
(define OPEN 'open)

#; {Board   = (board [Matrixof Node] [Listof Player])}
#; {Player  = (player String p x y)}
#; {Node    = (node Tile PortMap)}
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

(define matrix0
  (build-matrix 20 20
                (lambda (i j)
                  (node (if (= i 0) 'wall 'open)
                        (if (= j 0) 'wall 'open)))))

(define (init-board-3-players)
  (define matrix3
    (let* ([m matrix0]
           [m (matrix-set m 0 0 (node configuration1 (create-portmap 0 0)))]
           [m (matrix-set m 0 2 (node configuration2 (create-portmap 0 2)))]
           [m (matrix-set m 2 0 (node 90configuration2 (create-portmap 2 0)))])
      m))
  (define players3 `(,(player "red" 2 0 0) ,(player "white" 3 0 2) ,(player "blue" 3 2 0)))
  (board matrix3 players3))

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
    `((,configuration1 "red" 2 0 0) (,configuration2 "white" 3 0 2) (,90configuration2 "blue" 3 2 0)))
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