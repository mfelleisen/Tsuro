#lang racket


(require Tsuro/Code/Common/tiles)
(require htdp/matrix)
(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; data representation of the game board 

(define SIZE 10) ; Tsuro is played on a board with SIZE x SIZE configured tiles

(define WALL 'wall)
(define OPEN 'open)

#; {Board   = (board [Matrixof Node] [Listof Player])}
#; {Player  = (player p x y)}
#; {Node    = (node Tile PortMap)}
#; {PortMap = (Vectorof Connect) :: [Port ->f Connect]}
#; {Connect = (connect Next Next)}
#; {Next    = (U WALL OPEN (next Port Index Index))}
#; {Index   = [0 .. SIZE]}

#; {Placement0 = [List Configuration PortIndex Index Index]
               where (list c p x y) must satisfy the following conditions: 
               1. (x,y) must describe a position at the periphery of the board 
               2. p must be a port that faces an empty square}

(struct board [nodes players] #:transparent)
(struct player [port x y] #:transparent)
(struct node [tile map] #:transparent)
(struct connect [one two] #:transparent)
(struct next [port x y] #:transparent)

;; ---------------------------------------------------------------------------------------------------
;; initialize a board from a list of (initial) Placements0
#; { [Listof Placement0] -> Board }
(define (initialize lo-placements)
  (define players (for/list ([p lo-placements]) (apply player (rest p))))
  (define matrix
    (for/fold ((m matrix0)) ((p lo-placements))
      (match-define `(,c ,_ ,x ,y) p)
      (matrix-set m x y (node c (create-portmap x y)))))
  (board matrix players))

(define matrix0
  (build-matrix 20 20
                (lambda (i j)
                  (node (if (= i 0) 'wall 'open)
                        (if (= j 0) 'wall 'open)))))

(module+ test
  (define 00-tile (node configuration1 (create-portmap 0 0)))
  (define matrix1 (matrix-set matrix0 0 0 00-tile))

  (check-equal? (initialize (list (list configuration1 2 0 0))) (board matrix1 (list (player 2 0 0))))
  
  (define matrix2
    (let* ([m matrix1]
           [m (matrix-set m 0 2 (node configuration2 (create-portmap 0 2)))]
           [m (matrix-set m 2 0 (node 90configuration2 (create-portmap 2 0)))])
      m))
  (define inits2
    `((,configuration1 2 0 0)
      (,configuration2 3 0 2)
      (,90configuration2 3 2 0)))
  (define players2 `(,(player 2 0 0) ,(player 3 0 2) ,(player 3 2 0)))
  (check-equal? (initialize inits2) (board matrix2 players2)))

;; ---------------------------------------------------------------------------------------------------
;; create a portmap for the given indicies 
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
