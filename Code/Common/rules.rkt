#lang racket

;; a Tsuro rule checker: it checks whether the requested player action is legal
;; and, if so, computes the new state 

(require Tsuro/Code/Common/actions)
(require (only-in Tsuro/Code/Common/board state?))
(require (only-in Tsuro/Code/Common/tokens avatar?))

(define (ok s) (or/c #false s))

(provide
 (contract-out
  [legal-initial
   (->i ([s initial-state?]
         [c (s) (and/c avatar? (compose not (curry set-member? (survivors s))))]
         [t1 tile-index?]
         [t2 tile-index?]
         [t3 tile-index?]
         [ia init-action/c])
        (r (ok initial-state?)))]
  [legal-take-turn
   (->i ([s state?]
         [c (s) (and/c avatar? (curry set-member? (survivors s)))]
         [t1 tile-index?]
         [t2 tile-index?]
         [ta turn-action/c])
         (r (ok state?)))]))

;; ---------------------------------------------------------------------------------------------------
(require (except-in Tsuro/Code/Common/board state?))
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/port)
(require SwDev/Debugging/spy)

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test-cases))
  (require (submod Tsuro/Code/Common/board picts))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; A player gets handed a board representation and responds with an action.

;; An init-action/c action specifies a tile placement
;; and on which port of the tile the avatar is placed.
;; It is legal if the tile
;; -- is placed at the periphery of the board
;; -- does not neighbor an already placed tile in a cardinal direction
;; -- the player faces an empty square on the board.

(define (legal-initial state0 player given-ti1 given-ti2 given-ti3 ia)
  (match-define (list (list ti d) p x y) ia)
  
  (define spot (rest ia))
  (cond
    [(not (or (equal? ti given-ti1) (equal? ti given-ti2) (equal? ti given-ti3))) #false]
    [((dont-use-taken-spot/c state0) spot)
     (define tile (rotate-tile (tile-index->tile ti) #:degree d))
     (define state+1 (place-first-tile state0 player tile spot))
     (if (bad-spot? state+1) #false (and (initial-state? state+1) state+1))]
    [else #false]))

;; ---------------------------------------------------------------------------------------------------
;; A  turn-action/c merely specifies a tile index and a rotation.
;; It is legal if placing the tile does not lead to
;; -- the suicide of the player's avatar unless all options demand it 
;; -- the infinite looping of any avatar, including the player's

(define (legal-take-turn state player given-ti1 given-ti2 ta)
  (match-define (list ti d) ta)
  (cond
    [(not (or (equal? ti given-ti1) (equal? ti given-ti2))) #false]
    [else 
     (define tile (rotate-tile (tile-index->tile ti) #:degree d))
     (define state+1 (add-tile state player tile))
     (cond
       [(collided? state+1) (collided-state state+1)]
       [(infinite? state+1)
        (and (all-infinite-suicide? state player given-ti1 given-ti2)
             (for/fold ((state state)) ([avatar (infinite-player state+1)])
               (minus-player state avatar)))]
       [(suicide? state+1 player)
        (if (all-suicide? state player given-ti1 given-ti2) state+1 #false)]
       [else state+1])]))

#; {State Player -> Boolean}
;; is player a dead in state? 
(define (suicide? state player)
  (boolean? (member player (survivors state))))

#; {State Player TileIndex TileIndex -> Boolean}
(define (all-suicide? state player ti1 ti2)
  (for/and ((t (append (all-tiles ti1) (all-tiles ti2))))
    (define state+1 (add-tile state player t))
    ;; neither (infinite? state+1) nor (collided? state+1) yield suicide 
    (and (state? state+1) (suicide? state+1 player))))

#; {State Player TileIndex TileIndex -> Boolean}
(define (all-infinite-suicide? state player ti1 ti2)
  (for/and ((t (append (all-tiles ti1) (all-tiles ti2))))
    (define state+1 (add-tile state player t))
    (infinite? state+1)))

(module+ test

  ;; -------------------------------------------------------------------------------------------------
  ;; check the initial placement rule 

  (define (check-initial player #:ti (ti1 0) init (msg "") (expected #f))
    (check-equal? (legal-initial state3 player ti1 0 0 init) expected msg))
  
  (check-initial "green" #:ti 0 `[[1 0] ,(index->port 0) 0 0] "not a given tile")
  ; (check-initial "red" #:ti 0 `[[0 0] ,(index->port 0) 0 0] "already played")
  (check-initial "green" #:ti state3+green-ti `[[,state3+green-ti 0] ,(index->port 0) 4 0] "bad port")
  (check-initial "green" `[[0 0] ,(index->port 0) 0 0] "taken spot")

  (define state3-initial `[[,state3+green-ti 0] ,@state3+green-spot])
  (check-initial "green" #:ti state3+green-ti state3-initial "o" state3+green)

  ;; -------------------------------------------------------------------------------------------------
  ;; check the turn rule
  (define (check-turn state action expected msg #:t1 (t1 #f) #:t2 (t2 #f))
    (match-define [list player [list tile-index _]] action)
    (define action2 (second action))
    (define tile-1-index (or t1 tile-index))
    (define tile-2-index (or t2 tile-index))
    (check-equal? (legal-take-turn state player tile-1-index tile-2-index action2) expected msg))

  (check-turn good-intermediate-state state3-action #false "plain suicide?")
  (check-turn state-suicide state3-action-infinite state-suicide++ "forced suicide")

  (check-turn state3 state3-action-infinite #:t1 3 #false "plain infinite loop")
  (check-turn state3 state3-action-infinite (minus-player state3 "red") "force infinite-loop suicide")
  (check-turn good-intermediate-state #:t1 0 #:t2 0 '["red" [1 0]] #false "not a given tile")

  (check-turn collision-state collision-action collision-state++ "collision")
  
  (match-define [list a1 a2 a3] good-state-actions)
  (check-turn good-intermediate-state a1 #:t2 0 good-intermediate-state+ "+")
  (check-turn good-intermediate-state+ a2 #:t2 0 good-intermediate-state++ "++")
  (check-turn good-intermediate-state++ a3 #:t2 0 good-intermediate-state+++ "+++")
  
  (check-turn state10-pre state10-action #false #:t1 24 #:t2 25 "probable failure from hw9"))
