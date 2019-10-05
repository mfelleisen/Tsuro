;; TDDO: resolve player alive: rules.rkt might be wrong; push to referee with dependent contracts? 

#lang racket

;; a Tsuro rule checker 

(require Tsuro/Code/Common/actions)
(require (only-in Tsuro/Code/Common/board state?))
(require (only-in Tsuro/Code/Common/tokens color?))

(define (ok s) (or/c #false s))

(provide
 (contract-out
  [legal-initial
   (-> initial-state? color? tile-index? tile-index? tile-index? init-action/c (ok initial-state?))]
  [legal-take-turn
   (-> state? color? tile-index? tile-index? turn-action/c (ok state?))]))

;; ---------------------------------------------------------------------------------------------------
(require (except-in Tsuro/Code/Common/board state?))
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/port)
(require SwDev/Debugging/spy)

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test))
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
    [(not (or (equal? ti given-ti1) (equal? ti given-ti2))) #false]
    [(set-member? (survivors state0) player) #false]
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
       [(infinite? state+1) #false]
       [(suicide? state+1 player)
        (if (all-suicide? state player given-ti1 given-ti2) state+1 #false)]
       [else state+1])]))

#; {State Player -> Boolean}
;; is player a dead in state? 
(define (suicide? state player)
  (boolean? (member player (survivors state))))

#;{State Player TileIndex TileIndex -> Boolean}
(define (all-suicide? state player ti1 ti2)
  (for/and ((t (append (all-tiles ti1) (all-tiles ti2))))
    (define state+1 (add-tile state player t))
    (suicide? state+1 player)))

(module+ test

  ;; -------------------------------------------------------------------------------------------------
  ;; check the initial placement rule 

  (define (check-initial player #:ti (ti1 0) init (msg "") (expected #f))
    (check-equal? (legal-initial state3 player ti1 0 0 init) expected msg))
  
  (check-initial "green" #:ti 0 `[[1 0] ,(index->port 0) 0 0] "not a given tile")
  (check-initial "red" #:ti 0 `[[0 0] ,(index->port 0) 0 0] "already played")
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
  (check-turn state3 state3-action-infinite #false "infinite loop")
  (check-turn good-intermediate-state #:t1 0 #:t2 0 '["red" [1 0]] #false "not a given tile")  
  
  (match-define [list a1 a2 a3] good-state-actions)
  (check-turn good-intermediate-state #:t2 0 a1 good-intermediate-state+ "+")
  (check-turn good-intermediate-state+ #:t2 0 a2 good-intermediate-state++ "++")
  (check-turn good-intermediate-state++ #:t2 0 a3 good-intermediate-state+++ "+++"))
