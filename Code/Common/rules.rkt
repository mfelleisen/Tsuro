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

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test))
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
  (define tidx (first ia))
  (define spot (rest ia))
  (cond
    [(not (or (equal? ti given-ti1) (equal? ti given-ti2))) #false]
    [(and (not (set-member? (survivors state0) player)) ((dont-use-taken-spot/c state0) spot))
     (define tile (tile-index->tile tidx))
     (define state+1 (place-first-tile state0 player tile spot))
     (and (initial-state? state+1) state+1)]
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
     (if (and (suicide? state+1 player) (all-suicide? state player given-ti1 given-ti2))
         state+1
         (and (not (infinite? state+1))
              (not (suicide? state+1 player))
              state+1))]))

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

  ; (check-false (legal-initial state3 "red" 0 0 0 `[[0 666] ,(index->port 0) 0 0]) "i: not degree")
  (check-false (legal-initial state3 "red" 0 0 0 `[[1 0] ,(index->port 0) 0 0]) "i: not a given")

  (let ()
    (match-define [list player [list tile-index degrees]] state3-action)
    (define action2 (second state3-action))
    (check-false (legal-take-turn good-intermediate-state player tile-index tile-index action2)))

  ; (check-false (legal-take-turn good-intermediate-state "red" 0 0 `[0 666]) "not degree")
  (check-false (legal-take-turn good-intermediate-state "red" 0 0 `[1 0]) "not a given tile")

  (match-define [list player [list tile-index degrees]] (first good-state-actions))
  (define action1 [list tile-index degrees])
  (check-true (state? (legal-take-turn good-intermediate-state player tile-index 0 action1))))
