#lang racket

(require Tsuro/Code/Common/actions)
(require (only-in Tsuro/Code/Common/board state?))
(require (only-in Tsuro/Code/Common/tokens color?))

(define result/c (or/c state?))

(provide
 (contract-out
  [legal-initial   (-> state? color? init-action/c result/c)]
  [legal-take-turn (-> state? color? turn-action/c result/c)]))

;; ---------------------------------------------------------------------------------------------------
(require (except-in Tsuro/Code/Common/board state?))
(require Tsuro/Code/Common/tiles)

;; ---------------------------------------------------------------------------------------------------
;; A player gets handed a board representation and responds with an action.

;; An init-action/c action specifies a tile placement
;; and on which port of the tile the avatar is placed.
;; It is legal if the tile
;; -- is placed at the periphery of the board
;; -- does not neighbor an already placed tile in a cardinal direction
;; -- the player faces an empty square on the board.


(define (legal-initial board player ia)
  (match-define (list (list ti d) p x y) ia)
  ;; ???? needs new board action 
  #false)

;; ---------------------------------------------------------------------------------------------------
;; A  turn-action/c merely specifies a tile index and a rotation.
;; It is legal if placing the tile does not lead to
;; -- the suicide of the player's avatar 
;; -- the infinite looping of any avatar, including the player's
;; -- the collision of two (or more) player avatars on the same spot.

(define (legal-take-turn state player ta)
  (match-define (list ti d) ta)
  (define tile (rotate-tile (tile-index->tile ti) d))
  (define state+1 (add-tile state player tile))
  (and (not (infinite? state+1)) (not (collided? state+1)) (not (suicide? state+1 player)) state+1))

#; {State Player -> Boolean}
;; is player a dead in state? 
(define (suicide? state player)
  (boolean? (member player (survivors state))))