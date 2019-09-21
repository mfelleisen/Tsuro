#lang racket

(provide
 strategy/c

 (contract-out
  [first-strategy strategy/c]))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/player-interface)

;; ---------------------------------------------------------------------------------------------------
(define strategy/c
  (class/c 
   (initial
    (->m color? initial-player-on-tile*/c tile-index? tile-index? tile-index? init-action/c))
   [take-turn
    (->m color? intermediate*/c tile-index? tile-index? turn-action/c)]))

(define first-strategy
  (class object%

    ;; find first free spot starting from (0,0) where a tile can be placed at the periphery
    ;; then find the first free port facing an empty tile 
    (define/public (initial my-name-for-game tiles-placed-so-far tile1 tile2 tile3)
      (define board (initialize tiles-placed-so-far))
      (define spot  (find-first-free-spot board))
      (cons my-name-for-game spot))

    ;; use the first tile, don't rotate 
    (define (take-turn my-name-for-game tiles-placed-so-far tile1 tile2)
      (list tile1 0))

    (super-new)))