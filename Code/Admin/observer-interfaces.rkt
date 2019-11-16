#lang racket

(provide
 turn?
 game-observer/c)

(require Tsuro/Code/Common/actions)
(require Tsuro/Code/Common/board)
(require Tsuro/Code/Common/tokens)
(require Tsuro/Code/Common/tiles)

(define turn? [list/c [list/c avatar? natural-number/c turn-action/c] tile-index? tile-index?])
(define game-observer/c
  ;; accept information about the current state for a regular turn,
  ;; the action requested by the active avatar, and
  ;; the next state or #false if it is illegal
  ;; when a player cheats, the observer is not called 
  (-> (-> state? turn? (or/c false state?) any)))

(define tournament-observer/c
  (-> (-> (listof player/c) (listof player/c) any)))