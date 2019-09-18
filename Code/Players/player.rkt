#lang racket

(require Tsuro/Code/Common/player-interface)

;; internals of players
;; internally, the player is game mechanics while the strategy component makes game decisions 

(define strategy/c
  (class/c 
   (initial   (-> color? initial-player-on-tile*/c tile-index? tile-index? tile-index? init-action/c))
   [take-turn (-> color? intermediate*/c tile-index? tile-index? turn-action/c)]))

(define internal%/c
  (class/c
   {init-field [strategy strategy/c]}))

(define internal-player (and/c internal%/c player%/c))
