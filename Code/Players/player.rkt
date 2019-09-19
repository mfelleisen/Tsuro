#lang racket

(require Tsuro/Code/Common/player-interface)

(define strategy/c
  (class/c 
   (initial
    (->m color? initial-player-on-tile*/c tile-index? tile-index? tile-index? init-action/c))
   [take-turn
    (->m color? intermediate*/c tile-index? tile-index? turn-action/c)]))

(define internal%/c (class/c {init-field [strategy strategy/c]}))

(define internal-player (and/c internal%/c player%/c))

(provide
 (contract-out
  [player% internal-player]))

;; ---------------------------------------------------------------------------------------------------
;; internals of players
;; internally, the player is game mechanics while the strategy component makes game decisions 

(define player%
  (class object%
    (init-field strategy)

    (field (my-name-for-game "blue"))
    (field (other-players '()))

    [define/public (playing-as my-name)
      (set! my-name-for-game my-name)]
    
    [define/public (playing-with others)
      (set! other-players others)]
    
    #; (-> initial-player-on-tile*/c tile-index? tile-index? tile-index? init-action/c)
    (define/public (initial tiles-placed-so-far tile1 tile2 tile3)
      ;; checkable ~~ the placed players are among others; what's my position in the game
      (send strategy initial my-name-for-game tiles-placed-so-far tile1 tile2 tile3))

    #; (-> intermediate*/c tile-index? tile-index? turn-action/c)
    [define/public (take-turn tiles-placed-so-far tile1 tile2)
      ;; optional: update _others_ because some may no longer be with us 
      (send strategy take-turn my-name-for-game tiles-placed-so-far tile1 tile2)]

    #; (-> [listof color?] any)
    [define/public (end-of-game order-of-exist)
      (void)]

    #; (-> [listof string?] any)
    [define/public (end-of-tournament results)
      (void)]
    
    (super-new)))


(define first-strategy
  (class object%
    
    (define/public (initial my-name-for-game tiles-placed-so-far tile1 tile2 tile3)
      (define board (initialize tiles-placed-so-far))
      (define spot  (find-first-free-spot board))
      (cons my-name-for-game spot))

    (define (take-turn my-name-for-game tiles-placed-so-far tile1 tile2)
      (list tile1 0))

    (super-new)))