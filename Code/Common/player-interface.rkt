#lang racket

;; on the Administrative side, the player has contact with an administrator and many referees

(provide
 ;; a contract that describes the player class's interface to the administrator 
 player%/c

 init-action/c

 turn-action/c

 (all-from-out Tsuro/Code/Common/board)
 (all-from-out Tsuro/Code/Common/tiles)
 (all-from-out Tsuro/Code/Common/tokens))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/board)
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/tokens)

;; ---------------------------------------------------------------------------------------------------

(define init-action/c player-on-tile/c)

(define turn-action/c (list/c tile-index? degree?)) 

(define referee-player%/c
  (class/c
   [playing-as   (->m color? any)]
   [playing-with (->m [listof color?] any)]
   (initial      (->m initial-player-on-tile*/c tile-index? tile-index? tile-index? init-action/c))
   [take-turn    (->m intermediate*/c tile-index? tile-index? turn-action/c)]
   [end-of-game  (->m [listof color?] any)]))

(define admin-player%/c
  (class/c
   [end-of-tournament (->m [listof string?] any)]))

(define player%/c (and/c referee-player%/c admin-player%/c))

;; protocol:
;; -- playing-as is called first and once per game
;; -- playing-with is called at the beginning of a game, after playing-as
;; -- initial is called once per game, in third place
;; -- take-turn is called repeatedly until this player is the last or all avatars dropped off
;; -- end-of-game is called at the end of the game,

;; the protocol is abondoned when this player raised an exception or whas terminated 