#lang racket

(provide
 turn?
 game-observer/c

 ages/c
 tournament-observer/c)

(require Tsuro/Code/Common/actions)
(require Tsuro/Code/Common/board)
(require Tsuro/Code/Common/tokens)
(require Tsuro/Code/Common/tiles)
(require (only-in Tsuro/Code/Common/player-interface player/c))

(define turn? [list/c [list/c avatar? natural-number/c turn-action/c] tile-index? tile-index?])
(define game-observer/c
  ;; accept information about the current state for a regular turn,
  ;; the action requested by the active avatar, and
  ;; the next state or #false if it is illegal
  ;; when a player cheats, the observer is not called 
  (-> (-> state? turn? (or/c false state?) any)))

(define ages/c natural-number/c)
(define sorted-ages/c (λ (lloap) (define ages (apply append lloap)) (equal? ages (sort ages >))))
(define tournament-observer/c
  ;; set up the canvas arrangement by specifying age in [0,max-age)
  (->i ([max-age natural-number/c])
       (r (->i ([games (and/c (listof (and/c cons? (listof ages/c))) sorted-ages/c)]) (r any/c)))))