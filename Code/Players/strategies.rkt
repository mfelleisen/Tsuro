#lang racket

(provide strategy/c)

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/player-interface)

;; ---------------------------------------------------------------------------------------------------
(define strategy/c
  (class/c
   ;; the initial step does not need to know which player the initial placement is chosen for 
   (initial   (->m initial-state? tile-index? tile-index? tile-index? init-action/c))
   ;; the take-tuen must know; it can find out who else is playing via survivors (if needed)
   [take-turn (->m avatar? state? tile-index? tile-index? turn-action/c)]))