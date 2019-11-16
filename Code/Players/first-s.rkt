#lang racket

(require (only-in Tsuro/Code/Players/strategies strategy/c))

(provide (contract-out [first-strategy% strategy/c]))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/player-interface)
(require (except-in Tsuro/Code/Players/strategies strategy/c))

(define first-strategy%
  (class (base-strategy% clockwise (λ _ 'dummy) (λ _ 'dummy))
    (super-new)
    ;; use the first tile, don't rotate 
    (define/override (take-turn me board tile1 tile2)
      (list tile1 0))))

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test))
  (require rackunit)
  
  (define strategy (new first-strategy%))
  (define me "red")
  (check-equal? (send strategy take-turn me state3 1 2) (list 1 0)))
