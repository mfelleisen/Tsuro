#lang racket

(require (only-in Tsuro/Code/Players/strategies strategy/c))

(provide (contract-out [first-strategy% strategy/c]))

;; --------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/player-interface)
(require (except-in Tsuro/Code/Players/strategies strategy/c))

(define (default-tile tiles) `[,(first tiles) 0])
(define first-strategy% (base-strategy% clockwise ports-clockwise forwards default-tile))


(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test))
  (require rackunit)
  
  (define strategy (new first-strategy%))
  (define me "red")
  (check-equal? (send strategy take-turn me state3 1 2) (list 1 0))
  (check-equal? (send strategy take-turn me state3 4 34) (list 4 0)))