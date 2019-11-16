#lang racket

(require Tsuro/Code/Players/strategies)
(provide-strategy first-s%)

(define first-s%
  (class (base-strategy% clockwise ports-clockwise (λ _ 'dummy) (λ _ 'dummy))
    (super-new)
    ;; use the first tile, don't rotate 
    (define/override (take-turn me board tile1 tile2)
      (list tile1 0))))

(module+ test
  (require (submod ".."))
  (define strategy (new first-s%))
  (define me "red")
  (check-equal? (send strategy take-turn me state3 1 2) (list 1 0)))
