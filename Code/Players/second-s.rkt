#lang racket

(require Tsuro/Code/Players/strategies)
(provide-strategy second-s%)

(define (default-tile tiles) `[,(first tiles) 0])
(define second-s% (base-strategy% clockwise ports-clockwise forwards default-tile))

(module+ test
  (require (submod ".."))
  (define strategy (new second-s%))
  (define me "red")
  (check-equal? (send strategy take-turn me state3 1 2) (list 1 0))
  (check-equal? (send strategy take-turn me state3 4 34) (list 4 0)))