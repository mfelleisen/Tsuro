#lang racket

(require Tsuro/Code/Players/strategies)
(provide-strategy third-s%)

(define A (index->port 0))
(define (default-tile tiles) `[,(second tiles) 0])
(define third-s% (base-strategy% counter-clockwise (ports-counterclockwise A) backwards default-tile))

(module+ test
  (require (submod ".."))
  (define strategy (new third-s%))
  (define me "red")

  (check-equal? (send strategy initial (initialize '[]) 1 2 3) `[(3 0) ,(index->port 0) 0 1])
  (check-equal? (send strategy take-turn me state3 1 2) (list 2 0))
  (check-equal? (send strategy take-turn me state3 4 34) (list 34 0)))