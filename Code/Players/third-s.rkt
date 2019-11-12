#lang racket

(require (only-in Tsuro/Code/Players/strategies strategy/c))

(provide (contract-out [first-strategy% strategy/c]))

;; --------------------------------------------------------------------------------------------------
(require (except-in Tsuro/Code/Players/strategies strategy/c))
(require Tsuro/Code/Common/rules)
(require Tsuro/Code/Common/tiles)

(require SwDev/Debugging/spy)

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define first-strategy%
  (class base-strategy%
    (super-new)

    ;; use the first rotated tile that is legal or tile1 at 0 if none are legal 
    (define/override (take-turn me state tile1 tile2)
      (define candidate 
        (for*/first ([index  (list tile2 tile1)]
                     [degree DEGREES]
                     [action (in-value (list index degree))]
                     #:when (legal-take-turn state me tile1 tile2 action))
          action))
      (or candidate (list tile2 0)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define strategy (new first-strategy%))
  (define me "red")
  (check-equal? (send strategy take-turn me state3 1 2) (list 2 0))
  (check-equal? (send strategy take-turn me state3 4 34) (list 34 0)))