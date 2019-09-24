#lang racket

(provide
 strategy/c

 (contract-out
  [first-strategy% strategy/c]))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/player-interface)
(require Tsuro/Code/Common/grid)
(require Tsuro/Code/Common/port)

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test))
  (require Tsuro/Code/Common/port)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define strategy/c
  (class/c 
   (initial
    (->m initial-player-on-tile*/c tile-index? tile-index? tile-index? init-action/c))
   [take-turn
    (->m intermediate*/c tile-index? tile-index? turn-action/c)]))

(define first-strategy%
  (class object%

    ;; find first free spot starting from (0,0) where a tile can be placed at the periphery
    ;; then find the first free port facing an empty tile 
    (define/public (initial tiles-placed-so-far tile1 tile2 tile3)
      (define board (initialize tiles-placed-so-far))
      (define spot  (find-first-free-spot board clock-wise pick-port))
      (cons (list tile3 0) spot))

    ;; use the first tile, don't rotate 
    (define/public (take-turn tiles-placed-so-far tile1 tile2)
      (list tile1 0))

    (super-new)))

#; {Location -> Location}
(define (clock-wise loc)
  (define-values (x y) (apply values loc))
  (cond
    [(and (< (+ x 1) SIZE) (= y 0))          (list (+ x 1) 0)]
    [(and (= (+ x 1) SIZE) (< (+ y 1) SIZE)) (list x (+ y 1))]
    [(and (> x 0) (= (+ y 1) SIZE))          (list (- x 1) y)]
    [(and (= x 0) (>  y 0))                  (list x (- y 1))]
    [else (error 'find-first-free-spot "out of free periphery positions")]))

(define (counter-clock-wise loc)
  (define-values (x y) (apply values loc))
  (cond
    [(and (= x 0) (< (+ y 1) SIZE))          (list x (+ y 1))]
    [(and (< (+ x 1) SIZE) (= (+ y 1) SIZE)) (list (+ x 1) y)]
    [(and (= (+ x 1) SIZE) (> y 0))          (list x (- y 1))]
    [(and (> x 0) (= y 0))                   (list (- x 1) y)]
    [else (error 'find-first-free-spot "out of free periphery positions")]))

#; {Location -> Port}
;; ASSUME no neighboring tile 
(define (pick-port loc)
  (define n (apply neighbor-locations loc))
  (for/first ((p (in-list PORTS)) #:when (apply port-facing-inward? p loc)) p))

(module+ test
  (define port-red (index->port 2))
  (define two (index->port 2))
  (define strategy (new first-strategy%))
  (checks-initialization-sequence
   (λ (color port x y spot init state) (send strategy initial init 1 2 3))
   (λ (color port x y spot init state) (cons `(3 0) spot))
   `(,port-red 0 0)
   `[("red" ,two 2 0) ("black" ,two 4 0) ("blue" ,two 6 0) ("white" ,two 8 0) ("green" 0 9 1)])

  (check-equal? (send (new first-strategy%) take-turn '[] 1 2) (list 1 0)))