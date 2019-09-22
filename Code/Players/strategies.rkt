#lang racket

(provide
 strategy/c

 (contract-out
  [first-strategy% strategy/c]))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/player-interface)

(module+ test
  (require (submod ".."))
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
      (define spot  (find-first-free-spot board))
      (cons (list tile3 0) spot))

    ;; use the first tile, don't rotate 
    (define/public (take-turn tiles-placed-so-far tile1 tile2)
      (list tile1 0))

    (super-new)))

(module+ test
  (define-syntax-rule (checks init0 spot1 (color p x y) ...)
    (let*-values ([(strategy) (new first-strategy%)]
                  [(init spot) (values init0 spot1)]
                  [(init spot)
                   (let ([c (~a 'color)])
                     (check-equal? (send strategy initial init 1 2 3) (cons `(3 0) spot) c)
                     (values (cons (list* tile-00 c spot) init) (list (index->port p) x y)))]
                  ...)
      (check-equal? (send strategy initial init 1 2 3) (cons `(3 0) spot) "last one")))

  (define port-red (index->port 2))

  (checks '() `(,port-red 0 0) (red 2 2 0) (black 2 4 0) (blue 2 6 0) (white 2 8 0) (green 0 9 1))

  (check-equal? (send (new first-strategy%) take-turn '[] 1 2) (list 1 0)))