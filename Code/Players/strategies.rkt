#lang racket

(provide
 strategy/c
 (contract-out [base-strategy% strategy/c]))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/player-interface)

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test))
  (require Tsuro/Code/Common/port)
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define strategy/c
  (class/c
   ;; the initial step does not need to know which player the initial placement is chosen for 
   (initial   (->m initial-state? tile-index? tile-index? tile-index? init-action/c))
   ;; the take-tuen must know; it can find out who else is playing via survivors (if needed)
   [take-turn (->m avatar? state? tile-index? tile-index? turn-action/c)]))

(define base-strategy%
  (class object%
    (super-new)
    
    ;; use first free spot starting from (0,0) [exclusive] where a tile can be placed at the periphery
    ;; use first free port facing an empty tile; search clockwise in both cases 
    (define/public (initial board tile1 tile2 tile3)
      (define spot (first (find-free-spots board clockwise)))
      (cons (list tile3 0) spot))

    (define/public (take-turn me state tile1 tile2)
      (error 'take-turn "abstract strategy"))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define strategy (new base-strategy%))
  (define me "red")
  
  (check-equal? (send strategy initial (initialize '()) 1 2 3) `[[3 0] ,(index->port 2) 1 0])
  (check-exn exn:fail? (λ () (send strategy take-turn me state3 1 2))))