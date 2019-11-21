#lang racket

(require (only-in Tsuro/Code/Common/grid location/c))
(require (only-in Tsuro/Code/Common/port port?))
(require (only-in Tsuro/Code/Common/player-interface tile-index? turn-action/c))

(define tiles/c (listof tile-index?))
(define locs/c  (listof location/c))
(define ports/c [listof port?])

(provide
 #; (provide-strategy name:id)
 ;; import standard test facilities: player-interface, (sumod board test-cases), port and this module;
 ;; provide the strategy called name both as name and as first-strategy (for dynamic loading)
 provide-strategy
 
 strategy/c
 
 (contract-out
  [forwards               (-> tiles/c tiles/c)]
  [backwards              (-> tiles/c tiles/c)]
  [base-strategy% (-> locs/c ports/c (-> tiles/c tiles/c) (-> tiles/c turn-action/c) strategy/c)]))

;; ---------------------------------------------------------------------------------------------------
(require (except-in Tsuro/Code/Common/player-interface tile-index? turn-action/c))
(require Tsuro/Code/Common/rules)
(require (except-in Tsuro/Code/Common/port port?))

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test-cases))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define strategy/c
  (class/c
   ;; the initial step does not need to know which player the initial placement is chosen for 
   (initial   (->m initial-state? tile-index? tile-index? tile-index? init-action/c))
   ;; the take-tuen must know; it can find out who else is playing via survivors (if needed)
   [take-turn (->m avatar? state? tile-index? tile-index? turn-action/c)]))

(define (base-strategy% initial-spots-to-be-searched ports-to-be-searched tiles-usage default-action)
  (class object%
    (super-new)
    
    ;; use first free spot starting from (0,0) [exclusive] where a tile can be placed at the periphery
    ;; use first free port facing an empty tile; search clockwise in both cases 
    (define/public (initial board tile1 tile2 tile3)
      (define spot (first (find-free-spots board initial-spots-to-be-searched ports-to-be-searched)))
      (cons (list tile3 0) spot))

    ;; use the first rotated tile that is legal or tile1 at 0 if none are legal 
    (define/public (take-turn me state tile1 tile2)
      (define tiles (list tile1 tile2))
      (define candidate 
        (for*/first ([index  (in-list (tiles-usage tiles))]
                     [degree DEGREES]
                     [action (in-value (list index degree))]
                     #:when (legal-take-turn state me tile1 tile2 action))
          action))
      (or candidate (default-action tiles)))))

(define (forwards tiles) tiles)
(define (backwards tiles) (reverse tiles))

; (define ports-clockwise PORTS)
; (define ports-counterclockwise (cons (first PORTS) (reverse (rest PORTS))))

;; ---------------------------------------------------------------------------------------------------
(define-syntax (provide-strategy stx)
  (define new-stx
    '(require
       Tsuro/Code/Common/player-interface
       (submod Tsuro/Code/Common/board test-cases)
       Tsuro/Code/Common/port
       Tsuro/Code/Players/strategies
       rackunit))
  (syntax-case stx []
    [(_ name)
     #`(begin #,(datum->syntax stx new-stx)
              (provide (contract-out [name strategy/c][rename name first-strategy% strategy/c])))]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define strategy (new (base-strategy% clockwise PORTS (λ _ '[]) (λ (tiles) (error 't "abstract")))))
  (define me "red")
  
  (check-equal? (send strategy initial (initialize '()) 1 2 3) `[[3 0] ,(index->port 2) 1 0])
  (check-exn exn:fail? (λ () (send strategy take-turn me state3 1 2))))