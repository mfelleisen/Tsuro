#lang racket

;; represent ports with digits from 0 ...

(require Tsuro/Code/Common/port-signature) (provide-port-signature)

(require Tsuro/Code/Common/pattern-matching)

;; -----------------------------------------------------------------------------
(define PORTS (build-list PORT# identity))
(define (port? x) (< -1 x PORT#))
(define index->port values)
(define port->index values)
(define <-port <)
(define (90degrees x) (modulo (+ x 2) 8))

(define (port->direction p)
  (case p
    [(0 1) 'NORTH]
    [(2 3) 'EAST]
    [(4 5) 'SOUTH]
    [(6 7) 'WEST]))

(define (facing-port p)
  (case p
    [(0) 5]
    [(1) 4]
    [(2) 7]
    [(3) 6]
    [(4) 1]
    [(5) 0]
    [(6) 3]
    [(7) 2]))

(def/mp port-pat
  (_ p) #'(? (λ (s) (and (integer? s) (<= 0 s PORT#))) p))

(define (port->jsexpr p)
  p)

(define (jsexpr->port pj)
  pj)