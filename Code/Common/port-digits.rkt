#lang racket

;; represent ports with digits from 0 ...

(require Tsuro/Code/Common/port-signature) (provide-port-signature)

;; -----------------------------------------------------------------------------
(define PORTS (build-list 8 identity))
(define index->port values)
(define port->index values)
(define <-port <)
(define (90degrees x) (modulo (+ x 2) 8))