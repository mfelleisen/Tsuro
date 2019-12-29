#lang racket

;; represent ports with digits from 0 ...

(require Tsuro/Code/Common/port-signature) (provide-port-signature)

(require SwDev/Lib/pattern-matching)
(require SwDev/Lib/should-be-racket)

(module+ test
  (require rackunit))

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
  (_ p) #'(? (λ (s) (and (string? s) (= (string-length s) 1) (string->port s))) p))

(define (port->jsexpr p)
  (string (integer->char (+ p charA))))

(define (jsexpr->port pj)
  (and* (string? pj) (= (string-length pj) 1) (string->port pj)))

(define (string->port pj)
  (define char  (string-ref pj 0))
  (define where (- (char->integer char) charA))
  (and (port? where) where))

(define charA (char->integer #\A))

(module+ test
  (check-equal? (jsexpr->port (port->jsexpr 0)) 0)
  
  (check-equal? (jsexpr->port "A") 0)
  (check-equal? (jsexpr->port "X") #false))
                   
#; {[[Listof Port] -> [Listof Port]] Port -> [Listof Port]}
(define ((port-traversal-starting-at direction) p)
  (let loop ([my-ports (direction PORTS)][post '()])
    (cond
      [(equal? (first my-ports) p) (append my-ports (reverse post))]
      [else (loop (rest my-ports) (cons (first my-ports) post))])))

(define ports-clockwise (port-traversal-starting-at values))
(define ports-counterclockwise (port-traversal-starting-at reverse))

(module+ test
  (define ports-clockwise PORTS)
  (define ports-counterclockwise (cons (first PORTS) (reverse (rest PORTS))))

  (check-equal? ((port-traversal-starting-at values) 0) ports-clockwise)
  (check-equal? ((port-traversal-starting-at reverse) 0) ports-counterclockwise))
