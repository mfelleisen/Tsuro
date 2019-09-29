#lang racket

;; represents ports as letters A ...

(require Tsuro/Code/Common/port-signature) (provide-port-signature)

(require SwDev/Lib/pattern-matching)
(require SwDev/Lib/should-be-racket)

(module+ test
  (require rackunit))

(define PORTS (build-list PORT# (λ (i) (integer->char (+ 65 i)))))
(define (port? x) (member x PORTS))
(define (index->port i) (list-ref PORTS i))
(define (port->index p) (- 8 (length (memq p PORTS))))
(define (<-port p q) (> (length (memq p PORTS)) (length (memq q PORTS))))

(define (port->direction p)
  (case (port->index p)
    [(0 1) 'NORTH]
    [(2 3) 'EAST]
    [(4 5) 'SOUTH]
    [(6 7) 'WEST]))
                         
(define (90degrees x)
  (define new-ports (append (drop PORTS 2) (take PORTS 2)))
  (define rotation  (map list PORTS new-ports))
  (second (assq x rotation)))

(define (facing-port p)
  (index->port
   (case (port->index p)
     [(0) 5]
     [(1) 4]
     [(2) 7]
     [(3) 6]
     [(4) 1]
     [(5) 0]
     [(6) 3]
     [(7) 2])))

(def/mp port-pat
  (_ p) #'(? (λ (s) (and (string? s) (= (string-length s) 1) (port? (string-ref s 0)))) p))

(define (port->jsexpr p)
  (string p))

(define (jsexpr->port pj)
  (and* (string? pj) (= (string-length pj) 1) (string-ref pj 0) => (λ (p) (and (port? p) p))))

(module+ test

  (check-equal? (jsexpr->port (port->jsexpr #\A)) #\A)
  
  (check-equal? (jsexpr->port "A") #\A)
  (check-equal? (jsexpr->port "X") #false))
