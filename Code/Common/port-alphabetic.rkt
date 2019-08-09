#lang racket

;; represents ports as letters A ...

(require Tsuro/Code/Common/port-signature) (provide-port-signature)

(define PORTS (build-list 8 (λ (i) (integer->char (+ 65 i)))))
(define (index->port i) (list-ref PORTS i))
(define (port->index p) (- 8 (length (memq p PORTS))))
(define (<-port p q) (> (length (memq p PORTS)) (length (memq q PORTS))))

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