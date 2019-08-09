#lang racket

(provide provide-port-signature)

(define-syntax (provide-port-signature stx)
  (datum->syntax stx 
   '(provide
      ;; [Listof Portlabels]
      PORTS

      #; {Nat -> Port}
      index->port

      #; {Port -> Nat}
      port->index

      #; {Port Port -> Boolean}
      <-port

      #; {Port -> Port}
      ;; rotate a port by 90 degrees
      90degrees

      #; {Port -> Port}
      facing-port)))