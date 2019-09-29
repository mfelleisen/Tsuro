#lang racket

(provide
 PORT#
 provide-port-signature)

(define PORT# 8)

(define-syntax (provide-port-signature stx)
  (datum->syntax stx 
                 '(provide
                   ;; type Port 
     
                   ;; [Listof Portlabels]
                   PORTS

                   port-pat

                   #; {Any -> Boolean : member of PORTS}
                   port?

                   #; {Nat -> Port}
                   index->port

                   #; {Port -> Nat}
                   port->index

                   #; {Port -> (U 'NORTH 'EAST 'SOUTH 'WEST)}
                   port->direction 

                   #; {Port Port -> Boolean}
                   <-port

                   #; {Port -> Port}
                   ;; rotate a port by 90 degrees
                   90degrees

                   #; {Port -> Port}
                   facing-port

                   #; {Port -> JSexpr}
                   port->jsexpr

                   #; {JSexrpr -> (U False Port)}
                   ;; satisfies port-pat
                   jsexpr->port)))

  