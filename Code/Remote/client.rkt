#lang racket

;; a client that signs up one player with a server at a given IP address
;; and port, and then participates in a distributed a tournament 

(require (only-in Tsuro/Code/Common/player-interface player/c))
(require (only-in SwDev/Testing/make-client port/c))

(provide
 (contract-out
  [client 
   #; (client players ip port#)
   ;; runs a client that connects all players to a server at ip on port#
   (->* ([listof [list/c string? player/c]]) (string? port/c) any)]))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Remote/administrator)
(require (except-in SwDev/Testing/make-client port/c))
(require SwDev/Debugging/spy)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define LOCAL     "127.0.0.1")

(define (client players (ip LOCAL) (port 45678))
  (struct result [name value] #:transparent)
  (define done (make-channel))
  (define player-threads
    (for/list ((p players))
      (match-define [list name behavior] p)
      (define-values (receiver _) (connect-to-server-as-receiver ip port))
      (define admin (make-remote-administrator receiver))
      (thread (λ () (channel-put done (result name (admin behavior)))))))
  (sync (handle-evt done displayln)))