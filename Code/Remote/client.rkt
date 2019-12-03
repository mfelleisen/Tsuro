#lang racket

;; a client that signs up one player with a server at a given IP address
;; and port, and then participates in a distributed a tournament 

(require (only-in Tsuro/Code/Common/player-interface player/c))
(require (only-in SwDev/Testing/make-client port/c))

(provide
 (contract-out
  [client 
   #; (client players ip port# wait?)
   ;; runs a client that connects all players to a server at ip on port#
   ;; waits for all of them if wait?
   (->* ([listof [list/c string? player/c]]) (string? port/c boolean?) any)]))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Remote/administrator)
(require (except-in SwDev/Testing/make-client port/c))
(require SwDev/Debugging/spy)

(require Tsuro/Code/Lib/xsend)

(module+ test
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define LOCAL "127.0.0.1")
(define TIME-PER-CLIENT 30)

(define (client players (ip LOCAL) (port 45678) (wait? #false))
  (define (co) (connect-to-server-as-receiver ip port))
  (define player-threads (make-players wait? players co))
  (when wait?
    (wait-for-all player-threads)
    (displayln "all done")))

#; {type ChanneledThreads = [Listof [List Channel String  Thread]]}

#; {Boolean [Listof Player] [-> (values InputPort OutputPort)] -> ChanneledThreads}
(define (make-players wait? players connector)
  (define done (make-channel))
  (for/list ((p players))
    (match-define [list name behavior] p)
    (define-values (receiver _) (connector))
    (define admin (make-remote-administrator receiver))
    (list done
          name
          (thread
           (λ ()
             (define r (parameterize ([time-out-limit TIME-PER-CLIENT]) (xcall admin behavior)))
             (if wait? (channel-put done (list name r)) (void)))))))

#; {ChanneledThreads -> Void}
;; display the results 
(define (wait-for-all player-threads)
  (when (cons? player-threads)
    (define removes-itself
      (for/list ((dp player-threads))
        (match-define [list done name th] dp)
        (handle-evt done (λ (r) (wait-for-all (remq dp player-threads))))))
    (apply sync removes-itself)))
