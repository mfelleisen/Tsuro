#lang racket

;; a tournament server that signs up players over TCP and runs a tournament 

(require (only-in Tsuro/Code/Admin/administrator results/c))
(require (only-in Tsuro/Code/Common/player-interface player/c))

(define port/c (and/c natural-number/c (</c 60000) (>/c 10000)))
(define player#/c natural-number/c)
(define secs/c natural-number/c)
(define named-results/c [list/c [listof [listof string?]] [listof string?]])

(provide
 (contract-out
  [server
   #; (server player# wait-for-sec port#)
   ;; returns the list of players, sorted in descending order of age, plus rankings from
   ;; runsning an administrator on the N players that connected on port# in
   ;; wait-for-msec seconds or N >= player# as soon as that many signed up 
   (-> player#/c secs/c port/c (list/c (listof player/c) results/c))]))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Remote/player)
(require (except-in Tsuro/Code/Admin/administrator results/c))

(require SwDev/Testing/communication)
(require SwDev/Debugging/spy)

(module+ test
  (require (submod ".."))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define LOCAL     "127.0.0.1")
(define MAX-TCP   30)
(define REOPEN    #t)
(define MIN-ERROR "server did not sign up any players")
(define DEFAULT-RESULT '[[] [[][]]]) 

(define test-run?  (make-parameter #false))

(define (server min-players time-limit port)
  (define send-players (make-channel))
  (define time-s-up    (make-channel))
  (define custodian    (make-custodian))
  (parameterize ([current-custodian custodian])
    (thread (sign-up-players min-players time-limit port send-players time-s-up)))
  (define players (receive-players time-limit send-players time-s-up))
  (begin0
    (cond
      [(empty? players) (displayln MIN-ERROR (current-error-port)) DEFAULT-RESULT]
      [(test-run?) => (λ (result) (channel-put result players) DEFAULT-RESULT)]
      [else (run-administrator players)])
    (custodian-shutdown-all custodian)))

#; {N Channel Channel -> [Listof (U Players N)]} 
(define (receive-players time-limit send-players time-s-up)
  (reverse
   (cond
     [(sync/timeout time-limit send-players) => values]
     [else
      (channel-put time-s-up 'give-me-the-players)
      (channel-get send-players)])))

#;{N Positive Port-Number Channel Channel -> (-> Void)}
;; communicate the players the signed up in reverse-chronological order on send-players
(define ((sign-up-players min-players time-limit port send-players time-s-up))
  (define listener (tcp-listen port MAX-TCP REOPEN))
  (let collect-up-to-min-players ((players '()))
    (sync
     (handle-evt time-s-up (λ (_false) (channel-put send-players players)))
     (handle-evt listener 
                 (λ (listener)
                   (define players++ (add-player players listener))
                   (if (>= (length players++) min-players)
                       (channel-put send-players players++)
                       (collect-up-to-min-players players++)))))))

#; (Listener [Listof Player] -> [Listof Player])
(define (add-player players listener)
  (with-handlers ((exn:fail:network? (lambda (xn) (log-error (~a (exn-message xn))) players)))
    (define-values (in out) (tcp-accept listener))
    (define next (if (test-run?) (add1 (length players)) (new (make-remote-player in out))))
    (cons next players)))

#; ([Listof ExternalPlayer] (U False [Listof String]) -> [List [Listof ExternalPlayer] Results])
(define (run-administrator players)
  (define result (administrator players))
  [list players result])

;; ---------------------------------------------------------------------------------------------------
(module+ test

  #; { N Port-Number (U False n:N) -> (U False [Listof 0]: (=/c (length) n))}
  #; (run-server-test m p k)
  ;; runs the server on port p, waitig for m players, but receiving k
  (define (run-server-test min-players port k)
    [define custodian (make-custodian)]
    [define result    (make-channel)]
    [define err-out   (open-output-string)]
    (parameterize ([test-run?          result]
                   [current-custodian  custodian]
                   [current-error-port err-out])
      (define th (thread (λ () (server min-players 3 port))))
      (sleep 1)
      (if (boolean? k) (sync th) (for ([i k]) (define-values (- +) (tcp-connect LOCAL port)) 0)))
    (begin0
      (if k (channel-get result) (get-output-string err-out))
      (custodian-shutdown-all custodian)))

  (check-equal? (run-server-test 10 45678 #f) (string-append MIN-ERROR "\n") "no sign ups")
  (check-equal? (run-server-test 10 45679 10) (build-list 10 add1) "sign up enough players")
  (check-equal? (run-server-test 10 45679  9) (build-list  9 add1) "sign up too few players"))
