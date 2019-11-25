#lang racket

;; on the Administrative side, the player has contact with
;; -- an tournament administrator
;; -- many referees
;; so there are two interfaces and a combined one for the implementor of the component

(require Tsuro/Code/Common/player-interface)

(provide
 ;; a contract that describes the player class's interface to the administrator 
 (contract-out
  (make-remote-player (-> input-port? output-port? player/c))))

(require (submod Tsuro/Code/Common/actions json))
(require (submod Tsuro/Code/Common/tiles json))
(require (submod Tsuro/Code/Common/tokens json))
(require (submod Tsuro/Code/Common/board json))

(require Tsuro/Code/Common/port)

(require Tsuro/Code/Players/player)
(require SwDev/Testing/communication)
(require (for-syntax syntax/parse))
; (require (only-in (for-syntax racket) ~a))

(module+ test
  (require (submod ".."))
  (require json)
  (require rackunit))

(define (make-remote-player in out)
  (new remote-player% [in in] [out out]))

#|
call        arguments                         result
playing-as  ["playing-as", [Color]]           "void"
others      ["others", [Color, ..., Color]]   "void"
initial     ["initial", initial]              action
take-turn   ["take-turn", intermediate]       tile-pat
end-of-tournament  ["end-of-tournament", [Boolean]] "void"
|#

(define remote-player%
  (class object% [init-field in out]
    (super-new)

    (define-syntax (define/remote stx)
      (syntax-parse stx
        [(_ (m (~optional (~seq #:name n:string) #:defaults ([n #'(~a 'm)])) (x:id ->to) ... <-from))
         #'(define/public (m x ...)
             (send-message `[,n [,(->to x) ...]] out)
             (define msg (read-message in))
             (define dec (<-from msg))
             (unless dec (error 'm "wrong return value received: ~e" msg))
             dec)]))

    (define/remote (playing-as (me avatar->jsexpr) jsexpr->void))
    (define/remote (playing-with #:name "others" (others (curry map avatar->jsexpr)) jsexpr->void))
    (define/remote (initial (simple intermediate*->jsexpr)
                            (t1 tile-index->jsexpr)
                            (t2 tile-index->jsexpr)
                            (t3 tile-index->jsexpr)
                            jsexpr->init-action))
    (define/remote (take-turn (state intermediate*->jsexpr)
                              (t1 tile-index->jsexpr)
                              (t2 tile-index->jsexpr)
                              jsexpr->turn-action))
    (define/remote (end-of-tournament (result values) jsexpr->void))))

(define (jsexpr->void j)
  (match j
    ["void" (void)]))

(define tile-index->jsexpr values)

;; ---------------------------------------------------------------------------------------------------
(module+ test 
  (define (ci) (open-input-string "\"void\""))
  (define (bd) (open-input-string "\"not-void\""))
  (define (rp ci) (make-remote-player (ci) (current-output-port)))
  (define-syntax-rule (mp ci method args ...)
    (let* ([result (gensym)]
           [output (with-output-to-string (λ () (set! result (send (rp ci) method args ...))))])
      (list (read-message (open-input-string output)) result)))
  
  (check-equal? (mp ci playing-as "red") [list `["playing-as" ["red"]] (void)])
  (check-equal? (mp ci playing-with `["red" "blue"]) [list `["others" [["red" "blue"]]] (void)])  
  (check-equal? (mp ci end-of-tournament #t) (list `["end-of-tournament" [,true]] (void)))

  (check-exn exn? (λ () (mp bd playing-as "red")))
  (check-exn exn? (λ () (mp bd playing-with `["red"])))
  (check-exn exn? (λ () (mp bd end-of-tournament #false)))

  (define init0  `[[0 90] ,(index->port 4) 0 0])
  (define (i0) (open-input-string (jsexpr->string (init-action->jsexpr init0))))
  (check-equal? (mp i0 initial '[] 0 1 2) (list `["initial" [[] 0 1 2]] init0))

  (define a0 `[,(jsexpr->tile '[0 90]) "red" ,(index->port 2) 0 0])
  (define j0 (intermediate*->jsexpr `[,a0]))
  (check-equal? (mp i0 initial (list a0) 0 1 2) (list `["initial" [,j0 0 1 2]] init0))

  ;; TODO:
  ;; -- exn JSON for initial 
  ;; -- take-turn 
  
  )
  