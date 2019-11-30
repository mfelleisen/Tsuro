#lang racket

;; this remote player implements the same interface as the player but conveys its arguments
;; to the given TCP out stream and receives the results on the TCP in stream

(require Tsuro/Code/Common/player-interface)

(provide
 ;; a contract that describes the player class's interface to the administrator 
 (contract-out
  (make-remote-player (-> input-port? output-port? player/c))))

;                                                                                      
;       ;                                  ;                                           
;       ;                                  ;                          ;                
;       ;                                  ;                                           
;    ;;;;   ;;;   ;;;;    ;;;   ; ;;    ;;;;   ;;;   ; ;;    ;;;    ;;;    ;;;    ;;;  
;   ;; ;;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;; ;;  ;;  ;  ;;  ;  ;;  ;     ;   ;;  ;  ;   ; 
;   ;   ;  ;   ;; ;   ;  ;   ;; ;   ;  ;   ;  ;   ;; ;   ;  ;         ;   ;   ;; ;     
;   ;   ;  ;;;;;; ;   ;  ;;;;;; ;   ;  ;   ;  ;;;;;; ;   ;  ;         ;   ;;;;;;  ;;;  
;   ;   ;  ;      ;   ;  ;      ;   ;  ;   ;  ;      ;   ;  ;         ;   ;          ; 
;   ;; ;;  ;      ;; ;;  ;      ;   ;  ;; ;;  ;      ;   ;  ;;        ;   ;      ;   ; 
;    ;;;;   ;;;;  ;;;;    ;;;;  ;   ;   ;;;;   ;;;;  ;   ;   ;;;;   ;;;;;  ;;;;   ;;;  
;                 ;                                                                    
;                 ;                                                                    
;                 ;                                                                    

(require (submod Tsuro/Code/Common/actions json))
(require (submod Tsuro/Code/Common/tokens json))
(require (submod Tsuro/Code/Common/board json))
(require Tsuro/Code/Common/port)

(require SwDev/Testing/communication)
(require (for-syntax syntax/parse))

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/tiles json))
  (require json)
  (require rackunit))

;                                            
;                                            
;          ;;;                               
;            ;                               
;   ;;;;     ;    ;;;;   ;   ;   ;;;    ;;;; 
;   ;; ;;    ;        ;  ;   ;  ;;  ;   ;;  ;
;   ;   ;    ;        ;   ; ;   ;   ;;  ;    
;   ;   ;    ;     ;;;;   ; ;   ;;;;;;  ;    
;   ;   ;    ;    ;   ;   ; ;   ;       ;    
;   ;; ;;    ;    ;   ;   ;;    ;       ;    
;   ;;;;      ;;   ;;;;    ;     ;;;;   ;    
;   ;                      ;                 
;   ;                     ;                  
;   ;                    ;;                  

(define (make-remote-player in out)
  (new remote-player% [in in] [out out]))

(define remote-player%
  (class object% [init-field in out]
    (super-new)

    (define/private (send-json tag json <-from)
      (send-message json out)
      (define msg (read-message in))
      (with-handlers ([exn:misc:match?
                       (λ (xn)
                         (log-error "~a: wrong return value: ~e" tag msg)
                         (log-error (exn-message xn))
                         (raise xn))])
        (<-from msg)))

    (define-syntax (define/remote stx)
      (syntax-parse stx
        [(_ (m (~optional (~seq #:name n:string) #:defaults ([n #'(~a 'm)])) [->to] <-from))
         #'(define/public (m x) (send-json 'm `[,n ,(map ->to x)] <-from))]
        [(_ (m (~optional (~seq #:name n:string) #:defaults ([n #'(~a 'm)])) ->to ... <-from))
         #:with (x ...) (generate-temporaries #'(->to ...))
         #'(define/public (m x ...) (send-json 'm `[,n [,(->to x) ...]] <-from))]))

    (define/remote (playing-as avatar->jsexpr jsexpr->void))
    (define/remote (playing-with #:name "others" [avatar->jsexpr] jsexpr->void))
    (define/remote (initial simple tile-index tile-index tile-index action))
    (define/remote (take-turn intermediate tile-index tile-index tile-pat))
    (define/remote (end-of-tournament result->jsexpr jsexpr->void))))

(define simple intermediate*->jsexpr)
(define intermediate intermediate*->jsexpr)
(define tile-pat jsexpr->turn-action)
(define tile-index values)
(define action jsexpr->init-action)

(define (jsexpr->void j) (match j ["void" (void)]))
(define result->jsexpr values)

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (define emm? exn:misc:match?)
  (define oos*json (compose open-input-string jsexpr->string))

  (define (rp return-value) (make-remote-player (return-value) (current-output-port)))
  (define-syntax-rule (mp ci method args ...)
    (let* ([result (gensym)]
           [output (with-output-to-string (λ () (set! result (send (rp ci) method args ...))))])
      (list (read-message (open-input-string output)) result)))

  ;; -------------------------------------------------------------------------------------------------
  ;; the simple functions
  (define (ci) (open-input-string "\"void\""))
  (check-equal? (mp ci playing-as "red") [list `["playing-as" ["red"]] (void)])
  (check-equal? (mp ci playing-with `["red" "blue"]) [list `["others" ["red" "blue"]] (void)])  
  (check-equal? (mp ci end-of-tournament #t) (list `["end-of-tournament" [,true]] (void)))

  (define (bd) (open-input-string "\"not-void\""))
  (check-exn emm? (λ () (mp bd playing-as "red")))
  (check-exn emm? (λ () (mp bd playing-with `["red"])))
  (check-exn emm? (λ () (mp bd end-of-tournament #false)))

  ;; -------------------------------------------------------------------------------------------------
  ;; initialization 

  (define tile0 `[0 90])
  (define jile0 (jsexpr->tile '[0 90]))
  (define two (index->port 2))
  
  (define init0 `[,tile0 ,two 0 0]) ;; init action chosen by "red" player 
  (define (i0)  (oos*json (init-action->jsexpr init0)))

  (define a0 '[])
  (define j0 (intermediate*->jsexpr a0))
  (check-equal? (mp i0 initial a0 0 1 2) (list `["initial" [,j0 0 1 2]] init0) "empty board")
  
  (define b0 `[[,jile0 "red" ,two 0 0]])
  (define k0 (intermediate*->jsexpr b0))
  (check-equal? (mp i0 initial b0 0 1 2) (list `["initial" [,k0 0 1 2]] init0) "one tile present")

  (check-exn emm? (λ () (mp (λ () (oos*json "[0 90 80]")) initial b0 0 1 2)) "badly shaped")
  (check-exn emm? (λ () (mp (λ () (oos*json "[0")) initial b0 0 1 2)) "incomplete JSON ")

  ;; -------------------------------------------------------------------------------------------------
  ;; take turn 
  
  (define turn0 `[2 0]) ;; turn action chosen by "red" and "black" player (illegal by logic)
  (define (i*) (open-input-string (jsexpr->string (turn-action->jsexpr turn0))))

  (define c* '("red" "black" "green"))
  (define a* (map (λ (x c) `[,jile0 ,c ,two ,x 0]) '(0 2 4) c*))
  (define j* (intermediate*->jsexpr a*))
  (check-equal? (mp i* take-turn a* 0 1) [list `["take-turn" [,j* 0 1]] turn0] "first move past init")

  (define ++ [(jsexpr->tile turn0) (facing-port two)]) ;; compute where red is now 
  (define b* `[[,jile0 0 0] ,@(map (λ (x c p) `[,jile0 ,c ,p ,x 0]) '(1 2 4) c* `[,++ ,two ,two])])
  (define k* (intermediate*->jsexpr b*))
  (check-equal? (mp i* take-turn b* 0 1) [list `["take-turn" [,k* 0 1]] turn0] "intermediate turn")

  (check-exn emm? (λ () (mp (λ () (oos*json "[0 90 80]")) take-turn b* 0 1)) "bad tt")
  (check-exn emm? (λ () (mp (λ () (oos*json "[0 90")) take-turn b* 0 1)) "incomplete tt"))
