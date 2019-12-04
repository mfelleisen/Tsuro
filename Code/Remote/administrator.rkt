#lang racket

;; a tournament administrator that plays a complete tournament with players given ranked by "age"
;; and produces the list of first-placed players; it informs all non-cheaters whether they were
;; first-placed or not (boolean)

(require (only-in Tsuro/Code/Common/player-interface player/c))
(require (only-in json jsexpr?))

(provide
 (contract-out
  [make-remote-administrator
   (-> (-> (-> (or/c eof-object? jsexpr?) jsexpr?) any) (-> player/c any/c))]))

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

(require (submod Tsuro/Code/Common/board json))
(require Tsuro/Code/Common/tokens)
(require Tsuro/Code/Common/tiles)

(require Tsuro/Code/Lib/xsend)
(require SwDev/Lib/pattern-matching)

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/tiles json))
  (require Tsuro/Code/Common/port)
  (require Tsuro/Code/Players/player)
  (require Tsuro/Code/Players/first-s)
  (require rackunit))

;                                                                               
;                                                        ;                      
;                                                        ;            ;         
;                                                        ;                      
;   ;;;;    ;;;;   ;;;   ;   ;  ;   ;         ;;;;    ;;;; ;;;;;;   ;;;   ; ;;  
;   ;; ;;   ;;  ; ;; ;;   ; ;   ;   ;             ;  ;; ;; ;  ;  ;    ;   ;;  ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;              ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;    ;     ; ;           ;;;;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;   ;   ;     ;   ;   ;;;    ; ;          ;   ;  ;   ; ;  ;  ;    ;   ;   ; 
;   ;; ;;   ;     ;; ;;   ; ;    ;;           ;   ;  ;; ;; ;  ;  ;    ;   ;   ; 
;   ;;;;    ;      ;;;   ;   ;    ;            ;;;;   ;;;; ;  ;  ;  ;;;;; ;   ; 
;   ;                             ;                                             
;   ;                            ;                                              
;   ;                           ;;

(define ((make-remote-administrator receiver) player)
  (define done? (box (gensym)))
  (define r (dispatcher done? player))
  (let loop ()
    (time-out-limit 33)
    (receiver r)
    (unless (boolean? (unbox done?))
      (loop)))
  (unbox done?))

(def/mp initial
  (_ i t1 t2 t3) #'`[[,(and i (init-pat _ a p _x _y)) (... ...)] ,(? ti? t1) ,(? ti? t2) ,(? ti? t3)])
(def/mp intermediate
  (_ i t1 t2) #'`[,(and i (state-pat)) ,(? ti? t1) ,(? ti? t2)])

(require (submod Tsuro/Code/Common/actions json))

#; {[Box Boolean] Player -> [JSexpr -> JSexpr]}
(define ((dispatcher done? p) input-received)
  (match input-received
    [(? eof-object?) #false]
    [`["playing-as" [,(? avatar? as)]] (send p playing-as as) "void"]
    [`["others" [,(? avatar? others) ...]] (send p playing-with others) "void"]
    [`["initial" ,[initial i t1 t2 t3]] (init-action->jsexpr (send p initial (j->i i) t1 t2 t3))]
    [`["take-turn"  ,(intermediate i t1 t2)] (turn-action->jsexpr (send p take-turn (j->i i) t1 t2))]
    [`["end-of-tournament" [,(? boolean? result)]]
     (set-box! done? result)
     (send p end-of-tournament result)
     "void"]
    [other (error 'remote-administrator "the server sent an ill-formed message: ~e" other)]))

(define j->i jsexpr->intermediate*)
(define ti? tile-index?)

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
  (define p1 (new player% [strategy (new first-s%)]))
  (define b1 (box (gensym)))

  (check-equal? ((dispatcher b1 p1) eof) #false)

  (check-equal? ((dispatcher b1 p1) `["playing-as" ["red"]]) "void")
  (check-true (symbol? (unbox b1)))

  (check-equal? ((dispatcher b1 p1) `["others" ["red" "blue"]]) "void")
  (check-true (symbol? (unbox b1)))

  (check-equal? ((dispatcher b1 p1) `["end-of-tournament" [,#true]]) "void")
  (check-true (unbox b1))

  (set-box! b1 (gensym))
  (check-equal? ((dispatcher b1 p1) `["end-of-tournament" [,#false]]) "void")
  (check-false (unbox b1))

  (define two (index->port 2))
  (define a0 '[])
  (define j0 (intermediate*->jsexpr a0))
  (check-equal? ((dispatcher b1 p1) `["initial" [,j0 0 1 2]]) (init-action->jsexpr `[[2 0] ,two 1 0]))

  (define tile0 `[0 90])
  (define jile0 (jsexpr->tile tile0))
  (define c* '("red" "black" "green"))
  (define a* (map (λ (x c) `[,jile0 ,c ,two ,x 0]) '(0 2 4) c*))
  (define j* (intermediate*->jsexpr a*))
  (check-equal? ((dispatcher b1 p1) `["take-turn" [,j* 0 1]]) (turn-action->jsexpr `[0 0]))

  (check-true ((make-remote-administrator (λ (f) (f `["end-of-tournament" [#true]]))) p1))

  (define b*
    `[ ["playing-as" ["red"]]
       ["others" ["red" "blue"]]
       ["initial" [,j0 0 1 2]]
       ["take-turn" [,j* 0 1]]
       ["end-of-tournament" [,#true]] ])
  (check-true ((make-remote-administrator (λ (f) (begin0 (f (first b*)) (set! b* (rest b*))))) p1))

  (check-exn exn:fail? (λ () ((make-remote-administrator (λ (f) (f `[0 [#true]]))) p1))))
