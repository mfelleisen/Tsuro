#lang racket

;; a tournament administrator that plays a complete tournament with players given ranked by "age"
;; and produces the list of first-placed players; it informs all non-cheaters whether they were
;; first-placed or not (boolean)

(require Tsuro/Code/Admin/basics)
(require (only-in Tsuro/Code/Common/player-interface player/c))

(define player*/c [listof player/c])

(require Tsuro/Code/Admin/observer-interfaces)

;; ---------------------------------------------------------------------------------------------------
(provide
 (contract-out
  [administrator
   (->* [(and/c player*/c cons? distinct?)] [#:observers (listof tournament-observer/c)]
        ;; yields 
        (list/c (listof player*/c) player*/c))]))



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

(require Tsuro/Code/Admin/referee)
(require Tsuro/Code/Players/player)
(require Tsuro/Code/Players/first-s)
(require Tsuro/Code/Lib/xsend)

(require SwDev/Debugging/spy)

(module+ test
  (require (submod ".."))
  (require Tsuro/Code/Players/player)
  (require Tsuro/Code/Players/strategies)
  (require rackunit)

  (require (for-syntax racket/syntax))
  (require (for-syntax syntax/parse)))

;                                                   
;                                                   
;                                                   
;                                                   
;  ;;;;;;  ;;;;   ; ;;   ;;;;    ;;;;   ;;;    ;;;; 
;  ;  ;  ;     ;  ;;  ;      ;  ;;  ;  ;;  ;   ;;  ;
;  ;  ;  ;     ;  ;   ;      ;  ;   ;  ;   ;;  ;    
;  ;  ;  ;  ;;;;  ;   ;   ;;;;  ;   ;  ;;;;;;  ;    
;  ;  ;  ; ;   ;  ;   ;  ;   ;  ;   ;  ;       ;    
;  ;  ;  ; ;   ;  ;   ;  ;   ;  ;; ;;  ;       ;    
;  ;  ;  ;  ;;;;  ;   ;   ;;;;   ;;;;   ;;;;   ;    
;                                   ;               
;                                ;  ;               
;                                 ;;                

#; {type Player* = [Listof Player]}
#; {type Results = [List [Listof Player*] Player*]}
;; [Listof Player*] represents the rankings; the sub-lists are non-empty?;
;; Player* are the cheaters 

#; {[Listof Player] -> Results}
(define (administrator lop0 #:observers (o*0 '()))
  (define max-age (length lop0))
  (define o* (map (λ (uninit-observer) (uninit-observer max-age)) o*0))
  (define-values (ranked cheats) (run-all-games lop0 o*))
  (if (empty? ranked)
      (list '[] cheats) ;; everybody is a cheater 
      (inform-all/not-cheaters (first ranked) (apply append (rest ranked)) cheats)))

#;{[Listof Player] [Listof Observer] -> (values ([Listof Player] [Listof Player]))}
(define (run-all-games lop0 o*0)
  (let loop ([lop1 lop0][lop#-previous-round +inf.0][cheats '()][o* o*0])
    (define lop (re-sort lop1 lop0))
    (define lop# (length lop))
    (cond
      [(>= lop# lop#-previous-round)
       (log-error "infinite loop detected")
       (xinform-observers o* (list lop) lop0) ;; final observation
       (values (list lop) cheats)]
      [(too-few-for-one-game lop)
       (xinform-observers o* (list lop) lop0) ;; final observation
       (values (list lop) cheats)]
      [(enough-for-one-game lop)
       (define o*1 (xinform-observers o* (list lop) lop0))
       (match-define [list ranked new-cheats] (referee lop))
       (define all-cheats (append new-cheats cheats))
       (cond
         [(empty? ranked)
          (xinform-observers o*1 '[] lop0) ;; final observation
          (values '[[]] all-cheats)]
         [else
          (xinform-observers o*1 (list (re-sort (first ranked) lop0)) lop0)
          (values ranked all-cheats)])]
      [else
       (define games   (prepare-games lop))
       (define o*1     (xinform-observers o* games lop0))
       (define results (map referee games))
       (match-define `[,top-2 ,all-cheats] (top-2/cheats results cheats))
       (loop top-2 lop# all-cheats o*1)])))

#;{Player* Player* -> Player*}
;; sort top-2 list according to lop0 
(define (re-sort top-2 lop0)
  (filter (λ (x) (member x top-2)) lop0))

#; {[Listof X] -> Boolean}
(define (too-few-for-one-game lop)
  (or (empty? lop) (empty? (rest lop)) (empty? (rest (rest lop)))))

#; {[Listof X] -> Boolean}
;; assuming lop has 3 elements 
(define (enough-for-one-game lop)
  (or (empty? (rest (rest (rest lop))))
      (empty? (rest (rest (rest (rest lop)))))
      (empty? (rest (rest (rest (rest (rest lop))))))))

(module+ test
  (define box% (class object% [init content] (super-new)))
  (match-define (list box1 box2 box3 box4) (map (λ (x) (new box% [content x])) '(1 2 3 4)))
  (check-equal? (re-sort (list box2 box1 box3) (list box1 box2 box3 box4)) (list box1 box2 box3)))

;                       
;                     ; 
;                     ; 
;                     ; 
;    ;;;   ; ;;    ;;;; 
;   ;;  ;  ;;  ;  ;; ;; 
;   ;   ;; ;   ;  ;   ; 
;   ;;;;;; ;   ;  ;   ; 
;   ;      ;   ;  ;   ; 
;   ;      ;   ;  ;; ;; 
;    ;;;;  ;   ;   ;;;; 
;                       
;                       
;                       

#; {[Listof Player] [Listof Player] [Listof Player] -> Results}
;; EFFECT inform winners and losers; move players that fail this message into cheaters 
(define (inform-all/not-cheaters winners0 losers0 cheats0)
  (define-values (winners cheats1)
    (for/fold ([winners '()][cheats cheats0]) ([p (in-list winners0)])
      (inform-one p #true winners cheats)))
  (define-values (losers cheaters)
    (for/fold ([losers '()][cheats2 cheats1]) ([ranked losers0])
      (for/fold ([lost '[]][cheats cheats2] #:result (values (cons* lost losers) cheats)) ((r ranked))
        (inform-one r #false lost cheats))))
  (list (cons winners (reverse losers)) cheaters))

#; {Player Boolean [Listof Player] [Listof Player] -> (values [Listof Player] [Listof Player])}
(define (inform-one p msg winners cheats)
  (define void-failed (xsend p end-of-tournament msg))
  (if (failed? void-failed)
      (values winners (cons p cheats))
      (values (cons p winners) cheats)))

(define (cons* x lox) (if (empty? x) lox (cons x lox)))
  
(module+ test
  (define strategy  (new first-strategy%))
  (define winner    (new player% [strategy strategy]))
  (define bad-win   (new bad-end-of-tournament%))
  (define loser     (new player% [strategy strategy]))
  (define bad-loser (new bad-end-of-tournament%))
  (define cheater   (new bad-turn-time%))
  
  (check-equal? (inform-all/not-cheaters `[,winner ,bad-win] `[[,loser] [,bad-loser]] `[,cheater])
                `[[[,winner] [,loser]] [,bad-loser ,bad-win ,cheater]]))

;                                     
;                                     
;     ;                          ;;;; 
;     ;                         ;    ;
;   ;;;;;   ;;;   ;;;;               ;
;     ;    ;; ;;  ;; ;;              ;
;     ;    ;   ;  ;   ;             ; 
;     ;    ;   ;  ;   ;            ;  
;     ;    ;   ;  ;   ;           ;   
;     ;    ;; ;;  ;; ;;          ;    
;     ;;;   ;;;   ;;;;          ;;;;;;
;                 ;                   
;                 ;                   
;                 ;                   
  
#; {[Listof Results] Player* Player* -> [List Player* Player*]}
;; retrieve the list of finishers in the top 2 places (if any)
;; order them in terms of age
;; compute the cheaters 
(define (top-2/cheats results* cheats)
  (define all-top-2-finishers (append-map get-top-2-aux results*))
  (define cheaters            (append-map second results*))
  (list all-top-2-finishers cheaters))

#; {[Listof Results] -> Player*}
;; get the top-2 rankings, as a a single list 
(define (get-top-2-aux results)
  (match-define (list ranked cheats) results)
  (match ranked
    ['() '()]
    [`(,firsts) firsts]
    [`(,firsts ,seconds ,others ...) (append firsts seconds)]))

;                                                   
;                                                   
;                                                   
;                                                   
;   ;;;;    ;;;;   ;;;   ;;;;   ;;;;    ;;;;   ;;;  
;   ;; ;;   ;;  ; ;;  ;  ;; ;;      ;   ;;  ; ;;  ; 
;   ;   ;   ;     ;   ;; ;   ;      ;   ;     ;   ;;
;   ;   ;   ;     ;;;;;; ;   ;   ;;;;   ;     ;;;;;;
;   ;   ;   ;     ;      ;   ;  ;   ;   ;     ;     
;   ;; ;;   ;     ;      ;; ;;  ;   ;   ;     ;     
;   ;;;;    ;      ;;;;  ;;;;    ;;;;   ;      ;;;; 
;   ;                    ;                          
;   ;                    ;                          
;   ;                    ;                          

#; {[Listof Player] -> [Listof [Listof Player]]}
(define/contract (prepare-games lop0)
  (-> (and/c list? (λ (l) (>= (length l) MIN-PLAYERS))) (listof list?))
  ;; GEN REC
  (reverse
   (let loop ([lop lop0][N (length lop0)][games-of-proper-length '()])
     (case N
       [(0) games-of-proper-length]
       [(1)
        (define lop-1 (append (first games-of-proper-length) lop))
        (define game1 (take lop-1 (- MAX-PLAYERS 2)))
        (define game2 (drop lop-1 (- MAX-PLAYERS 2)))
        (list* game2 game1 (rest games-of-proper-length))]
       [(2)
        (define lop-1 (append (first games-of-proper-length) lop))
        (define game1 (take lop-1 (- MAX-PLAYERS 1)))
        (define game2 (drop lop-1 (- MAX-PLAYERS 1)))
        (list* game2 game1 (rest games-of-proper-length))]
       [(3 4)
        (cons lop games-of-proper-length)]
       [else
        (define next-game (take lop MAX-PLAYERS))
        (define remaining (drop lop MAX-PLAYERS))
        (loop remaining (- N MAX-PLAYERS) (cons next-game games-of-proper-length))]))))

(module+ test
  (check-equal? (prepare-games '(a b c)) '[(a b c)])
  (check-equal? (prepare-games '(a b c d)) '[(a b c d)])
  (check-equal? (prepare-games '(a b c d e f)) '[(a b c) (d e f)])
  (check-equal? (prepare-games '(a b c d e f g)) '[(a b c d) (e f g)])
  (check-equal? (prepare-games '(a b c d e f g h)) '[(a b c d e) (f g h)])
  (check-equal? (prepare-games '(a b c d e f g h i j)) '[(a b c d e) (f g h i j)])
  (check-equal? (prepare-games '(z y x u v a b c d e f)) '[(z y x u v) (a b c) (d e f)]))


;                                                                 
;          ;                                                      
;          ;                                                      
;          ;                                                      
;    ;;;   ;;;;    ;;;    ;;;    ;;;;  ;   ;   ;;;    ;;;;   ;;;  
;   ;; ;;  ;; ;;  ;   ;  ;;  ;   ;;  ; ;   ;  ;;  ;   ;;  ; ;   ; 
;   ;   ;  ;   ;  ;      ;   ;;  ;      ; ;   ;   ;;  ;     ;     
;   ;   ;  ;   ;   ;;;   ;;;;;;  ;      ; ;   ;;;;;;  ;      ;;;  
;   ;   ;  ;   ;      ;  ;       ;      ; ;   ;       ;         ; 
;   ;; ;;  ;; ;;  ;   ;  ;       ;       ;    ;       ;     ;   ; 
;    ;;;   ;;;;    ;;;    ;;;;   ;       ;     ;;;;   ;      ;;;  
;                                                                 
;                                                                 
;                                                                 


#; {[Listof Observer] [Listof [Listof Player]] [Listof Player] -> [Listof Observer]}
;; inform observers about the current turn; produce all those that interact properly 
(define (xinform-observers observers0 games lop0)
  (define ages (games->ages games lop0))
  (let loop ([observers observers0][broken '[]])
    (cond
      [(empty? observers) (remove* broken observers0)]
      [else 
       (define o1 (first observers))
       (define void-failed (o1 ages))
       (if (failed? void-failed)
           (loop (remove o1 (rest observers)) (cons o1 broken))
           (loop (rest observers) broken))])))

#; {[Listof [Listof Player]] [Listof Player] -> [Listof [Listof N]]}
(define (games->ages games lop0)
  (define player->age-local (player->age lop0))
  (map (λ (1game) (map player->age-local 1game)) games))

#; {[Listof Player] -> Player -> N}
(define ((player->age lop0) player)
  (- (length (member player lop0)) 1))

(module+ test
  (define oldest-3 (box 3))
  (define middle-2 (box 2))
  (define middle-1 (box 1))
  (define youngest (box 0))
  (define players `[,oldest-3 ,middle-2 ,middle-1 ,youngest])

  (check-equal? ((player->age players) oldest-3) 3)
  (check-equal? ((player->age players) youngest) 0)


  (check-equal? (games->ages `[[,oldest-3 ,middle-2] [,middle-1 ,youngest]] players) `[[3 2][1 0]]))



;                                                                        
;                                                                        
;                    ;                   ;                    ;          
;                                        ;                    ;          
;  ;;;;;;  ;;;;    ;;;   ; ;;          ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;  ;  ;  ;     ;     ;   ;;  ;           ;    ;;  ;  ;   ;    ;    ;   ; 
;  ;  ;  ;     ;     ;   ;   ;           ;    ;   ;; ;        ;    ;     
;  ;  ;  ;  ;;;;     ;   ;   ;           ;    ;;;;;;  ;;;     ;     ;;;  
;  ;  ;  ; ;   ;     ;   ;   ;           ;    ;          ;    ;        ; 
;  ;  ;  ; ;   ;     ;   ;   ;           ;    ;      ;   ;    ;    ;   ; 
;  ;  ;  ;  ;;;;   ;;;;; ;   ;           ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                                                        
;                                                                        
;                                                                        

(module+ test

  (require Tsuro/Code/Admin/tournament-observer)

  (define-syntax (define-player stx)
    (syntax-parse stx
      [(_ avatar:id name #;{:str or :id} (~optional (~seq #:with %) #:defaults ([% #'player%])))
       (with-syntax ([name-external (make-name stx "external" #'avatar #'name)]
                     [name-internal (make-name stx "internal" #'avatar #'name)])
         #'(define-values (name-external name-internal)
             (let* ([player (new % [strategy strategy])])
               (values player player))))]))

  (define-for-syntax (make-name stx tag avatar name)
    (define a (symbol->string (syntax-e avatar)))
    (define n (format "~a" (syntax-e name)))
    (if (string=? n "") (format-id stx "~a-~a" a tag) (format-id stx "~a-~a-~a" a n tag)))

  (define-player green "")
  (define-player red   "")
  (define-player blue  "")
  (define-player black "")
  (define-player white ""))

(module+ test 
  (define bad-playing-as% (class player% (super-new) (define/override (playing-as c) (raise 0))))
  (define bad-playing-with% (class player% (super-new) (define/override (playing-with c) (raise 1))))

  (define-player white as #:with bad-playing-as%)
  (define-player white with #:with bad-playing-with%)
  (define baddies (list white-as-external white-with-external))
  (define one-game (list green-external red-external blue-external black-external white-external))
  (define two-games (append baddies one-game))

  (check-true (empty? (caar (administrator (append baddies baddies)))))
  (check-true (empty? (caar (administrator (append baddies baddies baddies baddies)))))
  (match-define [list ranked cheats] (referee one-game))
  (check-equal? (administrator one-game) (list ranked cheats))
  (check-true (cons? (administrator one-game)))
  (check-true (cons? (administrator two-games)))

  (provide legal-admin-with illegal-admin-with)
  (define (legal-admin-with)
    (administrator two-games #:observers (list show-tournament)))
  (define (illegal-admin-with)
    (define-player white as #:with bad-playing-as%)
    (define-player white with #:with bad-playing-with%)
    (define baddies2 (list white-as-external white-with-external))
    (administrator (append baddies baddies2) #:observers (list show-tournament))))

(module+ picts
  (require (submod ".." test))
  (legal-admin-with)
  [illegal-admin-with])
  
