#lang racket

;; a tournament administrator that plays a complete tournament with players given ranked by "age"
;; and produces the list of first-placed players; it informs all non-cheaters whether they were
;; first-placed or not (boolean)

(require Tsuro/Code/Admin/basics)
(require (only-in Tsuro/Code/Common/player-interface player/c))

;; ---------------------------------------------------------------------------------------------------
(provide
 (contract-out
  [administrator (-> (and/c [listof player/c] cons? distinct?) (listof player/c))]))

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

#; {[Listof Player] -> [Listof Player]}
(define (administrator lop0)
  (define-values (winners cheats) (run-all-games lop0))
  (inform-all-non-cheaters winners lop0 cheats))

#;{[Listof Player] -> (values ([Listof Player] [Listof Player]))}
(define (run-all-games lop0)
  (let loop ([lop1 lop0][cheats '()])
    (define lop (re-sort lop1 lop0))
    (cond
      [(too-few-for-one-game lop) (values lop cheats)]
      [(enough-for-one-game lop)
       (match-define [list ranked new-cheats] (referee lop))
       (define all-cheats (append new-cheats cheats))
       (values (if (empty? ranked) '[] (first ranked)) all-cheats)]
      [else
       (define games   (prepare-games lop))
       (define results (map referee games))
       (match-define [list top-2 all-cheats] (top-2-reordered/cheaters results cheats))
       (loop top-2 all-cheats)])))

#|
def run_all_games(lop0):
  lop = lop0
  while True:
    if oo_few_for_a_game(lop):
      return ...
    if enough_for_one_game(lop):
      .. referee(lop)
      return ...
    # run many games 
    games   = prepare_games(lop)
    results = run_all_games(games)
    lop     = get_top_2(results)
|#

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

#; {[Listof Player] [Listof Player] [Listof Player] -> [Listof Player]}
;; EFFECT inform lop0 - cheaters of whether they are a member of winners
;; eliminate winners that fail this message 
(define (inform-all-non-cheaters winners lop0 cheats)
  (reverse 
   (for/fold ([final-winners '()]) ([p (in-set (set-subtract (apply set lop0) (apply set cheats)))])
     (define is-winner (cons? (member p winners)))
     (define void-failed (xsend p end-of-tournament is-winner))
     (cond
       [(failed? void-failed) final-winners]
       [is-winner (cons p final-winners)]
       [else final-winners]))))

(module+ test
  (define strategy (new first-strategy%))
  (define winner  (new player% [strategy strategy]))
  (define bad-win (new bad-end-of-tournament%))
  (define loser   (new player% [strategy strategy]))
  (define cheater (new bad-turn-time%))

  (define all (list winner bad-win loser cheater))
  
  (check-equal? (inform-all-non-cheaters (list winner bad-win) all (list cheater)) (list winner)))

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
(define (top-2-reordered/cheaters results* cheats)
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

  (check-true (empty? (administrator (append baddies baddies))))
  (check-true (empty? (administrator (append baddies baddies baddies baddies))))
  (check-equal? (administrator one-game) (first (first (referee one-game))))
  (check-true (cons? (administrator one-game)))
  (check-true (cons? (administrator two-games))))
