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
(require Tsuro/Code/Players/strategies)
(require Tsuro/Code/Lib/xsend)

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

#; {[Listof Player] -> [Listof Player]}
(define (administrator lop0)
  (define-values (winners cheats)
    (let administrator ([lop lop0][cheats '()])
      (case (length lop)
        [(0 1 2) (values lop cheats)]
        [(3 4 5)
         (match-define (list ranked c) (referee lop))
         (values (if (empty? ranked) '() (first ranked)) cheats)]
        [else
         (define games   (prepare-games lop))
         (define results (map referee games))
         (define top-2   (get-top-2 results))
         (administrator top-2 (append (append-map second results) cheats))])))
  (inform-all-non-cheaters winners lop0 cheats))

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
;                       
;                       
;   ;;;;   ;   ;  ;   ; 
;       ;  ;   ;   ; ;  
;       ;  ;   ;   ;;;  
;    ;;;;  ;   ;    ;   
;   ;   ;  ;   ;   ;;;  
;   ;   ;  ;   ;   ; ;  
;    ;;;;   ;;;;  ;   ; 
;                       
;                       
;                       

#; {[Listof [List [Listof Player*] Player*]] -> [Listof Player]}
;; get the list of finishers in the top 2 places (if any)
(define (get-top-2 results*)
  (define (get-top-2 results)
    (match-define (list ranked cheats) results)
    (match ranked
      ['() '()]
      [`(,firsts) firsts]
      [`(,firsts ,seconds ,others ...) (append firsts seconds)]))
  (append-map get-top-2 results*))

#; {[Listof Player] -> [Listof [Listof Player]]}
;; lop contains GAME# or more players 
(define (prepare-games lop)
  ;; GEN REC
  (define first-game (take lop MAX-PLAYERS))
  (define remaining  (drop lop MAX-PLAYERS))
  (reverse
   (let loop ([lop remaining][N (length remaining)][games-of-proper-length (list first-game)])
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

;                                                                 
;                                                                 
;                                 ;                    ;          
;                                 ;                    ;          
;   ;;;;   ;   ;  ;   ;         ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;       ;  ;   ;   ; ;            ;    ;;  ;  ;   ;    ;    ;   ; 
;       ;  ;   ;   ;;;            ;    ;   ;; ;        ;    ;     
;    ;;;;  ;   ;    ;             ;    ;;;;;;  ;;;     ;     ;;;  
;   ;   ;  ;   ;   ;;;            ;    ;          ;    ;        ; 
;   ;   ;  ;   ;   ; ;            ;    ;      ;   ;    ;    ;   ; 
;    ;;;;   ;;;;  ;   ;           ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                                                 
;                                                                 
;                                                                 

(module+ test
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

  (define one-game (list green-external red-external blue-external black-external white-external))
  (define two-games (append (list white-as-external white-with-external) one-game))
  
  (administrator one-game)
  (administrator two-games))