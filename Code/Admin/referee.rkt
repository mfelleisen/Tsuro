#lang racket

;; a referee that plays a single game with players given in the order of their "age"
;; and produces a ranking (list of players placed at same position) and a list of cheats

(require Tsuro/Code/Admin/basics)
(require (only-in Tsuro/Code/Common/player-interface referee-player/c))
(define referee-player/c*  (listof referee-player/c))
(define ranked/c [listof referee-player/c*])

(require Tsuro/Code/Admin/observer-interfaces)

(provide
 (contract-out
  (referee
   ;; run a complete game from initial moves thru final stages
   ;; EFFECT provide observers with updates on regular turns 
   (->* [(and/c [listof referee-player/c] distinct? enough?)] [#:observers (listof game-observer/c)]
        ;; yields 
        (list/c ranked/c referee-player/c*)))))

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

(require (except-in Tsuro/Code/Common/player-interface referee-player/c))
(require Tsuro/Code/Common/rules)
(require Tsuro/Code/Common/tokens)

(require Tsuro/Code/Lib/xsend)

(require SwDev/Debugging/spy )

(module+ test
  (require (submod ".."))
  (require Tsuro/Code/Admin/game-observer)
  (require Tsuro/Code/Players/player)
  (require (prefix-in 1: Tsuro/Code/Players/first-s))
  (require (prefix-in 2: Tsuro/Code/Players/second-s))
  (require (prefix-in 3: Tsuro/Code/Players/third-s))
  (require (submod Tsuro/Code/Common/board test-cases))
  (require Tsuro/Code/Common/port)
  (require rackunit)

  (require (for-syntax racket/syntax))
  (require (for-syntax syntax/parse)))

;                                                                 
;       ;                                                         
;       ;           ;                                             
;       ;           ;                                             
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;         
;   ;; ;;      ;    ;        ;          ;;  ; ;;  ;  ;; ;;        
;   ;   ;      ;    ;        ;          ;     ;   ;; ;   ;        
;   ;   ;   ;;;;    ;     ;;;;          ;     ;;;;;; ;   ;        
;   ;   ;  ;   ;    ;    ;   ;          ;     ;      ;   ;        
;   ;; ;;  ;   ;    ;    ;   ;          ;     ;      ;; ;;   ;;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;      ;;;;  ;;;;    ;;   
;                                                    ;            
;                                                    ;            
;                                                    ;            

(define TILES (range TILES#))
(define TIME-PER-CALL .8)

#; {type Tiles* = [Listof TileIndex]}

#; {type Rankings = [Listof Player*]}
#; (type Player*  = [Listof Player])
#; {type Observer* = [Listof Observer]}

(struct internal [external avatar age] #:transparent)
#; {type Internal = (internal Player Avatar N)}

#; {type Internal* = [Listof Internal]}

;                                                   
;                    ;;                             
;                   ;                               
;                   ;                               
;    ;;;;   ;;;   ;;;;;   ;;;    ;;;;   ;;;    ;;;  
;    ;;  ; ;;  ;    ;    ;;  ;   ;;  ; ;;  ;  ;;  ; 
;    ;     ;   ;;   ;    ;   ;;  ;     ;   ;; ;   ;;
;    ;     ;;;;;;   ;    ;;;;;;  ;     ;;;;;; ;;;;;;
;    ;     ;        ;    ;       ;     ;      ;     
;    ;     ;        ;    ;       ;     ;      ;     
;    ;      ;;;;    ;     ;;;;   ;      ;;;;   ;;;; 
;                                                   
;                                                   
;                                                   

(define (referee external* #:observers (o*0 '()))
  (define internal* (assign-avatars external*))
  (define cheaters0 (inform-about-self-and-others internal*))
  (match-define (list state0 cheaters1 remaining) (initial-placements (remove* cheaters0 internal*)))
  ;; there are always enough tiles left so remaining doesn't need to be refilled
  (define o* (map (λ (uninit-observer) (uninit-observer)) o*0))
  (match-define (list ranked cheaters2) (play-game state0 remaining (remove* cheaters1 internal*) o*))
  (list ranked (map internal-external (append cheaters2 cheaters1 cheaters0))))

;; ---------------------------------------------------------------------------------------------------
#; {Player* -> Internal*}
(define (assign-avatars external*)
  (define max (length external*))
  (for/list ((e external*) (c AVATAR-COLORS) (age (in-naturals)))
    (internal e c (- max age))))

(module+ test
  (check-equal? (assign-avatars '(0 1 2)) (map internal (range 3) (take AVATAR-COLORS 3) '(3 2 1))))

;; ---------------------------------------------------------------------------------------------------
#; {Internal* -> Internal*}
(define (inform-about-self-and-others internal*)
  (define all-avatars (map internal-avatar internal*))
  (for/fold ((cheaters '())) ((i internal*))
    (match-define (internal external avatar age) i)
    (define void-failed (xsend external playing-as avatar))
    (cond
      [(failed? void-failed) (cons i cheaters)]
      [else (define void-failed (xsend external playing-with (remove avatar all-avatars)))
            (if (failed? void-failed) (cons i cheaters) cheaters)])))

(module+ test
  (define first-s (new 1:first-strategy%))
  
  (define-syntax (define-player stx)
    (syntax-parse stx
      [(_ avatar:id name #;{:str or :id} (~optional (~seq #:with %) #:defaults ([% #'player%])))
       (with-syntax ([name-external (make-name stx "external" #'avatar #'name)]
                     [name-internal (make-name stx "internal" #'avatar #'name)])
         #'(define-values (name-external name-internal)
             (let* ([player (if (eq? player% %) (new % [strategy first-s]) [new %])])
               (values player (internal player (~a 'avatar) 0)))))]))

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
  (define-player white as #:with bad-playing-as%)
  (define-player white with #:with bad-playing-with%)
  
  (define inform0 (list white-as-internal red-internal white-with-internal))
  (check-equal? (inform-about-self-and-others inform0) (list (third inform0) (first inform0)))
  
  (define baddies (list white-as-external white-with-external))

  ;; reverse because they get cons-ed on to the empty list as the ref folds over the list
  (check-equal? (second (referee (append baddies baddies))) (reverse (append baddies baddies))))

;                                                   
;                                                   
;      ;             ;     ;       ;          ;;;   
;                          ;                    ;   
;    ;;;   ; ;;    ;;;   ;;;;;   ;;;   ;;;;     ;   
;      ;   ;;  ;     ;     ;       ;       ;    ;   
;      ;   ;   ;     ;     ;       ;       ;    ;   
;      ;   ;   ;     ;     ;       ;    ;;;;    ;   
;      ;   ;   ;     ;     ;       ;   ;   ;    ;   
;      ;   ;   ;     ;     ;       ;   ;   ;    ;   
;    ;;;;; ;   ;   ;;;;;   ;;;   ;;;;;  ;;;;     ;; 
;                                                   
;                                                   
;                                                   

(define INIT# 3)

#; {Internal* -> (list State Internal* Tiles*)}
;; KNOWLEDGE there are enough tile types to initialize the board 
(define (initial-placements internal*)
  (define places0 '[])
  (define state0  (initialize places0))
  (for/fold ([initials places0][state state0][cheats '()][remaining TILES]
	     #:result (list state cheats remaining)) ([i internal*])
    (one-initial-turn i initials state cheats remaining)))

#; {Internal [Listof Initial] State Player* Tiles* -> (values [Listof Initial] State Player* Tiles*)}
(define (one-initial-turn i initials state cheats remaining)
  (match-define (internal external avatar age) i)
  (match-define [list (list tile1 tile2 tile3) tiles+1] (split-tiles remaining INIT#))
  (define choice-failed (xsend external initial initials tile1 tile2 tile3))
  (cond
    [(failed? choice-failed) (values initials state (cons i cheats) tiles+1)]
    [(legal-initial (initialize initials) avatar tile1 tile2 tile3 choice-failed)
     =>
     (λ (next) (values (cons (->initials avatar choice-failed) initials) next cheats tiles+1))]
    [else (values initials state (cons i cheats) tiles+1)]))

#; {Avatar InitialAction -> Initial}
(define (->initials avatar ia)
  (match-define (list (list ti d) port x y) ia)
  (define tile (rotate-tile (tile-index->tile ti) #:degree d))
  (list tile avatar port x y))

(module+ test
  (define-player white init-time #:with bad-init-time%)
  (define-player white choice #:with bad-init-choice%)

  (define initial0 (list red-internal white-init-time-internal white-choice-internal))
  (define good-init (send/apply red-external initial '[] (take TILES 3)))
  (check-equal? (initial-placements initial0)
                (list (initialize (list (->initials (internal-avatar red-internal) good-init)))
                      (reverse (rest initial0))
                      (drop TILES 9))))

;                              
;                              
;                              
;                              
;    ;;;;  ;;;;  ;;;;;;   ;;;  
;   ;;  ;      ; ;  ;  ; ;;  ; 
;   ;   ;      ; ;  ;  ; ;   ;;
;   ;   ;   ;;;; ;  ;  ; ;;;;;;
;   ;   ;  ;   ; ;  ;  ; ;     
;   ;; ;;  ;   ; ;  ;  ; ;     
;    ;;;;   ;;;; ;  ;  ;  ;;;; 
;       ;                      
;    ;  ;                      
;     ;;                       

(define TURN# 2)

#; {State Tiles* Internal* Observer* -> (list Rankings Internal*)}
(define (play-game state0 tiles0 i* (o* '()))
  (let play ([state state0][tiles tiles0][rankings '[]][cheats '[]][o* o*])
    (cond
      [(final? state)
       (define s (survivors state))
       (list (add-rank (map (find-external i*) s) rankings) cheats)]
      [else
       (match-define [list state+1 tiles+1 ranked cheats+1 o*+1] (play-1-round state tiles i* o*))
       (play state+1 tiles+1 (add-rank ranked rankings) (append cheats+1 cheats) o*+1)])))

#; {Player* Rankings -> Rankings}
(define (add-rank ranked rankings)
  (if (empty? ranked) rankings (cons ranked rankings)))

(module+ test #; play-game
  (define-player red turn-time #:with bad-turn-time%)
  (define-player blue turn-time #:with bad-turn-time%)

  (define forced-suicide-tiles (list state-suicide-index state-suicide-index))
  (define 2-rounds-suicide (append forced-suicide-tiles forced-suicide-tiles))

  (check-equal? (play-game collision-state++ 2-rounds-suicide (list red-internal white-internal))
                (list (list (map internal-external (list white-internal red-internal))) '[])
                "play one game forced infinite loop of two players")

  (define blue-time-spot (list (index->port 2) 0 8))
  (define blue-tile      (tile-index->tile 0))
  (define blue-collision (place-first-tile collision-state "blue" blue-tile blue-time-spot))
  (define blue-+-players `(,blue-turn-time-internal ,red-internal ,white-internal))
  (check-equal? (play-game blue-collision 2-rounds-suicide blue-+-players)
                (list (list (map internal-external (list white-internal red-internal)))
                      `[,blue-turn-time-internal])
                "play one game forced infinite loop of two players"))

;                                     
;                                   ; 
;                                   ; 
;                                   ; 
;    ;;;;   ;;;   ;   ;  ; ;;    ;;;; 
;    ;;  ; ;; ;;  ;   ;  ;;  ;  ;; ;; 
;    ;     ;   ;  ;   ;  ;   ;  ;   ; 
;    ;     ;   ;  ;   ;  ;   ;  ;   ; 
;    ;     ;   ;  ;   ;  ;   ;  ;   ; 
;    ;     ;; ;;  ;   ;  ;   ;  ;; ;; 
;    ;      ;;;    ;;;;  ;   ;   ;;;; 
;                                     
;                                     
;                                     

#; {State Tiles* Internal* Observer* -> (list State [Listf TileIndex] Player* Internal* Observer*)}
(define (play-1-round state0 tiles0 internal* (observers0 '()))
  (define finder (find-external internal*))
  (for/fold ([state state0][tiles tiles0][ranked '[]][cheats '()][o* observers0]
	     #:result (list state tiles ranked cheats o*)) ([i internal*])
     (play-1-turn i state tiles ranked cheats finder o*)))

(module+ test #; play-one-round 
  (define active3 (list red-turn-time-internal))
  (check-equal? (play-1-round state-suicide TILES active3)
                (list (minus-player state-suicide "red")
                      (cddr TILES)
                      '[]
                      `[,red-turn-time-internal]
                      '[])
                "turn diverges")

  (define chosen-suicide-tiles (cons state-suicide-index TILES))
  (check-equal? (play-1-round state-suicide chosen-suicide-tiles (list red-internal))
                (list (minus-player state-suicide "red")
                      (cddr chosen-suicide-tiles)
                      '[]
                      `[,red-internal]
                      '[])
                "chosen suicide of last player")
  
  (define rankings `[[,red-external]])
  (define cheats   '[])
  (check-equal? (play-1-round state-suicide forced-suicide-tiles (list red-internal))
                (list state-suicide++ TILES `[,red-external] '[] '[])
                "forced suicide of last player")
  
  ;; this test case shows that the "at periphery or 2 neightbors" condition on intermediates
  ;; is too strong; but it is not clear what to replace it with because sometimes it's okay
  (check-equal? (play-1-round collision-state+++ 2-rounds-suicide `(,red-internal ,white-internal))
                (list (minus-player (minus-player collision-state+++ "red") "white")
                      (cddr 2-rounds-suicide)
                      (map internal-external (list white-internal red-internal))
                      '[]
                      '[])
                "one turn infinite loop of two players (1)"))

;                              
;                              
;     ;                        
;     ;                        
;   ;;;;;  ;   ;   ;;;;  ; ;;  
;     ;    ;   ;   ;;  ; ;;  ; 
;     ;    ;   ;   ;     ;   ; 
;     ;    ;   ;   ;     ;   ; 
;     ;    ;   ;   ;     ;   ; 
;     ;    ;   ;   ;     ;   ; 
;     ;;;   ;;;;   ;     ;   ; 
;                              
;                              
;                              

#; {type Find = [Avatar -> Player]}

#; {Internal State Tiles* Rankings Player* Find -> (values State Tiles* Player* Internal* Observer*)}
(define (play-1-turn i state tiles ranked cheats finder (observers '()))
  (match-define (internal external avatar age) i)
  (cond
    [(boolean? (member avatar (survivors state))) (values state tiles ranked cheats observers)]
    [else 
     (match-define (list (list tile1 tile2) tiles+1) (split-tiles tiles TURN#))
     (define choice-failed
       (parameterize ([time-out-limit TIME-PER-CALL])
         (xsend external take-turn (state->intermediate* state) tile1 tile2)))
     (log-error "~a response from player" choice-failed)
     (define potential-turn (list (list avatar age choice-failed) tile1 tile2))
     (cond
       [(failed? choice-failed)
        (values (minus-player state avatar) tiles+1 ranked (cons i cheats) observers)]
       [(legal-take-turn state avatar tile1 tile2 choice-failed)
        => (λ (next)
             (define o* (xinform-observers observers state potential-turn next))
             (values next tiles+1 (add-to ranked finder state next) cheats o*))]
       [else
        (define o* (xinform-observers observers state potential-turn #false))
        (values (minus-player state avatar) tiles+1 ranked (cons i cheats) o*)])]))

;; ---------------------------------------------------------------------------------------------------
#; {Rankings [Avatar -> Player] State State -> Rankings}
(define (add-to rankings finder state-n state-n+1)
  (define survivors-n (apply set (survivors state-n)))
  (define survivors-1 (apply set (survivors state-n+1)))
  (cond
    [(set=? survivors-1 survivors-n) rankings]
    [else (append (map finder (set->list (set-subtract survivors-n survivors-1))) rankings)]))

#; {Internal* -> [ Avatar -> Player ]}
(define [(find-external internal*) avatar]
  (internal-external (first (memf (λ (i) (equal? (internal-avatar i) avatar)) internal*))))

(module+ test #; find-external 
  (define finder0 (find-external '[]))
  (check-equal? (add-to '[] finder0  good-intermediate-state good-intermediate-state++) '[])
  
  (define finder1 (find-external (list red-internal)))
  (check-equal? (add-to '[] finder1 state-suicide state-suicide++) (list red-external))

  (define active2 `(,red-internal ,white-internal ,blue-internal))
  (define finder2 (find-external active2))
  (check-equal? (add-to '[] finder2 good-intermediate-state+ state-suicide)
                (map internal-external (rest active2))))

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

#;
(module+ test
  (provide 3-illegal-with-observer 3-legal-with-observer)
  
  (log-error "refereeing 3")
  ;; the colors that are assigned to the players have nothing to do with their names
  (define (3-illegal-with-observer)
    (referee (list red-external white-external green-external) #:observers (list show-turn)))

  (define (make-p) (new player% [strategy (new 2:first-strategy%)]))
  (define a (make-p))
  (define b (make-p))
  (define c (make-p))

  (define (3-legal-with-observer)
    (referee (list a b c) #:observers (list show-turn)))
  
  (log-error "refereeing 4")
  (referee (list red-external white-external green-external blue-external))
  (log-error "refereeing 5")
  (referee (list red-external white-external green-external blue-external black-external)))

(module+ test 
  (log-error "report of potential bug")

  ;; the most common reported error occurs in: 
  #|
    [{"name":"a","strategy":"Tsuro/Code/Players/second-s.rkt"},
     {"name":"b","strategy":"Tsuro/Code/Players/third-s.rkt"},
     {"name":"c","strategy":"Tsuro/Code/Players/first-s.rkt"},
     {"name":"d","strategy":"Tsuro/Code/Players/first-s.rkt"}]
  |#

  (define 10-1 (new player% [strategy (new 2:second-s%)]))
  (define 10-2 (new player% [strategy (new 3:third-s%)]))
  (define 10-3 (new player% [strategy (new 1:first-s%)]))
  (define 10-4 (new player% [strategy (new 1:first-s%)]))
  
  (define b10 (internal 10-1 "black" 1)) (send 10-1 playing-as "black")
  (define w10 (internal 10-2 "white" 2)) (send 10-2 playing-as "white")
  (define i10 (internal 10-3 "red" 2))   (send 10-3 playing-as "red")
  (define g10 (internal 10-4 "green" 2)) (send 10-4 playing-as "green")
  (define internals (list i10 b10 w10 g10))
  
  (check-equal? (let-values (([s b c d e] (play-1-turn i10 state10-pre '(24 25) '[] '[] (λ _ 10-3))))
                  (list s d)) ;; [state, cheats]
                (list (minus-player state10-pre "red") (list i10))
                "probable failure from hw9")
  
  (check-equal? (fourth (play-1-round state10-pre '[24 25] (list i10 b10) )) (list i10))

  (check-equal? (second (play-game state10-pre '[24 25] internals)) (list i10))

  (check-equal? (second (referee (list 10-1 10-2 10-3 10-4))) `(,10-4 ,10-3)))

#;
(module+ test

  ;; test case Code/9/all-tests/8-in.json 
  #|
  [{"name":"a","strategy":"Tsuro/Code/Players/third-s.rkt"},
   {"name":"b","strategy":"Tsuro/Code/Players/third-s.rkt"},
   {"name":"c","strategy":"Tsuro/Code/Players/third-s.rkt"},
   {"name":"d","strategy":"Tsuro/Code/Players/third-s.rkt"},
   {"name":"e","strategy":"Tsuro/Code/Players/third-s.rkt"}]
  |#

  (define (make-9) (new player% [strategy (new 3:third-s%)]))
  (referee (list (make-9) (make-9) (make-9) (make-9) (make-9)) #:observers (list show-turn)))

#;
(module+ picts
  (require (submod ".." test))
  (3-illegal-with-observer)
  (3-legal-with-observer))


;                                                                        
;                                                                        
;                                                                        
;                                                                        
;    ;;;    ;;;  ;;;;;; ;;;;;;   ;;;   ; ;;          ;;;;   ;   ;  ;   ; 
;   ;;  ;  ;; ;; ;  ;  ;;  ;  ; ;; ;;  ;;  ;             ;  ;   ;   ; ;  
;   ;      ;   ; ;  ;  ;;  ;  ; ;   ;  ;   ;             ;  ;   ;   ;;;  
;   ;      ;   ; ;  ;  ;;  ;  ; ;   ;  ;   ;          ;;;;  ;   ;    ;   
;   ;      ;   ; ;  ;  ;;  ;  ; ;   ;  ;   ;         ;   ;  ;   ;   ;;;  
;   ;;     ;; ;; ;  ;  ;;  ;  ; ;; ;;  ;   ;         ;   ;  ;   ;   ; ;  
;    ;;;;   ;;;  ;  ;  ;;  ;  ;  ;;;   ;   ;          ;;;;   ;;;;  ;   ; 
;                                                                        
;                                                                        
;                                                                        

#; {Tiles* N -> (List Tiles* Tiles*)}
;; the second list has at least n elements 
(define (split-tiles r n)
  (define remaining (drop r n))
  (list (take r n) (if (>= (length remaining) n) remaining (append remaining TILES))))

(module+ test
  (check-equal? (split-tiles '(a b c d) 2) '[(a b) [c d]])
  (check-equal? (split-tiles '(a b c) 3) (list '(a b c) TILES)))

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

#; {[Listof Observer] Turn (U State #false) -> [Listof Observer]}
;; inform observers about the current turn; produce all those that interact properly 
(define (xinform-observers observers0 state turn legal)
  (let loop ([observers observers0][broken '[]])
    (cond
      [(empty? observers) (remove* broken observers0)]
      [else 
       (define o1 (first observers))
       (define void-failed (o1 state turn legal))
       (if (failed? void-failed)
           (loop (remove o1 (rest observers)) (cons o1 broken))
           (loop (rest observers) broken))])))

;; ---------------------------------------------------------------------------------------------------
(define BAD-PLACEMENT:fmt "~a broke the rules of placing initial avatars~a")
(define BAD-MOVE:fmt      "~a broke the take-turn rules\n [~e]")
(define XOTHER:fmt        "~a's 'other' method failed\n [~e]")
(define XSETUP1:fmt       "~a failed with the 'placement' method\n[~a]")
(define XSETUP2:fmt       "~a timed out for the 'placement' method\n[~a]")
(define XPLAY:fmt         "~a failed with the 'take-turn' method\n[~a]")
