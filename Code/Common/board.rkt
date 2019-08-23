#lang racket/gui

;; a data representation for game States, plus basic functions for manipulating them

;; TODO
;; -- factor out squares

;; safety
;; - contract for initialize and add?

;; legality checks: 
;; - determine whether the addition of a tile for a player P forces P to commit suicide
;; - determine whether the addition of a tile adds a cyclic path

;                                                                 
;                                                                 
;                          ;                           ;          
;                          ;                           ;          
;    ;;;    ;;;   ; ;;   ;;;;;   ;;;;  ;;;;    ;;;   ;;;;;   ;;;  
;   ;;  ;  ;; ;;  ;;  ;    ;     ;;  ;     ;  ;;  ;    ;    ;   ; 
;   ;      ;   ;  ;   ;    ;     ;         ;  ;        ;    ;     
;   ;      ;   ;  ;   ;    ;     ;      ;;;;  ;        ;     ;;;  
;   ;      ;   ;  ;   ;    ;     ;     ;   ;  ;        ;        ; 
;   ;;     ;; ;;  ;   ;    ;     ;     ;   ;  ;;       ;    ;   ; 
;    ;;;;   ;;;   ;   ;    ;;;   ;      ;;;;   ;;;;    ;;;   ;;;  
;                                                                 
;                                                                 
;                                                                 
  
(require (only-in Tsuro/Code/Common/square SIZE looking-at square-tile))
(require (only-in Tsuro/Code/Common/tiles tile?))
(require (only-in Tsuro/Code/Common/port-alphabetic port?))
(require Tsuro/Code/Lib/or)

#; {Nat -> Boolean : Index}
(define (index? z) (< -1 z SIZE))

;                                                                                             
;                                                                                             
;                 ;;;       ;            ;                  ;;;                               
;                   ;                    ;                    ;                               
;                   ;     ;;;    ;;;   ;;;;;         ;;;;     ;    ;;;;    ;;;    ;;;    ;;;  
;                   ;       ;   ;   ;    ;           ;; ;;    ;        ;  ;;  ;  ;;  ;  ;   ; 
;                   ;       ;   ;        ;           ;   ;    ;        ;  ;      ;   ;; ;     
;   ;;;;;;          ;       ;    ;;;     ;           ;   ;    ;     ;;;;  ;      ;;;;;;  ;;;  
;                   ;       ;       ;    ;           ;   ;    ;    ;   ;  ;      ;          ; 
;                   ;       ;   ;   ;    ;           ;; ;;    ;    ;   ;  ;;     ;      ;   ; 
;                    ;;   ;;;;;  ;;;     ;;;         ;;;;      ;;   ;;;;   ;;;;   ;;;;   ;;;  
;                                                    ;                                        
;                                                    ;                                        
;                                                    ;

#; {Location = [List Index Index]}

#; ({Any -> Any : X} ([Listof X] -> Boolean) -> Contract)
(define (make-list-of-places-ctc items/c ctc)
  (and/c [listof items/c]
         locations-distinct
         ctc))

#; {[Listof Intermediate] -> Boolean : locations are distinct }
(define (locations-distinct specifications)
  (define locations (map place-of specifications))
  (= (length locations) (set-count (apply set locations))))

#; {Intermediate -> [List Index Index]}
(define (place-of x) ;; this is a trick; I should place the Index parts first and second 
  (define r (reverse x))
  (list (second r) (first r)))

;                                                                                                    
;                                                                                                    
;                           ;                 ;;;                         ;;;                        
;                                               ;                           ;                        
;                  ;;;    ;;;   ; ;;    ;;;;    ;     ;;;          ;;;;     ;    ;;;;    ;;;    ;;;  
;                 ;   ;     ;   ;;  ;  ;;  ;    ;    ;;  ;         ;; ;;    ;        ;  ;;  ;  ;;  ; 
;                 ;         ;   ;   ;  ;   ;    ;    ;   ;;        ;   ;    ;        ;  ;      ;   ;;
;   ;;;;;;         ;;;      ;   ;   ;  ;   ;    ;    ;;;;;;        ;   ;    ;     ;;;;  ;      ;;;;;;
;                     ;     ;   ;   ;  ;   ;    ;    ;             ;   ;    ;    ;   ;  ;      ;     
;                 ;   ;     ;   ;   ;  ;; ;;    ;    ;             ;; ;;    ;    ;   ;  ;;     ;     
;                  ;;;    ;;;;; ;   ;   ;;;;     ;;   ;;;;         ;;;;      ;;   ;;;;   ;;;;   ;;;; 
;                                          ;                       ;                                 
;                                       ;  ;                       ;                                 
;                                        ;;                        ;                                 

#; {[List String (Index Index -> Boolean)] -> Contract }
(define (make-placement/c label+good?)
  (match-define (list label good?) label+good?)
  [list/dc
   [t tile?] [n string?] [p port?] [x index?] [y index?]
   #:post label (p x y)
   (and (good? x y)
        (player-facing-inward? p x y))])

#; (Index Index -> Boolean : index is near boder)
(define (bordering-periphery? x y)
  (or (= x 0) (= x SIZE) (= y 0) (= y SIZE)))

#; { PortIndex Index Index -> Boolean : p on (x,y) looks at an interior square}
(define (player-facing-inward? p x y)
  (define-values (x-facing y-facing) (looking-at p x y))
  (and (index? x-facing) (index? y-facing)))

(define at-periphery-facing-inward (list "at-periphery-facing-inward" bordering-periphery?))
(define facing-inward (list "facing-inward" (λ (x y) #t)))

;                                                                                      
;                                                                                      
;                    ;             ;     ;       ;          ;;;       ;                
;                                        ;                    ;                        
;                  ;;;   ; ;;    ;;;   ;;;;;   ;;;   ;;;;     ;     ;;;   ;;;;;   ;;;  
;                    ;   ;;  ;     ;     ;       ;       ;    ;       ;       ;  ;;  ; 
;                    ;   ;   ;     ;     ;       ;       ;    ;       ;      ;   ;   ;;
;   ;;;;;;           ;   ;   ;     ;     ;       ;    ;;;;    ;       ;     ;    ;;;;;;
;                    ;   ;   ;     ;     ;       ;   ;   ;    ;       ;    ;     ;     
;                    ;   ;   ;     ;     ;       ;   ;   ;    ;       ;   ;      ;     
;                  ;;;;; ;   ;   ;;;;;   ;;;   ;;;;;  ;;;;     ;;   ;;;;; ;;;;;   ;;;; 
;                                                                                      
;                                                                                      
;                                                                                      

;; initial placements

#; {InitialTile** = [Listof PlayerOnTile] s.t. distinct non-neigboring locs}
#; {PlayerOnTile  = [List Tile PlayerName PortIndex Index Index] s.t. constraints}

#; {InitialTile** -> Boolean : distinct non-neighboring locations}
(define (no-neighbors? tile-spec)
  (for/and ((t (in-list tile-spec)))
    (define loc (place-of t))
    (define all-but (remove loc tile-spec))
    (for/and ((n (apply neighbor-locations loc)))
      (not (member n all-but)))))

(define player-on-tile/c (make-placement/c at-periphery-facing-inward))
(define initial-player-on-tile*/c (make-list-of-places-ctc player-on-tile/c no-neighbors?))

;                                                                                                    
;                                                                      ;                             
;                    ;            ;                                    ;     ;            ;          
;                                 ;                                    ;                  ;          
;                  ;;;   ; ;;   ;;;;;   ;;;    ;;;; ;;;;;;   ;;;    ;;;;   ;;;   ;;;;   ;;;;;   ;;;  
;                    ;   ;;  ;    ;    ;;  ;   ;;  ;;  ;  ; ;;  ;  ;; ;;     ;       ;    ;    ;;  ; 
;                    ;   ;   ;    ;    ;   ;;  ;    ;  ;  ; ;   ;; ;   ;     ;       ;    ;    ;   ;;
;   ;;;;;;           ;   ;   ;    ;    ;;;;;;  ;    ;  ;  ; ;;;;;; ;   ;     ;    ;;;;    ;    ;;;;;;
;                    ;   ;   ;    ;    ;       ;    ;  ;  ; ;      ;   ;     ;   ;   ;    ;    ;     
;                    ;   ;   ;    ;    ;       ;    ;  ;  ; ;      ;; ;;     ;   ;   ;    ;    ;     
;                  ;;;;; ;   ;    ;;;   ;;;;   ;    ;  ;  ;  ;;;;   ;;;;   ;;;;;  ;;;;    ;;;   ;;;; 
;                                                                                                    
;                                                                                                    
;                                                                                                    

;; intermediate placements 

#; {Intermediate* = [Listof Intermediate]
                  s.t.
                  (1) distinct locs
                  (2) every location is
                  -- either occupied,
                  -- at the periphery, 
                  -- or has two neighbors}
#; {Intermediate  = (U TilePlacement
                       PlayerOnTile w/o perhiphery constraint)}
#; {TilePlacement = [List Tile Index Index]}

#; {Intermediate* -> Boolean}
(define (occupied-periphery-or-2-neighbors tile-spec)
  (for/and ((t (in-list tile-spec)))
    (or~ (> (length t) 3)
         (apply bordering-periphery? (place-of t))
         #:let x-y (place-of t)
         #:let locations (map place-of tile-spec)
         #:let all-but (remove x-y locations)
         #:let neigbors# (for/sum ((n (apply neighbor-locations x-y)) #:when (member n all-but)) 1)
         (> neigbors# 1))))

(define tile/c (list/c tile? index? index?))
(define player-on-any-tile/c (make-placement/c facing-inward))
(define either-or (or/c tile/c player-on-any-tile/c))
(define intermediate*/c (make-list-of-places-ctc either-or occupied-periphery-or-2-neighbors))

;                                                   
;                                                   
;                          ;             ;          
;                          ;             ;          
;                  ;;;   ;;;;;  ;;;;   ;;;;;   ;;;  
;                 ;   ;    ;        ;    ;    ;;  ; 
;                 ;        ;        ;    ;    ;   ;;
;   ;;;;;;         ;;;     ;     ;;;;    ;    ;;;;;;
;                     ;    ;    ;   ;    ;    ;     
;                 ;   ;    ;    ;   ;    ;    ;     
;                  ;;;     ;;;   ;;;;    ;;;   ;;;; 
;                                                   
;                                                   
;                                                   

#; { State -> Boolean : every player faces an open square }
;; for initialize and add-tile and intermediate 
(define (every-player-faces-an-open-square s)
  (match-define (state board players) s)
  (for/and ((p (in-set players)))
    (match-define  (player name port x y)      p)
    (define-values (x-look y-look) (looking-at port x y))
    (boolean? (matrix-ref board x-look y-look))))

#; {State -> Boolean : every player can go backwatds to an outside port}
;; for intermediate, though it also holds for initialize and add-tile
(define (every-player-can-leave-going-backwards s)
  (match-define (state board players) s)
  (for/and ((p (in-set players)))
    (match-define (player name port x y) p)
    (define tile (square-tile (matrix-ref board x y)))
    (or~ #:let exit-port (tile port)
         (and (bordering-periphery? x y) (player-facing-inward? port x y))
         #:let player-moved-to-exit-port (player name (tile port) x y)
         (out? (move-one-player board player-moved-to-exit-port #:to-periphery? #t)))))

;; for tests of contracts, see below data examples 


;                                                   
;                                                   
;                                        ;          
;                                        ;          
;    ;;;   ;   ;  ;;;;    ;;;    ;;;;  ;;;;;   ;;;  
;   ;;  ;   ; ;   ;; ;;  ;; ;;   ;;  ;   ;    ;   ; 
;   ;   ;;  ;;;   ;   ;  ;   ;   ;       ;    ;     
;   ;;;;;;   ;    ;   ;  ;   ;   ;       ;     ;;;  
;   ;       ;;;   ;   ;  ;   ;   ;       ;        ; 
;   ;       ; ;   ;; ;;  ;; ;;   ;       ;    ;   ; 
;    ;;;;  ;   ;  ;;;;    ;;;    ;       ;;;   ;;;  
;                 ;                                 
;                 ;                                 
;                 ;

#; { State -> (-> PlayerName Boolean : the player is on the list of players)}
;; adding a tile to a properly built board
(define ((takes-part-in-game s) pname)
  (player? (find-player (state-players s) pname)))

(provide
 ;; type State
 ;; all players are on ports that face empty squares on the board 
 state?

 ;; type Player
 #; [name #;string? port #;port? x #;index? y #;index?]
 (struct-out player)

 (contract-out 
  [initialize
   ;; creates a state from a list of initial placements 
   (-> initial-player-on-tile*/c (and/c state? every-player-faces-an-open-square))]
  
  [exn:infinite? (-> any/c boolean?)]
  [add-tile
   ;; place a configured tile on the empty square that the player pn neighbors
   ;; EFFECT may raise (exn:infinite String CMS Player) to signal an infinite loop
   (->i ([s state?][c tile?][name (s) (and/c string? (takes-part-in-game s))])
        [result (and/c state? every-player-faces-an-open-square)])]
  
  [survivors (-> state? (listof string?))]

  [intermediate
   (-> intermediate*/c
       (or/c #false 
             (and/c state?
                    #; "and it also satisfies"
                    #; every-player-faces-an-open-square
                    #; every-player-can-leave-going-backwards)))]))

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

(require (except-in Tsuro/Code/Common/square SIZE looking-at square-tile))
(require (except-in Tsuro/Code/Common/tiles tile? table))
(require (except-in Tsuro/Code/Common/port-alphabetic port?))
(require Tsuro/Code/Common/matrix)
(require Tsuro/Code/Lib/should-be-racket)
(require pict)

(require Tsuro/Code/Lib/spy)

(module+ test
  (require (submod ".."))
  (require (for-syntax syntax/parse))
  (require rackunit))

(module+ json
  (require (submod Tsuro/Code/Common/tiles json))
  (require rackunit))

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

;; data representation of the game state: the board, the players  

;; the game state itself 
;; ------------------------------------------------------------------
#; {State   = (state Board Player*)}
#; {Player* = [Setof Player]}
#; {Player  = (player PlayerName p x y)}
#; {PlayerName = String}
#; {Board   = [Matrixof Square] :: {Index x Index}}
#; {Index   = [0 .. SIZE]}

(struct state  [board players] #:transparent)
(struct player [name port x y] #:transparent)

(define (survivors s)
  (map player-name (set->list (state-players s))))

;                                                                                             
;       ;                                                                                     
;       ;           ;                                                     ;;;                 
;       ;           ;                                                       ;                 
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;   ;   ;  ;;;;  ;;;;;;  ;;;;     ;     ;;;    ;;;  
;   ;; ;;      ;    ;        ;         ;;  ;   ; ;       ; ;  ;  ; ;; ;;    ;    ;;  ;  ;   ; 
;   ;   ;      ;    ;        ;         ;   ;;  ;;;       ; ;  ;  ; ;   ;    ;    ;   ;; ;     
;   ;   ;   ;;;;    ;     ;;;;         ;;;;;;   ;     ;;;; ;  ;  ; ;   ;    ;    ;;;;;;  ;;;  
;   ;   ;  ;   ;    ;    ;   ;         ;       ;;;   ;   ; ;  ;  ; ;   ;    ;    ;          ; 
;   ;; ;;  ;   ;    ;    ;   ;         ;       ; ;   ;   ; ;  ;  ; ;; ;;    ;    ;      ;   ; 
;    ;;;;   ;;;;    ;;;   ;;;;          ;;;;  ;   ;   ;;;; ;  ;  ; ;;;;      ;;   ;;;;   ;;;  
;                                                                  ;                          
;                                                                  ;                          
;                                                                  ;                          

(define the-empty-board (build-matrix SIZE SIZE (λ (_i _j) BLANK)))

(match-define `(,port-2 ,port-3 ,port-4) (map index->port '(2 3 4)))

(define player-red   "red")
(define player-blue  "blue")
(define player-white "white")

(define tile-00-index 34)
(define tile-02-index 33)
(define tile-20-index 34)

(define tile-00 (tile-index->tile 34))
(define tile-02 (tile-index->tile 33))
(define tile-20 (rotate-tile (tile-index->tile 34)))

(define inits-for-state-with-3-players
  `((,tile-00 ,player-red   ,port-2 0 0)
    (,tile-20 ,player-blue  ,port-4 2 0)
    (,tile-02 ,player-white ,port-3 0 2)))

(define 3players-list (map (λ (init) (apply player (rest init))) inits-for-state-with-3-players))
(define red-player (first 3players-list))
(define 3players (apply set 3players-list))

;; this is a function because it uses create-portmap explicitly 
(define (state-with-3-players #:with (with #false))
  (define square-00 (create-square '() tile-00 0 0))
  (define square-02 (create-square '() tile-02 0 2))
  (define square-20 (create-square '() tile-20 2 0))
  (define boatd3
    (let* ([m the-empty-board]
           [m (matrix-set m 0 0 square-00)]
           [m (matrix-set m 0 2 square-02)]
           [m (matrix-set m 2 0 square-20)])
      m))
  (define the-board (state boatd3 3players))
  (if with
      (values the-board square-00 square-02 square-20)
      the-board))

(module+ test ;; additional data samples
  (define-values (state-3-players square-00 square-02 square-20) (state-with-3-players #:with #true))
  (define board-3-players (state-board state-3-players))
  (define tile-to-add-to-board-3-index 33)
  (define tile-to-add-to-board-3 (tile-index->tile tile-to-add-to-board-3-index))
  (define placement1 (first inits-for-state-with-3-players))

  (check-equal? (survivors state-3-players) (set-map 3players player-name) "survivors"))

(module+ test ;; contracts for initial placements 
  (check-true (player-on-tile/c placement1))
  (check-true (initial-player-on-tile*/c inits-for-state-with-3-players))
  (check-true ((takes-part-in-game state-3-players) player-red))

  (check-true (intermediate*/c inits-for-state-with-3-players)
              "an initial configuration of tiles is also intermediate"))

;                                                          
;                                          ;               
;     ;                    ;               ;         ;;;   
;     ;                    ;               ;           ;   
;   ;;;;;   ;;;    ;;;   ;;;;;          ;;;;   ;;;     ;   
;     ;    ;;  ;  ;   ;    ;           ;; ;;  ;   ;    ;   
;     ;    ;   ;; ;        ;           ;   ;  ;        ;   
;     ;    ;;;;;;  ;;;     ;           ;   ;   ;;;     ;   
;     ;    ;          ;    ;           ;   ;      ;    ;   
;     ;    ;      ;   ;    ;           ;; ;;  ;   ;    ;   
;     ;;;   ;;;;   ;;;     ;;;          ;;;;   ;;;      ;; 
;                                                          
;                                                          
;                                                          

(module+ test
  ;; SYNTAX

  #; (state-from #:board0 board0 #:players0 [p ...] S ...)
  
  #; (init-list-from-tiles (S ...) ...)
  #; (S =  #f
        || I
        || (I PlayerName #:on Port)
        || (I #:rotate (0,90,180,270))
        || (I #:rotate (0,90,180,270) PlayerName #:on Port))
  #; {I = 0 .. TILES#}
  ;; creates a list of tile placements from which initialze and intermediate create a board 
  
  ;; -------------------------------------------------------------------------------------------------
  (begin-for-syntax
    (define-syntax-class degree [pattern 90][pattern 180][pattern 270])
    
    (define-syntax-class index/or-index-w-player
      (pattern (~datum #f)
               #:with square #'#f)
      (pattern
       (index (~optional (~seq #:rotate r:degree) #:defaults ([r #'0])) (~optional (~seq n #:on p)))
       #:declare n     (expr/c #'string?)
       #:declare p     (expr/c #'port?)
       #:declare index (expr/c #'(</c TILES#))
       #:with tile   #'(rotate-tile (tile-index->tile index.c) #:degree r)
       #:with square #'`(~? (,tile ,n.c ,p) (,tile)))
      (pattern index
               #:declare index (expr/c #'(</c TILES#))
               #:with tile   #'(tile-index->tile index.c)
               #:with square #'`(,tile)))

    (define-syntax-class square-spec
      [pattern (ti (~optional (~seq #:rotate r:degree) #:defaults ([r #'0])) xi yi)
               #:declare ti (expr/c #'(</c TILES#))
               #:declare xi (expr/c #'index?)
               #:declare yi (expr/c #'index?)
               #:with (tile x y) #'( (rotate-tile (tile-index->tile ti.c) #:degree r) xi.c yi.c)]))

  (define-syntax (init-list-from-tiles stx)
    (syntax-parse stx
      [(_ (t:index/or-index-w-player ...) ...)
       #'(init-list-from-tiles/proc (list (list t.square ...) ...))]))
 
  (define (init-list-from-tiles/proc rectangle)
    (define (f row i)
      (for/list ((cell (in-list row)) (j (in-naturals)) #:when cell)
        (append cell (list j i))))
    (apply append (for/list ((row (in-list rectangle)) (i (in-naturals))) (f row i))))

  (define-syntax (state-from stx)
    (syntax-parse stx
      [(_ (~optional (~seq #:board0 board0))
          (~optional (~seq #:players0 [p ...]))
          (t:index/or-index-w-player ...) ...)
       #:declare p (expr/c #'player?)
       #'(sft/proc (~? board0 the-empty-board) (~? (set p.c ...) (set)) `((,t.square ...) ...))]))

  (define (sft/proc board0 players0 rectangle)
    (define init-list (init-list-from-tiles/proc rectangle))
    (define-values (players board)
      (for/fold ((players players0) (board board0)) ((t init-list))
        (match t
          [`(,tile ,x ,y) (values players (add-new-square-update-neighbors board tile x y))]
          [`(,tile ,name ,port ,x ,y)
           (define new-player (player name port x y))
           (values (set-add players new-player) (add-new-square-update-neighbors board tile x y))])))
    (state board players)))

(module+ test ;; testing the DSL
  (check-equal? (init-list-from-tiles
                 ((34 "red" #:on port-2) #f (34 #:rotate 90 "blue" #:on port-4))
                 (#f)
                 ((33 "white" #:on port-3)))
                inits-for-state-with-3-players)

  (match-define (state board3 _) state-3-players)
  
  (check-equal? (state-from
                 [[tile-00-index "red" #:on port-2] #f [tile-20-index "blue" #:on port-4]]
                 (#f)
                 [[tile-02-index #:rotate 90 "white" #:on port-3]])
                state-3-players))

(module+ test ;; intermediate boards, states, and contracts 

  (define not-intermediate
    (init-list-from-tiles
     ((34 "red" #:on port-2) #f (34 #:rotate 90 "blue" #:on port-4))
     (33 33)
     ((33 #:rotate 180 "white" #:on port-3))))
  (check-false (intermediate*/c not-intermediate) "an isolated tile that nobody could have placed")
  
  (define tile-01 (tile-index->tile 33))
  (define tile-10 (tile-index->tile 33))
  (define contiguity-for-3-platers `((,tile-01 0 1) (,tile-10 1 0)))
  (define intermediate-bad-state (append inits-for-state-with-3-players contiguity-for-3-platers))

  ;; contracts for intermediate placements
  (check-true (andmap tile/c contiguity-for-3-platers) "tiles are tiles")
  (check-true (player-on-any-tile/c (first intermediate-bad-state)) "1 p")
  (check-false (tile/c (first intermediate-bad-state)) "1 t")
  (check-true ((or/c player-on-any-tile/c tile/c) (first intermediate-bad-state)) "1 or")
  (check-true ((or/c tile/c player-on-any-tile/c) (first intermediate-bad-state)) "reversed or")
  (check-true (intermediate*/c intermediate-bad-state))
  
  (check-false (every-player-faces-an-open-square     (intermediate-aux intermediate-bad-state)))
  (check-true (every-player-can-leave-going-backwards (intermediate-aux intermediate-bad-state))))

;                                                                        
;                                                                        
;      ;             ;     ;       ;          ;;;       ;                
;                          ;                    ;                        
;    ;;;   ; ;;    ;;;   ;;;;;   ;;;   ;;;;     ;     ;;;   ;;;;;   ;;;  
;      ;   ;;  ;     ;     ;       ;       ;    ;       ;       ;  ;;  ; 
;      ;   ;   ;     ;     ;       ;       ;    ;       ;      ;   ;   ;;
;      ;   ;   ;     ;     ;       ;    ;;;;    ;       ;     ;    ;;;;;;
;      ;   ;   ;     ;     ;       ;   ;   ;    ;       ;    ;     ;     
;      ;   ;   ;     ;     ;       ;   ;   ;    ;       ;   ;      ;     
;    ;;;;; ;   ;   ;;;;;   ;;;   ;;;;;  ;;;;     ;;   ;;;;; ;;;;;   ;;;; 
;                                                                        
;                                                                        
;                                                                        

(define (initialize lo-placements)
  (define players
    (for/set ([p (in-list lo-placements)])
      (apply player (rest p))))
  (define board
    (for/fold ((m the-empty-board)) ((placement (in-list lo-placements)))
      (match-define `(,tile ,_  ,_ ,x ,y) placement)
      (matrix-set m x y (create-square '() tile x y))))
  (state board players))

(module+ test ;; initialize 
  (check-exn exn:fail:contract? (λ () (initialize `((,tile1 "x" 2 0 0)))) "port, not index")
  (check-equal? (initialize inits-for-state-with-3-players) state-3-players))

;                                                                                      
;                                                        ;                             
;      ;            ;                                    ;     ;            ;          
;                   ;                                    ;                  ;          
;    ;;;   ; ;;   ;;;;;   ;;;    ;;;; ;;;;;;   ;;;    ;;;;   ;;;   ;;;;   ;;;;;   ;;;  
;      ;   ;;  ;    ;    ;;  ;   ;;  ;;  ;  ; ;;  ;  ;; ;;     ;       ;    ;    ;;  ; 
;      ;   ;   ;    ;    ;   ;;  ;    ;  ;  ; ;   ;; ;   ;     ;       ;    ;    ;   ;;
;      ;   ;   ;    ;    ;;;;;;  ;    ;  ;  ; ;;;;;; ;   ;     ;    ;;;;    ;    ;;;;;;
;      ;   ;   ;    ;    ;       ;    ;  ;  ; ;      ;   ;     ;   ;   ;    ;    ;     
;      ;   ;   ;    ;    ;       ;    ;  ;  ; ;      ;; ;;     ;   ;   ;    ;    ;     
;    ;;;;; ;   ;    ;;;   ;;;;   ;    ;  ;  ;  ;;;;   ;;;;   ;;;;;  ;;;;    ;;;   ;;;; 
;                                                                                      
;                                                                                      
;                                                                                      

(define (intermediate l-intermediate)
  (define s (intermediate-aux l-intermediate))
  (and (every-player-faces-an-open-square s)
       (every-player-can-leave-going-backwards s)
       s))

#; {Intermediate* -> State}
;; produces the state as specified without checking whether it is valid 
(define (intermediate-aux l-intermediate)
  (define board0 the-empty-board)
  (define-values (board players)
    (for/fold ((board board0) (players (set))) ([p l-intermediate])
      (match p
        [(list tile x y)
         (values (add-new-square-update-neighbors board tile x y)
                 players)]
        [(list tile name port x y)
         (values (add-new-square-update-neighbors board tile x y)
                 (set-add players (player name port x y)))])))
  (state board players))

(module+ test 
  (define intermediate-bad-state-2
    (init-list-from-tiles
     ((34 "red" #:on port-2) #f (34 #:rotate 90 "blue" #:on port-4))
     (33)
     (33 (33 #:rotate 180 "white" #:on port-3))))

  (check-true (every-player-faces-an-open-square       (intermediate-aux intermediate-bad-state-2)))
  (check-false (every-player-can-leave-going-backwards (intermediate-aux intermediate-bad-state-2)))

  (define intermediate-good
    (init-list-from-tiles
     ((34 "red" #:on port-2) #f (34 #:rotate 90 "blue" #:on port-4))
     (33)
     ((33 #:rotate 180 "white" #:on port-3))))
  
  (check-true (every-player-faces-an-open-square      (intermediate-aux intermediate-good)))
  (check-true (every-player-can-leave-going-backwards (intermediate-aux intermediate-good)))

  (define intermediate-good-state
    (state-from ((34 "red" #:on port-2) #f (34 #:rotate 90 "blue" #:on port-4))
                (33)
                ((33 #:rotate 180 "white" #:on port-3))))
  
  (check-equal? (intermediate intermediate-good) intermediate-good-state))

;                                                                        
;              ;      ;                                                  
;              ;      ;                         ;       ;   ;;;          
;              ;      ;                         ;             ;          
;   ;;;;    ;;;;   ;;;;         ;;;;          ;;;;;   ;;;     ;     ;;;  
;       ;  ;; ;;  ;; ;;             ;           ;       ;     ;    ;;  ; 
;       ;  ;   ;  ;   ;             ;           ;       ;     ;    ;   ;;
;    ;;;;  ;   ;  ;   ;          ;;;;           ;       ;     ;    ;;;;;;
;   ;   ;  ;   ;  ;   ;         ;   ;           ;       ;     ;    ;     
;   ;   ;  ;; ;;  ;; ;;         ;   ;           ;       ;     ;    ;     
;    ;;;;   ;;;;   ;;;;          ;;;;           ;;;   ;;;;;    ;;   ;;;; 
;                                                                        
;                                                                        
;                                                                        

(struct exn:infinite exn (player))

(define (add-tile state0 tile player-name)
  (match-define  (state board players) state0)
  (match-define  (player _ port x y) (find-player players player-name))
  (define-values (x-new y-new) (looking-at port x y))
  (define nu-board (add-new-square-update-neighbors board tile x-new y-new))
  (define-values (moved out* inf*) (move-players nu-board players x-new y-new))
  (when (cons? inf*) (raise (exn:infinite "hello" (current-continuation-marks) (first inf*))))
  ;; what to do with eliminated ones
  ;; (displayln `(player was thrown out ,out*))
  (state nu-board moved))

#; {(Setof Player) PlayerName -> Player}
(define (find-player players p)
  (define F (finder p))
  (for/first ((element (in-set players)) #:when (F element)) element))
  
#; {PlayerName -> (Player -> Boolean)}
(define (finder pn)
  (compose (curry equal? pn) player-name))

(module+ test ;; add-tile
  (check-equal? (find-player 3players "red") red-player)
  (match-define (list p1 p2) (set->list (set-remove (state-players state-3-players) red-player)))
  (define state+
    (state-from #:board0 board-3-players
                #:players0 [p2 p1]
                (#f (tile-to-add-to-board-3-index #:rotate 90))))

  (define board+ (state-board state+))
  
  (check-equal? (add-tile state-3-players (rotate-tile tile-to-add-to-board-3) player-red) state+
                "drive red player off"))

(module+ test ;; add-tile that causes an infinite loop 

  (define inf-tile-to-add-to-board-3 tile1)
  (check-exn exn:infinite? (λ () (add-tile state-3-players inf-tile-to-add-to-board-3 player-red))
             "drive red player into infinite loop"))

;                                                                                      
;                     ;                              ;                               ; 
;                     ;           ;                  ;                               ; 
;                     ;           ;                  ;                               ; 
;   ;   ;  ;;;;    ;;;;  ;;;;   ;;;;;   ;;;          ;;;;    ;;;   ;;;;    ;;;;   ;;;; 
;   ;   ;  ;; ;;  ;; ;;      ;    ;    ;;  ;         ;; ;;  ;; ;;      ;   ;;  ; ;; ;; 
;   ;   ;  ;   ;  ;   ;      ;    ;    ;   ;;        ;   ;  ;   ;      ;   ;     ;   ; 
;   ;   ;  ;   ;  ;   ;   ;;;;    ;    ;;;;;;        ;   ;  ;   ;   ;;;;   ;     ;   ; 
;   ;   ;  ;   ;  ;   ;  ;   ;    ;    ;             ;   ;  ;   ;  ;   ;   ;     ;   ; 
;   ;   ;  ;; ;;  ;; ;;  ;   ;    ;    ;             ;; ;;  ;; ;;  ;   ;   ;     ;; ;; 
;    ;;;;  ;;;;    ;;;;   ;;;;    ;;;   ;;;;         ;;;;    ;;;    ;;;;   ;      ;;;; 
;          ;                                                                           
;          ;                                                                           
;          ;                                                                           

#; {Board Confguration Index Index -> Matrix}

(define (add-new-square-update-neighbors board tile x y)
  (define neighbors  (neighbors* board x y))
  (define new-square (create-square neighbors tile x y))
  (for*/fold ((m (matrix-set board x y new-square))) ((g neighbors))
    (match-define (list x-n y-n) g)
    (define old-neighbor-square  (matrix-ref m x-n y-n))
    (define updated-neighbor-sq  (update-square old-neighbor-square x-n y-n x y))
    (matrix-set m x-n y-n updated-neighbor-sq)))

(module+ test
  (define nu-square (create-square (neighbors* board-3-players 1 0) tile-to-add-to-board-3 1 0))
  (define nu-board  (let* ([m board-3-players]
                           [m (matrix-set m 1 0 nu-square)]
                           [m (matrix-set m 0 0 (update-square square-00 0 0 1 0))]
                           [m (matrix-set m 2 0 (update-square square-20 2 0 1 0))])
                      m))
    
  (check-equal?
   (matrix->rectangle 
    (add-new-square-update-neighbors board-3-players tile-to-add-to-board-3 1 0))
   (matrix->rectangle 
    nu-board)))
;                                                                                                    
;                                                                                                    
;                           ;                               ;;;                                      
;                                                             ;                                      
;  ;;;;;;   ;;;   ;   ;   ;;;   ; ;;    ;;;;         ;;;;     ;    ;;;;   ;   ;   ;;;    ;;;;   ;;;  
;  ;  ;  ; ;; ;;  ;   ;     ;   ;;  ;  ;;  ;         ;; ;;    ;        ;  ;   ;  ;;  ;   ;;  ; ;   ; 
;  ;  ;  ; ;   ;   ; ;      ;   ;   ;  ;   ;         ;   ;    ;        ;   ; ;   ;   ;;  ;     ;     
;  ;  ;  ; ;   ;   ; ;      ;   ;   ;  ;   ;         ;   ;    ;     ;;;;   ; ;   ;;;;;;  ;      ;;;  
;  ;  ;  ; ;   ;   ; ;      ;   ;   ;  ;   ;         ;   ;    ;    ;   ;   ; ;   ;       ;         ; 
;  ;  ;  ; ;; ;;    ;       ;   ;   ;  ;; ;;         ;; ;;    ;    ;   ;   ;;    ;       ;     ;   ; 
;  ;  ;  ;  ;;;     ;     ;;;;; ;   ;   ;;;;         ;;;;      ;;   ;;;;    ;     ;;;;   ;      ;;;  
;                                          ;         ;                      ;                        
;                                       ;  ;         ;                     ;                         
;                                        ;;          ;                    ;;                         

#; {Board Player* Index Index -> (values Player* [Setof Player])}

;; move players facing (x,y), detrmine survivors, return those as the first list;
;; the second list are the drop-outs that run into walls

(define (move-players board players x y)
  (define-values (moved out inf)
    (for/fold ((moved (set)) (out '()) (inf '())) ((p (in-set players)))
      (match-define  (player name port x-p y-p) p)
      (define-values (x-at y-at) (looking-at port x-p y-p))
      (cond
        [(and (= x-at x) (= y-at y))
         (define p-moved (move-one-player board p))
         (cond
           [(out? p-moved) (values moved (cons p-moved out) inf)]
           [(inf? p-moved) (values moved out (cons p-moved inf))]
           [else (values (set-add moved p-moved) out inf)])]
        [else (values (set-add moved p) out inf)])))
  (values moved out inf))

(struct out [player] #:transparent)
(struct inf [player] #:transparent)

#; {Board Player [#:to-periphery Boolean] -> (U Player (out Player) (inf Player))}
(define (move-one-player board the-player #:to-periphery? (to-periphery #f))
  ;; start player on (port-p, x-p, y-p) that look at an occupied neighboring square
  (define is-at-periphery? (if to-periphery bordering-periphery? (λ (x y) #f)))
  (match-define (player name port-p x-p y-p) the-player)
  (let/ec return 
    (let move-one-player ([port-p port-p][x-p x-p][y-p y-p][seen `((,x-p ,y-p))])
      (when (and to-periphery (member ((matrix-ref board x-p y-p) (port->index port-p)) (list WALL OPEN)))
          ;; the player didn't get to the periphery but an open square 
          (return the-player))
      (match-define (list port x y external) (move-one-square board port-p x-p y-p))
      (cond
        [(member `(,x ,y) seen) (inf (player name port x y))]
        [(equal? WALL external) (out (player name port x y))]
        [(equal? OPEN external) (player name port x y)]
        [(is-at-periphery? x y) (out (player name port x y))]
        [else (move-one-player port x y (cons `(,x ,y) seen))]))))

#; {Board Port Index Index -> [List Index Index Next]}
;; move player at (x-p, y-p) on port port-p "thru" the tile of the next square
;; ASSUME the square is occupired with a tile 
(define (move-one-square board port x-p y-p)
  (define next ((matrix-ref board x-p y-p) (port->index port)))
  (define next-square (matrix-ref board (next-x next) (next-y next)))
  (define port-out    ((square-tile next-square) (next-port next)))
  (define external    (next-square (port->index port-out)))
  (list port-out (next-x next) (next-y next) external))

(module+ test ;; move player  
  (check-equal? (move-one-square board+ (player-port red-player) 0 0) (list (index->port 1) 1 0 WALL)
                "moved red player 1 step")

  (check-equal? (move-one-player board+ red-player) (out (player player-red (index->port 1) 1 0))
                "move red player out")
  
  (match-define (state board-good-move _)
    (state-from [(34 "red" #:on port-2) 33 (34 #:rotate 90 "blue" #:on port-4)]
                [33]
                [(33 #:rotate 180 "white" #:on port-3)]))

  (check-equal? (move-one-player board-good-move (player "red" port-2 0 0))
                (player player-red (index->port 5) 1 0)
                "move red player to internal square")

  (match-define (state board-inf _)
    (state-from [(34 "red" #:on port-2) 34 (34 #:rotate 90 "blue" #:on port-4)]
                [33]
                [(33 #:rotate 180 "white" #:on port-3)]))
  (define red-player-inf (player "red" port-2 0 0))
  
  (check-equal? (move-one-player board-inf red-player-inf) (inf red-player-inf) "move player inf"))

;                                                                 
;                               ;      ;                          
;                    ;          ;      ;                          
;                               ;      ;                          
;   ; ;;    ;;;    ;;;    ;;;;  ; ;;   ;;;;    ;;;    ;;;;   ;;;  
;   ;;  ;  ;;  ;     ;   ;;  ;  ;;  ;  ;; ;;  ;; ;;   ;;  ; ;   ; 
;   ;   ;  ;   ;;    ;   ;   ;  ;   ;  ;   ;  ;   ;   ;     ;     
;   ;   ;  ;;;;;;    ;   ;   ;  ;   ;  ;   ;  ;   ;   ;      ;;;  
;   ;   ;  ;         ;   ;   ;  ;   ;  ;   ;  ;   ;   ;         ; 
;   ;   ;  ;         ;   ;; ;;  ;   ;  ;; ;;  ;; ;;   ;     ;   ; 
;   ;   ;   ;;;;   ;;;;;  ;;;;  ;   ;  ;;;;    ;;;    ;      ;;;  
;                            ;                                    
;                         ;  ;                                    
;                          ;;                                     

#; {Board Index Index -> [Listof Location]}
;; determine (occupied) neighbors of the square at (x,y)
(define (neighbors* board x y)
  (filter (match-lambda [`(,x ,y) (matrix-ref board x y)]) (neighbor-locations x y)))

#; {Index Index -> [Listof Location]}
(define (neighbor-locations x y)
  (define all `((,x ,(- y 1)) (,x ,(+ y 1)) (,(- x 1) ,y) (,(+ x 1) ,y)))
  (filter (match-lambda [`(,x ,y) (and (index? x) (index? y))]) all))

;                                     
;                                     
;             ;            ;          
;                          ;          
;   ;;;;    ;;;    ;;;   ;;;;;   ;;;  
;   ;; ;;     ;   ;;  ;    ;    ;   ; 
;   ;   ;     ;   ;        ;    ;     
;   ;   ;     ;   ;        ;     ;;;  
;   ;   ;     ;   ;        ;        ; 
;   ;; ;;     ;   ;;       ;    ;   ; 
;   ;;;;    ;;;;;  ;;;;    ;;;   ;;;  
;   ;                                 
;   ;                                 
;   ;                                 

(define PLAYER-SIZE (quotient TILE-SIZE 5)) 

(define INSET  (+ 20 TILE-SIZE))
(define WIDTH  (+ INSET (* 10 TILE-SIZE) INSET))
(define HEIGHT (+ INSET (* 10 TILE-SIZE) INSET))

#; {State (Instanceof DC<%>) -> Pict}
(define (draw-state b dc)
  (match-define (state squares players) b)
  (define board-as-pict
    (let loop ([l (matrix->rectangle squares)][y 0])
      (cond
        [(empty? l) (blank)]
        [else
         (define row (first l))
         (define picts
           (for/list ((n (in-list row)) (x (in-naturals)))
             (define tile     (or (and (not (equal? BLANK n)) (square-tile n)) blank-tile))
             (define pict     (tile->pict tile))
             (define p-on-x-y (is-player-on players x y))
             (cond
               [(boolean? p-on-x-y) pict]
               [else
                (define color (player-name p-on-x-y))
                (define port  (player-port p-on-x-y))
                (add-player pict (jack-o-lantern PLAYER-SIZE color) port)])))
         (vl-append (apply hc-append picts) (loop (rest l) (+ y 1)))])))
  (draw-pict board-as-pict dc INSET INSET))

#; {Player* Natural Natural -> (U False Player)}
(define (is-player-on players x y)
  (define p (set-member players (λ (p) (match-define (player _ _ x0 y0) p) (and (= x x0) (= y y0)))))
  (if (set-empty? p) #f (set-first p)))

#; {PortIndex Natural Natural -> (values Natural Natural)}
(define (logical-coordinates->geometry port x y)
  (values (* x TILE-SIZE)) (* TILE-SIZE y))

#; {(-> State) -> Void}
(define (show-state s)
  (define frame (new frame% [label "hello"][width WIDTH][height HEIGHT]))
  
  (define canvas
    (new canvas%
         [parent frame]
         [paint-callback (λ (e dc) (draw-state (s) dc))]))
    
  (send frame show #t))

; (module+ test (show-state state-with-3-players))
; (module+ test (show-state (λ () state+)))
; (module+ test (show-state (λ () intermediate-good-state)))
; (module+ test (show-state (λ () state+)))


;                              
;      ;                       
;                              
;                              
;    ;;;    ;;;    ;;;   ; ;;  
;      ;   ;   ;  ;; ;;  ;;  ; 
;      ;   ;      ;   ;  ;   ; 
;      ;    ;;;   ;   ;  ;   ; 
;      ;       ;  ;   ;  ;   ; 
;      ;   ;   ;  ;; ;;  ;   ; 
;      ;    ;;;    ;;;   ;   ; 
;      ;                       
;      ;                       
;    ;;

(module+ json
  (define (square->jsexpr s x y)
    (list (tile->jsexpr (square-tile s)) x y))

  (define (state->jsexpr s)
    (define players (state-players s))
    (matrix-where (state-board s)
                  (λ (sq x y) sq)
                  (λ (sq x y)
                    (define tj (tile->jsexpr (square-tile sq)))
                    (match (is-player-on players x y)
                      [(? boolean?) (list tj x y)]
                      [(player name port _ _) (list tj name port x y)]))))

  (define (jsexpr->state sj)
    (define intermediates 
      (match sj
        [(list i ...) (for/list ((i sj)) (cons (jsexpr->tile (first i)) (rest i)))]))
    (cond
      [(intermediate*/c intermediates)
       (define candidate-state (intermediate intermediates))
       (and candidate-state)]
      [else intermediates]))
  
  (define s3 (state-with-3-players))
  (check-equal? (state-players (jsexpr->state (state->jsexpr s3))) (state-players s3))
  (check-equal? (state-board (jsexpr->state (state->jsexpr s3))) (state-board s3)))