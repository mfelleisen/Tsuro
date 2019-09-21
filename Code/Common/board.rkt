#lang racket/gui

;; a data representation for game States, plus basic functions for manipulating them

;; TDOO:
;; -- can init and intermediate tiles accept rotated tiles? 


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

(require (only-in Tsuro/Code/Common/grid index? SIZE looking-at square-tile))
(require (only-in Tsuro/Code/Common/tiles tile?))
(require (only-in Tsuro/Code/Common/port port?))
(require Tsuro/Code/Common/tokens)
(require SwDev/Lib/or)

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

#; {type Location = [List Index Index]}

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
   [t tile?] [n color?] [p port?] [x index?] [y index?]
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
  (define tile-locs (map place-of tile-spec))
  (for/and ((loc (in-list tile-locs)))
    (define all-but (remove loc tile-locs))
    (for/and ((n (apply neighbor-locations loc)))
      (not (member n all-but)))))

(module+ test
  (define (inits0 x) `[ [,(tile-index->tile 1) "white" 4 0 0] [,(tile-index->tile 1) "red" 2 ,x 0] ])
  (check-true (no-neighbors? (inits0 2)) "2 leaves one empty square")
  (check-false (no-neighbors? (inits0 1)) "1 means there are neigbors"))

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
  (match-define (state grid players) s)
  (for/and ((p (in-set players)))
    (match-define (player name port x y) p)
    (define-values (x-look y-look) (looking-at port x y))
    (boolean? (matrix-ref grid x-look y-look))))

#; {State -> Boolean : every player can go backwatds to an outside port}
;; for intermediate, though it also holds for initialize and add-tile
(define (every-player-can-leave-going-backwards s)
  (match-define (state grid players) s)
  (for/and ((p (in-set players)))
    (match-define (player name port x y) p)
    (define tile (square-tile (matrix-ref grid x y)))
    (or~ #:let exit-port (tile port)
         (and (bordering-periphery? x y) (player-facing-inward? port x y))
         #:let player-moved-to-exit-port (player name (tile port) x y)
         (out? (move-one-player grid player-moved-to-exit-port #:to-periphery? #t)))))

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

(define spot/c (list/c port? index? index?))

(provide
 ;; type State
 ;; all players are on ports that face empty squares on the grid 
 state?

 ;; type Player
 #; [name #;color? port #;port? x #;index? y #;index?]
 (struct-out player)

 player-on-tile/c
 initial-player-on-tile*/c
 intermediate*/c
  
 (contract-out 
  [initialize
   ;; creates a state from a list of initial placements 
   (-> initial-player-on-tile*/c (and/c state? every-player-faces-an-open-square))]

  [find-first-free-spot
   ;; search in clock-wise fashion starting from (0,0), a first square w/o neighbors at the periphery
   ;; search in clock-wise fashion from the left port on the NORTH side that faces inward 
   (-> state? spot/c)]
  
  [exn:infinite? (-> any/c boolean?)]
  [add-tile
   ;; place a configured tile on the empty square that the player pn neighbors
   ;; EFFECT may raise (exn:infinite String CMS Player) to signal an infinite loop
   (->i ([s state?][name (s) (and/c color? (curry set-member? (survivors state3)))][t tile?])
        [result (and/c state? every-player-faces-an-open-square)])]
  
  [survivors (-> state? (listof color?))]

  [intermediate
   (-> intermediate*/c
       (or/c #false 
             (and/c state?
                    #; "and it also satisfies"
                    #; every-player-faces-an-open-square
                    #; every-player-can-leave-going-backwards)))]))

(module+ json
  (provide
   state->jsexpr

   ;; if JSexpr matches state-pat, it is a candidate for the creation of an intermediate board
   state-pat

   ;; if JSExpr matches action-pat, it is a candidate for a add-tile action 
   action-pat

   #; {JSexpr -> (U State #false)}
   ;; it produces #false if intermediates produces #false because it's an illegal state
   (contract-out
    [jsexpr->state (-> (λ (x) (match x [state-pat #t][_ #f])) (or/c state? #false))])

   ;; States as JSexpr 
   state3-jsexpr
   good-intermediate-state-jsexpr
   good-state-actions
   good-intermediate-state+++-jsexpr

   ;; Intermediates as JSexpr
   bad-intermediate-spec-jsexpr
   bad-intermediate-spec-2-jsexpr

   ;; actions on states 
   state3-action
   state3-action-infinite
   state+-jsexpr
   ))

(module+ picts
  (provide
   state->pict
   show-state))

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

(require (except-in Tsuro/Code/Common/grid SIZE looking-at square-tile))
(require (except-in Tsuro/Code/Common/tiles tile?))
(require (except-in Tsuro/Code/Common/port port?))
(require Tsuro/Code/Common/matrix)

(require SwDev/Lib/should-be-racket)
(require pict)
(require (for-syntax syntax/parse))

(require SwDev/Debugging/spy)
(require SwDev/Debugging/diff)

(module+ test
  (require (submod ".."))
  (require rackunit))

(module+ json
  (require (submod Tsuro/Code/Common/tiles json))
  (require SwDev/Lib/pattern-matching)
  (require rackunit))

(module+ picts
  (require (submod Tsuro/Code/Common/tiles picts))
  (require (submod Tsuro/Code/Common/grid picts)))

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

;; data representation of the game state: the grid, the players  

;; the game state itself 
;; ------------------------------------------------------------------
#; {State   = (state grid Player*)}
#; {Player* = [Setof Player]}
#; {Player  = (player PlayerName p x y)}
#; {PlayerName = String}

(struct state  [grid players] #:transparent)
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

(match-define `(,port-red ,port-white ,port-blue) (map index->port '(2 3 4)))

(define player-red   "red")
(define player-blue  "blue")
(define player-white "white")

(define inits-for-state-with-3-players
  `((,tile-00 ,player-red   ,port-red 0 0)
    (,tile-20 ,player-blue  ,port-blue 2 0)
    (,tile-02 ,player-white ,port-white 0 2)))

(define 3players-list (map (λ (init) (apply player (rest init))) inits-for-state-with-3-players))
(define red-player (first 3players-list))
(define 3players (apply set 3players-list))

(define state3 (state grid3 3players))

(module+ test ;; some contract testing 
  (check-equal? (survivors state3) (set-map 3players player-name) "survivors")
  (check-true (player-on-tile/c (first inits-for-state-with-3-players)))
  (check-true (initial-player-on-tile*/c inits-for-state-with-3-players))
  (check-true (set-member? (survivors state3) player-red))
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

;; ---------------------------------------------------------------------------------------------------
;; SYNTAX

#; (state-from #:grid0 grid0 #:players0 [p ...] S ...)
  
#; (init-list-from-tiles (S ...) ...)
#; (S =  #f
      || I
      || (I PlayerName #:on Port)
      || (I #:rotate (0,90,180,270))
      || (I #:rotate (0,90,180,270) PlayerName #:on Port))
#; {I = 0 .. TILES#}
;; creates a list of tile placements from which initialze and intermediate create a grid 
  
;; ---------------------------------------------------------------------------------------------------
(begin-for-syntax
  (define-syntax-class degree [pattern 90][pattern 180][pattern 270])
    
  (define-syntax-class index/or-index-w-player
    (pattern (~datum #f)
             #:with square #'#f)
    (pattern
     (index (~optional (~seq #:rotate r:degree) #:defaults ([r #'0])) (~optional (~seq n #:on p)))
     #:declare n     (expr/c #'color?)
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

(define-syntax (intermediate-list-from-spec stx)
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
    [(_ (~optional (~seq #:grid0 grid0))
        (~or (~optional (~seq #:players0 [p ...])) (~optional (~seq #:set0 ps)))
        (t:index/or-index-w-player ...) ...)
     #:declare p  (expr/c #'player?)
     #:declare ps (expr/c #'set?)
     #:with players #'(~? (set p.c ...) (~? ps (set)))
     #'(sft/proc (~? grid0 the-empty-grid) players `((,t.square ...) ...))]))

(define (sft/proc grid0 players0 rectangle)
  (define init-list (init-list-from-tiles/proc rectangle))
  (define-values (grid players)
    (for/fold ((grid grid0) (players players0)) ((t init-list))
      (match t
        [`(,tile ,x ,y)
         (values (add-new-square-update-neighbors grid tile x y) players)]
        [`(,tile ,name ,port ,x ,y)
         (define new-player (player name port x y))
         (values (add-new-square-update-neighbors grid tile x y) (set-add players new-player))])))
  (state grid players))

(module+ test ;; testing the DSL
  (check-equal? (intermediate-list-from-spec
                 ((34 "red" #:on port-red) #f (34 #:rotate 90 "blue" #:on port-blue))
                 (#f)
                 ((33 "white" #:on port-white)))
                inits-for-state-with-3-players)
  
  (check-equal? (state-from
                 [[tile-00-index "red" #:on port-red] #f [tile-20-index "blue" #:on port-blue]]
                 (#f)
                 [[tile-02-index "white" #:on port-white]])
                state3))

(define like-state-dsl
  (state-from ((34 "red" #:on port-red) #f (34 #:rotate 90 "blue" #:on port-blue))
              ([33 #:rotate 180])
              ((33 #:rotate 180 "white" #:on port-white))))

(define state-dsl
  (state-from ((34 "red" #:on port-red) #f (34 #:rotate 90 "blue" #:on port-blue))
              (33)
              ((33 #:rotate 180 "white" #:on port-white))))

(module+ test
  (check-false (equal? like-state-dsl state-dsl)))


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
  (define grid
    (for/fold ((grid the-empty-grid)) ((placement (in-list lo-placements)))
      (match-define `(,tile ,_  ,_ ,x ,y) placement)
      (add-new-square-update-neighbors grid tile x y)))
  (state grid players))

(module+ test ;; initialize 
  (check-exn exn:fail:contract? (λ () (initialize `((,tile-00 "x" 2 0 0)))) "port, not index")
  (check-equal? (initialize inits-for-state-with-3-players) state3))

;; ---------------------------------------------------------------------------------------------------
(define (find-first-free-spot s0)
  (match-define (state grid players) s0)
  (let loop ([loc (list 0 0)])
    (if (free-for-init grid loc) (cons (pick-port loc) loc) (loop (clock-wise loc)))))

#; {Grid Location -> Boolean}
(define (free-for-init grid loc)
  (for/and ((n (cons loc (apply neighbor-locations loc))))
    (not (apply matrix-ref grid n))))

#; {Location -> Location}
(define (clock-wise loc)
  (define-values (x y) (apply values loc))
  (cond
    [(and (< (+ x 1) SIZE) (= y 0))          (list (+ x 1) 0)]
    [(and (= (+ x 1) SIZE) (< (+ y 1) SIZE)) (list x (+ y 1))]
    [(and (> x 0) (= (+ y 1) SIZE))          (list (- x 1) y)]
    [(and (= x 0) (>  y 0))                  (list x (- y 1))]
    [else (error 'find-first-free-spot "out of free periphery positions")]))

(define (counter-clock-wise loc)
  (define-values (x y) (apply values loc))
  (cond
    [(and (= x 0) (< (+ y 1) SIZE))          (list x (+ y 1))]
    [(and (< (+ x 1) SIZE) (= (+ y 1) SIZE)) (list (+ x 1) y)]
    [(and (= (+ x 1) SIZE) (> y 0))          (list x (- y 1))]
    [(and (> x 0) (= y 0))                   (list (- x 1) y)]
    [else (error 'find-first-free-spot "out of free periphery positions")]))

#; {Location -> Port}
;; ASSUME no neighboring tile 
(define (pick-port loc)
  (define n (apply neighbor-locations loc))
  (for/first ((p (in-list PORTS)) #:when (apply player-facing-inward? p loc)) p))

(module+ test
  (define-syntax-rule (checks init0 spot1 (color p x y) ...)
    (let*-values ([(init spot) (values init0 spot1)]
                  [(init spot)
                   (let ([c (~a 'color)])
                     (check-equal? (find-first-free-spot (initialize init)) spot c)
                     (values (cons (list* tile-00 c spot) init) (list (index->port p) x y)))]
                  ...)
      (check-equal? (find-first-free-spot (initialize init)) spot "last one")))

  (checks '() `(,port-red 0 0) (red 2 2 0) (black 2 4 0) (blue 2 6 0) (white 2 8 0) (green 0 9 1)))

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
  (define grid0 the-empty-grid)
  (define-values (grid players)
    (for/fold ((grid grid0) (players (set))) ([p l-intermediate])
      (match p
        [(list tile x y)
         (values (add-new-square-update-neighbors grid tile x y)
                 players)]
        [(list tile name port x y)
         (values (add-new-square-update-neighbors grid tile x y)
                 (set-add players (player name port x y)))])))
  (state grid players))

(define bad-intermediate-spec
  (intermediate-list-from-spec
   ((34 "red" #:on port-red) #f (34 #:rotate 90 "blue" #:on port-blue))
   (33 33)
   ((33 #:rotate 180 "white" #:on port-white))))

(define contiguity-for-3-platers `((,(tile-index->tile 33) 0 1) (,(tile-index->tile 33) 1 0)))
(define bad-intermediate-spec-2 (append inits-for-state-with-3-players contiguity-for-3-platers))

(define bad-intermediate-spec-3
  (intermediate-list-from-spec
   ((34 "red" #:on port-red) #f (34 #:rotate 90 "blue" #:on port-blue))
   (33)
   (33 (33 #:rotate 180 "white" #:on port-white))))

(module+ test ;; intermediate grids, states, and contracts 
  (check-false (intermediate*/c bad-intermediate-spec) "an isolated tile that nobody could place")

  (check-true (andmap tile/c contiguity-for-3-platers) "tiles are tiles")
  (check-true (player-on-any-tile/c (first bad-intermediate-spec-2)) "1 p")
  (check-false (tile/c (first bad-intermediate-spec-2)) "1 t")
  (check-true ((or/c player-on-any-tile/c tile/c) (first bad-intermediate-spec-2)) "1 or")
  (check-true ((or/c tile/c player-on-any-tile/c) (first bad-intermediate-spec-2)) "reversed or")
  
  (check-true (intermediate*/c bad-intermediate-spec-2) "it's okay as input but creates a bad state")
  (check-false (every-player-faces-an-open-square       (intermediate-aux bad-intermediate-spec-2))
               "avatar is interior because of contiguity")
  (check-true (every-player-can-leave-going-backwards   (intermediate-aux bad-intermediate-spec-2))
              "but it is on an initial square")

  (check-true (every-player-faces-an-open-square       (intermediate-aux bad-intermediate-spec-3)))
  (check-false (every-player-can-leave-going-backwards (intermediate-aux bad-intermediate-spec-3))
               "the white player can't leave"))

(define good-intermediate-spec
  (intermediate-list-from-spec
   ((34 "red" #:on port-red) #f (34 #:rotate 90 "blue" #:on port-blue))
   (33)
   ((33 #:rotate 180 "white" #:on port-white))))

(define good-intermediate-state
  (state-from ((34 "red" #:on port-red) #f (34 #:rotate 90 "blue" #:on port-blue))
              (33)
              ((33 #:rotate 180 "white" #:on port-white))))

(module+ test ;; testing intermediate's results
  (check-true (every-player-faces-an-open-square      (intermediate-aux good-intermediate-spec)))
  (check-true (every-player-can-leave-going-backwards (intermediate-aux good-intermediate-spec)))
  (check-equal? (intermediate good-intermediate-spec) good-intermediate-state))

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

(define (add-tile state0 player-name tile)
  (match-define  (state grid players) state0)
  (match-define  (player _ port x y) (find-player players player-name))
  (define-values (x-new y-new) (looking-at port x y))
  (define nu-grid (add-new-square-update-neighbors grid tile x-new y-new))
  (define-values (moved out* inf*) (move-players nu-grid players x-new y-new))
  (when (cons? inf*) (raise (exn:infinite player-name (current-continuation-marks) (first inf*))))
  ;; what to do with eliminated ones
  ;; (displayln `(player was thrown out ,out*))
  (state nu-grid moved))

#; {type Action = [List Color [List TileIndex Degree]]}

#; {State Action -> State}
(define (add-tile/a state action)
  (match-define (list player-name (list tile-index degree)) action)
  (add-tile state player-name (rotate-tile (tile-index->tile tile-index) #:degree degree)))

#; {Player* PlayerName -> Player}
(define (find-player players pn)
  (define F (compose (curry equal? pn) player-name))
  (for/first ((element (in-set players)) #:when (F element)) element))

(define tile-to-add-to-grid-3-index 33)
(define state3-action `(,player-red (,tile-to-add-to-grid-3-index 90)))

(define state+
  (state-from #:grid0 grid3
              #:set0 (set-remove (state-players state3) red-player)
              (#f (tile-to-add-to-grid-3-index #:rotate 90))))
(define state3+infinite-index 34)
(define state3+infinite (tile-index->tile state3+infinite-index))

(define good-state-actions `[(,player-red (11 0)) (,player-red (12 0)) (,player-red (34 0))])
(define good-intermediate-state+
  (state-from (34 (11 "red" #:on #\E) (34 #:rotate 90 "blue" #:on port-blue))
              (33)
              ((33 #:rotate 180 "white" #:on port-white))))
(define good-intermediate-state++
  (state-from (34 11                   (34 #:rotate 90 "blue" #:on port-blue))
              (33 (12 "red" #:on #\E))
              ((33 #:rotate 180 "white" #:on port-white))))
(define good-intermediate-state+++
  (state-from (34 11                   (34 #:rotate 90 "blue" #:on port-blue))
              (33 (12 "red" #:on #\D))
              ((33 #:rotate 180) 34)))

(module+ test ;; add-tile
  (check-equal? (find-player 3players "red") red-player)
  (check-equal? (add-tile/a state3 state3-action) state+ "drive red player off")
  
  (check-equal? (add-tile/a good-intermediate-state (first good-state-actions))
                good-intermediate-state+
                "red fwd 1")
  (check-equal? (add-tile/a good-intermediate-state+ (second good-state-actions))
                good-intermediate-state++
                "red fwd 2")
  (check-equal? (add-tile/a good-intermediate-state++ (third good-state-actions))
                good-intermediate-state+++
                "red fwd 3")
  
  (check-exn exn:infinite? (λ () (add-tile state3 player-red state3+infinite))
             "drive red player into infinite loop"))

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

#; {Grid Player* Index Index -> (values Player* [Listof Player] [Listof Player])}

;; move players facing (x,y), detrmine survivors, return those as the first list;
;; -- the second value is the list of drop-outs that run into walls
;; -- the third value is the list of drop-outs that get into an infinite loop 

(define (move-players grid players x y)
  (define-values (moved out inf)
    (for/fold ((moved (set)) (out '()) (inf '())) ((p (in-set players)))
      (match-define  (player name port x-p y-p) p)
      (define-values (x-at y-at) (looking-at port x-p y-p))
      (cond
        [(and (= x-at x) (= y-at y))
         (define p-moved (move-one-player grid p))
         (cond
           [(out? p-moved) (values moved (cons p-moved out) inf)]
           [(inf? p-moved) (values moved out (cons p-moved inf))]
           [else (values (set-add moved p-moved) out inf)])]
        [else (values (set-add moved p) out inf)])))
  (values moved out inf))

(struct out [player] #:transparent)
(struct inf [player] #:transparent)

#; {Grid Player [#:to-periphery Boolean] -> (U Player (out Player) (inf Player))}
;; (1) move a player forward to the first open square or the wall
;; (2) when to-periphery?: move a player backwards to the a square on the periphery or to the wall 
(define (move-one-player grid the-player #:to-periphery? (to-periphery #f))
  ;; start player on (port-p, x-p, y-p) that look at an occupied neighboring square
  (define peri? (if to-periphery bordering-periphery? (λ (x y) #f)))
  (match-define (player name port-p x-p y-p) the-player)
  (define place `(,x-p ,y-p, port-p))
  (let/ec return 
    (let move-one-player ([port port-p](square (matrix-ref grid x-p y-p))[seen (set place)])
      (when (and to-periphery (outside? (square port)))
        ;; the player didn't get to the periphery but an open square 
        (return the-player))
      (match (move-player-one-square grid square port name seen)
        [(? player? it) it]
        [(? out? it)    it]
        [(? inf? it)    it]
        [(list port square seen) (move-one-player port square seen)]))))

#; {type Seen  = [Setof Place]}
#; {type Place = [List Index Index Port]}

#; {Place Seen -> Boolean}
(define (seen? p seen) (set-member? seen p))

#; {Grid Square Port Name Seen -> (U Player (out Player) (inf Player) [List Port Index Index Seen])}
;; move player at (x-p, y-p) on player-port to port-in on the neighboring square and then to port-out 
;; ASSUME there is a tile at the player-square
(define (move-player-one-square grid player-square player-port name seen)
  (define next        (player-square player-port))
  (define port-in     (next-port next))
  (define x           (next-x next))
  (define y           (next-y next))
  (define next-square (matrix-ref grid (next-x next) (next-y next)))
  (define port-out    ((square-tile next-square) port-in))
  (define external    (next-square port-out))
  (define place       `(,x ,y ,port-out))
  (cond
    [(open? external)   (player name port-out x y)]
    [(wall? external)   (out (player name port-out x y))]
    [(seen? place seen) (inf (player name port-out x y))]
    [else (list port-out (matrix-ref grid x y) (set-add seen place))]))

(define grid+  (state-grid state+))
(define sq-00+  (matrix-ref grid+ 0 0))
(define (red i) (player "red" (index->port i) 1 0))
(define red-out (out (red 1)))

(match-define (state grid-inf _)
  (state-from [(34 "red" #:on port-red) 34 (34 #:rotate 90 "blue" #:on port-blue)]
              [33]
              [(33 #:rotate 180 "white" #:on port-white)]))

(module+ test ;; move player
  (check-equal? (move-player-one-square grid+ sq-00+ port-red "red" (set)) red-out "move red 1 step")
  (check-equal? (move-one-player grid+ red-player) red-out "move red player out")
  
  (match-define (state grid-good-move _)
    (state-from [(34 "red" #:on port-red) 33 (34 #:rotate 90 "blue" #:on port-blue)]
                [33]
                [(33 #:rotate 180 "white" #:on port-white)]))
  (check-equal? (move-one-player grid-good-move (player "red" port-red 0 0)) (red 5) "move red good")
  
  (check-equal? (move-one-player grid-inf red-player) (inf red-player) "move player inf"))

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

#; {Player* Natural Natural -> (U False Player)}
(define (is-player-on players x y)
  (define p (set-member players (λ (p) (match-define (player _ _ x0 y0) p) (and (= x x0) (= y y0)))))
  (cond
    [(set-empty? p) #f]
    [else (define p-on-x-y (set-first p))
          (list (player-name p-on-x-y) (player-port p-on-x-y))]))

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

(module+ picts
  (require (submod Tsuro/Code/Common/tiles picts))
  (require (submod Tsuro/Code/Common/grid picts))
  (define INSET  (+ 20 TILE-SIZE))
  (define WIDTH  (+ INSET (* 10 TILE-SIZE) INSET))
  (define HEIGHT (+ INSET (* 10 TILE-SIZE) INSET))

  #; {State (Instanceof DC<%>) -> Pict}
  (define (state->pict b dc)
    (match-define (state squares players) b)
    (define grid-as-pict
      (let loop ([l (matrix->rectangle squares)][y 0])
        (cond
          [(empty? l) (blank)]
          [else
           (define row (first l))
           (define picts
             (for/list ((square (in-list row)) (x (in-naturals)))
               (square->pict square (is-player-on players x y))))
           (vl-append (apply hc-append picts) (loop (rest l) (+ y 1)))])))
    (draw-pict grid-as-pict dc INSET INSET))
  
  #; {PortIndex Natural Natural -> (values Natural Natural)}
  #;
  (define (logical-coordinates->geometry port x y)
    (values (* x TILE-SIZE)) (* TILE-SIZE y))

  #; {State -> Void}
  (define (show-state s #:visible (v #t))
    (define frame (new frame% [label "hello"][width WIDTH][height HEIGHT]))
    
    (define canvas
      (new canvas%
           [parent frame]
           [paint-callback (λ (e dc) (state->pict s dc))]))
    (send canvas on-paint)

    (send frame show v)))

(module+ picts (show-state good-intermediate-state+ #:visible #f))

; (module+ picts (show-state good-intermediate-state++))

(module+ picts (show-state like-state-dsl))
(module+ picts (show-state state-dsl))

; (module+ picts (show-state good-intermediate-state+++))
; (module+ picts (show-state (add-tile good-intermediate-state++ player-red (tile-index->tile 34))))

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

  ;; -------------------------------------------------------------------------------------------------
  (define (state->jsexpr s)
    (define players (state-players s))
    (matrix-where (state-grid s) (λ (sq x y) sq) (square->jsexpr players)))

  #; {Player* -> [Square Index Index -> JSexpr]}
  ;; does not belong into square.rkt because that one doesn't know about (x,y)
  (define ((square->jsexpr players) sq x y)
    (define tj (tile->jsexpr (square-tile sq)))
    (match (is-player-on players x y)
      [(? boolean?) (list tj x y)]
      [(list name port) (list tj name (port->jsexpr port) x y)]))

  (define (intermediate*->jsexpr intermediates)
    (for/list ((i intermediates))
      (define ti (tile->jsexpr (first i)))
      (match i
        [`(,t ,x ,y) `(,ti ,x ,y)]
        [`(,t ,name ,port ,x ,y) `(,ti ,name ,(port->jsexpr port) ,x ,y)])))

  ;; -------------------------------------------------------------------------------------------------
  (def/mp init-pat
    (_ t n p x y) #'`(,(tile-pat t) ,(? color? n) ,(port-pat p) ,(? index? x) ,(? index? y)))
  (def/mp intermediate-pat
    (_ ti-d x y) #'`(,(tile-pat ti-d) ,(? index? x) ,(? index? y)))
  (def/mp state-pat
    (_) #'`(,(or (init-pat ti-d _ _ x y) (intermediate-pat ti-d x y)) (... ...)))
  
  (define (jsexpr->intermediate j)
    (match j
      [(init-pat ti-d name p x y)  (list (jsexpr->tile ti-d) name (jsexpr->port p) x y)]
      [(intermediate-pat ti-d x y) (list (jsexpr->tile ti-d) x y)]))

  (define (jsexpr->state sj)
    (define ims (match sj [(state-pat) (map jsexpr->intermediate sj)]))
    (cond
      [(intermediate*/c ims) (intermediate ims)]
      [else #f]))
  
  (def/mp action-pat
    (_ pn ti) #'`(,(? color? pn) ,(tile-pat ti)))
  
  (check-true (match state3-action [(action-pat pn ti) #t]))
  (define state3-action-infinite (list player-red (tile->jsexpr state3+infinite)))
  (check-true (match state3-action-infinite [(action-pat pn ti) #t]))

  (define state+-jsexpr (state->jsexpr state+))

  ;; -------------------------------------------------------------------------------------------------
  (check-equal? (jsexpr->state (state->jsexpr state3)) state3)
  (check-equal? (jsexpr->state (state->jsexpr good-intermediate-state)) good-intermediate-state)

  (define state3-jsexpr (state->jsexpr state3))
  (define good-intermediate-state-jsexpr (state->jsexpr good-intermediate-state))
  (define good-intermediate-state+++-jsexpr (state->jsexpr good-intermediate-state+++))
  
  (check-false (jsexpr->state (intermediate*->jsexpr bad-intermediate-spec)))
  (check-false (jsexpr->state (intermediate*->jsexpr bad-intermediate-spec-2)))

  (define bad-intermediate-spec-jsexpr (intermediate*->jsexpr bad-intermediate-spec))
  (define bad-intermediate-spec-2-jsexpr (intermediate*->jsexpr bad-intermediate-spec-2)))
