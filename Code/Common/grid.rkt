#lang racket

(require (only-in Tsuro/Code/Common/tiles tile?))
(require (only-in Tsuro/Code/Common/port port?))
(require SwDev/Lib/or)

#; {Nat -> Boolean : Index}
(define (index? z) (< -1 z SIZE))

(define grid? object?)
(define location/c (list/c index? index?))

(provide
 #; {type Grid}
 SIZE
 index?

 the-empty-grid

 location/c

 #; {Grid Tile Index Index -> Grid}
 (contract-out
  [add-new-square-update-neighbors
   ;; "update neighbors" means inform neighbor squares about the new one 
   (-> grid? tile? index? index? grid?)]

  [neighbor-locations
   (-> location/c [listof location/c])]
  
  [free-for-init
   (-> grid? location/c boolean?)]
  
  [port-facing-inward?
   ;; p on (x,y) looks at an interior square}
   (-> port? index? index? boolean?)]

  [looking-at
   (-> port? index? index? (values integer? integer?))]

  ;; data examples 
  [grid3 grid?])
 
 #; {type Square = BLANK || [Port -> Next]}
 BLANK
 (contract-out
  [square-tile (-> square? tile?)])

 #; {type Next = open? || wall? || next?}
 (contract-out
  [next-port (-> next? port?)]
  [next-x    (-> next? index?)]
  [next-y    (-> next? index?)]
  [open?     (-> any/c boolean?)]
  [wall?     (-> any/c boolean?)]
  [outside?  (-> any/c any)])
 
 ;; data examples 
 square-00
 square-02
 square-20)

(module+ picts
  (require Tsuro/Code/Common/tokens)

  (define ((empty-if-blank sq) on-sq) (or (not (equal? sq BLANK)) (empty? on-sq)))

  (provide

   tile+players->pict 

   (contract-out
    [square->pict
     ;; if square is BLANK, p must be '()
     (->i ([sq (or/c BLANK square?)]
           [on-sq (sq) (and/c (listof (list/c color? port?)) (empty-if-blank sq))])
          [r pict?])])))

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

(require (except-in Tsuro/Code/Common/tiles tile?))
(require (except-in Tsuro/Code/Common/port port?))
(require Tsuro/Code/Common/matrix)
(require SwDev/Lib/should-be-racket)
(require pict)

(module+ picts
  (require (except-in (submod Tsuro/Code/Common/tiles picts) table))
  (require pict))

(module+ test
  (require (submod ".." picts))
  (require (submod ".."))
  (require rackunit))

;                                                                 
;       ;                                  ;            ;;        
;       ;           ;                      ;           ;          
;       ;           ;                      ;           ;          
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;;   ;;;   ;;;;;        
;   ;; ;;      ;    ;        ;         ;; ;;  ;;  ;    ;          
;   ;   ;      ;    ;        ;         ;   ;  ;   ;;   ;          
;   ;   ;   ;;;;    ;     ;;;;         ;   ;  ;;;;;;   ;          
;   ;   ;  ;   ;    ;    ;   ;         ;   ;  ;        ;          
;   ;; ;;  ;   ;    ;    ;   ;         ;; ;;  ;        ;     ;;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;;;;   ;;;;    ;     ;;   
;                                                                 
;                                                                 
;                                                                 

(define SIZE 10) ; Tsuro is played on a board with SIZE x SIZE configured tiles

#; {Grid   = [Matrixof Square] :: {Index x Index}}
#; {Index   = [0 .. SIZE]}
#; {Square  = (U BLANK                   ;; an unoccupied, blank square 
                 (square Tile PortMap))} ;; a configured tile with connections to neighbors cached 
#; {PortMap = (Vectorof Next) :: [Port ->f Next]}
#; {Next    = (U
               WALL                      ;; periphery of board 
               OPEN                      ;; no neighboring tile yet 
               (next Port Index Index))} ;; points to a port on this or a neigboring square 

(define BLANK #false)
(struct square [tile map] #:transparent
  #:property
  prop:procedure
  (λ (this-square from-port) (vector-ref (square-map this-square) (port->index from-port))))

(struct next [port x y] #:transparent)
(struct wall [] #:transparent) (define WALL (wall))
(struct open [] #:transparent) (define OPEN (open))

;                                                          
;       ;                                                  
;       ;           ;                                      
;       ;           ;                                      
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;   ;   ;        
;   ;; ;;      ;    ;        ;         ;;  ;   ; ;         
;   ;   ;      ;    ;        ;         ;   ;;  ;;;         
;   ;   ;   ;;;;    ;     ;;;;         ;;;;;;   ;          
;   ;   ;  ;   ;    ;    ;   ;         ;       ;;;         
;   ;; ;;  ;   ;    ;    ;   ;         ;       ; ;    ;;   
;    ;;;;   ;;;;    ;;;   ;;;;          ;;;;  ;   ;   ;;   
;                                                          
;                                                          
;                                                          

(define the-empty-grid (build-matrix SIZE SIZE (λ (_i _j) BLANK)))

#; { PortIndex Index Index -> Boolean : p on (x,y) looks at an interior square}
(define (port-facing-inward? p x y)
  (define-values (x-facing y-facing) (looking-at p x y))
  (and (index? x-facing) (index? y-facing)))

(define (outside? x)
  (member x (list WALL OPEN)))

(define (neighbor-locations loc)
  (match-define (list x y) loc)
  (define all `((,x ,(- y 1)) (,x ,(+ y 1)) (,(- x 1) ,y) (,(+ x 1) ,y)))
  (filter (match-lambda [`(,x ,y) (and (index? x) (index? y))]) all))

(define (looking-at port x y)
  (case (port->direction port)
    [(NORTH) (values x (- y 1))]
    [(EAST)  (values (+ x 1) y)]
    [(SOUTH) (values x (+ y 1))]
    [(WEST)  (values (- x 1) y)]))

#; {Grid Location -> Boolean}
(define (free-for-init grid loc)
  (for/and ((n (cons loc (apply neighbor-locations loc))))
    (not (apply matrix-ref grid n))))

(define (add-new-square-update-neighbors grid0 tile x y)
  (define neighbors (map (cons-square grid0) (neighbors* grid0 x y)))
  (define squares*  (add-square neighbors tile x y))
  (for*/fold ((grid grid0)) ((sq+x+y squares*))
    (apply matrix-set grid sq+x+y)))

#; {Grid -> [List Index Index] -> [List Square Index Index]}
(define ((cons-square grid0) neighbor-coordinates)
  (cons (apply matrix-ref grid0 neighbor-coordinates) neighbor-coordinates))

#; {Grid Index Index -> [Listof Location]}
;; determine (occupied) neighbors of the square at (x,y)
(define (neighbors* grid x y)
  (filter (match-lambda [`(,x ,y) (matrix-ref grid x y)]) (neighbor-locations (list x y))))

#;
(->i ([neighbors [listof [list/c square? index? index?]]]
      [tile tile?]
      [x index?]
      [y index?])
     [result (neighbors)
             (and/c
              [listof [list/c index? index? square?]]
              (flat-named-contract 'added-one (λ (r) (= (length r) (+ (length neighbors) 1)))))])
;; create a square at (x,y) from tile, with current neighbor coordinates 
(define (add-square neighbors tile x y)
  (define portmap
    (for*/fold ((portmap (create-portmap x y))) ((g neighbors))
      (apply update-portmap portmap x y (rest g))))
  (cons (list x y (square tile portmap))
        (for/list ((g neighbors))
          (match-define (list neighbor-sq x-sq y-sq) g)
          (list x-sq y-sq (update-square neighbor-sq x-sq y-sq x y)))))
  
(define (update-square old x-old y-old x-new y-new)
  (square (square-tile old) (update-portmap (square-map old) x-old y-old x-new y-new)))

(define the-one-square (compose third first))

(module+ test ;; operating on squares
  (check-equal? (update-square square-00 0 0 1 0)
                (square tile-00 (update-portmap (square-map square-00) 0 0 1 0)))
  
  (define tile-10 (tile-index->tile 33))
  (check-equal? (add-square `((,square-20 2 0) (,square-00 0 0)) tile-10 1 0)
                (list 
                 (list 1 0 (square tile-10
                                   (let* ([pm (create-portmap 1 0)]
                                          [pm (update-portmap pm 1 0 2 0)]
                                          [pm (update-portmap pm 1 0 0 0)])
                                     pm)))
                 (list 2 0 (square tile-20 (update-portmap (square-map square-20) 2 0 1 0)))
                 (list 0 0 (square tile-00 (update-portmap (square-map square-00) 0 0 1 0))))))

  
;                                                   
;                        ;                          
;                        ;         ;                
;                        ;                          
;    ;;;   ;;;;    ;;;   ; ;;    ;;;   ; ;;    ;;;; 
;   ;;  ;      ;  ;;  ;  ;;  ;     ;   ;;  ;  ;;  ; 
;   ;          ;  ;      ;   ;     ;   ;   ;  ;   ; 
;   ;       ;;;;  ;      ;   ;     ;   ;   ;  ;   ; 
;   ;      ;   ;  ;      ;   ;     ;   ;   ;  ;   ; 
;   ;;     ;   ;  ;;     ;   ;     ;   ;   ;  ;; ;; 
;    ;;;;   ;;;;   ;;;;  ;   ;   ;;;;; ;   ;   ;;;; 
;                                                 ; 
;                                              ;  ; 
;                                               ;;  

#; {Index Index -> PortMap }
;; create the default portmap for a square 
(define-match-expander ??
  (λ (stx)
    (syntax-case stx ()
      [(_ w) #'(? (curry = (- w 1)))])))

(define (create-portmap x y)
  (match* (x y)
    [(0  0)                (west-north)]
    [(0 (?? SIZE))         (west-south)]
    [((?? SIZE) 0)         (east-north)]
    [((?? SIZE) (?? SIZE)) (east-south)]
    [(0  n)                (west-any)  ]
    [((?? SIZE) n)         (east-any)  ]
    [(n 0)                 (any-north) ]
    [(n (?? SIZE))         (any-south) ]
    [(n k)                 (any-any)   ]))

;; ---------------- ports:    0   1   2    3    4    5    6    7 ---
;;                            north    east      south     west  
[define (west-north) (vector WALL WALL OPEN OPEN OPEN OPEN WALL WALL)]
[define (west-south) (vector OPEN OPEN OPEN OPEN WALL WALL WALL WALL)]
[define (east-north) (vector WALL WALL WALL WALL OPEN OPEN OPEN OPEN)]
[define (east-south) (vector OPEN OPEN WALL WALL WALL WALL OPEN OPEN)]
[define (west-any)   (vector OPEN OPEN OPEN OPEN OPEN OPEN WALL WALL)]
[define (east-any)   (vector OPEN OPEN WALL WALL OPEN OPEN OPEN OPEN)]
[define (any-north)  (vector WALL WALL OPEN OPEN OPEN OPEN OPEN OPEN)]
[define (any-south)  (vector OPEN OPEN OPEN OPEN WALL WALL OPEN OPEN)]
[define (any-any)    (vector OPEN OPEN OPEN OPEN OPEN OPEN OPEN OPEN)]

(module+ test ;; creating a basic portmap 
  (define s-1 (- SIZE 1))
  (define ran (λ () (+ (random (- SIZE 2)) 1))) ;; in [1,s-1)

  (check-equal? (create-portmap 0     0   )  (west-north))
  (check-equal? (create-portmap 0     s-1)   (west-south))
  (check-equal? (create-portmap s-1   0)     (east-north))
  (check-equal? (create-portmap s-1   s-1)   (east-south))
  (check-equal? (create-portmap 0     (ran)) (west-any))
  (check-equal? (create-portmap s-1   (ran)) (east-any))
  (check-equal? (create-portmap (ran) 0)     (any-north))
  (check-equal? (create-portmap (ran) s-1)   (any-south))
  (check-equal? (create-portmap (ran) (ran)) (any-any)))

#; {PortMap Index Index Index Index -> PortMap}
;; update the external parts of the portmap at (x-pm,y-pm) to connect with neighbors on n at (x-n,y-n)
(define (update-portmap portmap x-pm y-pm x-new y-nnew)
  (for/vector ((pi (in-range (vector-length portmap))))
    (define port (index->port pi))
    (define-values (x-look y-look) (looking-at port x-pm y-pm))
    (if (and (eq? x-look x-new) (eq? y-look y-nnew))
        (next (facing-port port) x-new y-nnew)
        (vector-ref portmap pi))))

(module+ test ;; adding external connections to a portmap 
  (define nu-pm
    (let* ([pm (create-portmap 1 0)]
           [pm (update-portmap pm 1 0 2 0)]
           [pm (update-portmap pm 1 0 0 0)])
      pm))
  (define pm3+
    (let ([v (vector-copy nu-pm)])
      (vector-set! v 2 (next (facing-port (index->port 2)) 2 0))
      (vector-set! v 3 (next (facing-port (index->port 3)) 2 0))
      v))
          
  (check-equal? (update-portmap nu-pm 1 0 2 0) pm3+))

;                                                                                             
;                                                                                             
;                                                                         ;;;                 
;                                                                           ;                 
;  ;;;;;;   ;;;    ;;;;   ;;;           ;;;   ;   ;  ;;;;  ;;;;;;  ;;;;     ;     ;;;    ;;;  
;  ;  ;  ; ;; ;;   ;;  ; ;;  ;         ;;  ;   ; ;       ; ;  ;  ; ;; ;;    ;    ;;  ;  ;   ; 
;  ;  ;  ; ;   ;   ;     ;   ;;        ;   ;;  ;;;       ; ;  ;  ; ;   ;    ;    ;   ;; ;     
;  ;  ;  ; ;   ;   ;     ;;;;;;        ;;;;;;   ;     ;;;; ;  ;  ; ;   ;    ;    ;;;;;;  ;;;  
;  ;  ;  ; ;   ;   ;     ;             ;       ;;;   ;   ; ;  ;  ; ;   ;    ;    ;          ; 
;  ;  ;  ; ;; ;;   ;     ;             ;       ; ;   ;   ; ;  ;  ; ;; ;;    ;    ;      ;   ; 
;  ;  ;  ;  ;;;    ;      ;;;;          ;;;;  ;   ;   ;;;; ;  ;  ; ;;;;      ;;   ;;;;   ;;;  
;                                                                  ;                          
;                                                                  ;                          
;                                                                  ;                          

(define square-00 (the-one-square (add-square '() tile-00 0 0)))
(define square-02 (the-one-square (add-square '() tile-02 0 2)))
(define square-20 (the-one-square (add-square '() tile-20 2 0)))

;                                                                 
;       ;                                                         
;       ;           ;                                             
;       ;           ;                                             
;    ;;;;  ;;;;   ;;;;;  ;;;;           ;;;   ;   ;               
;   ;; ;;      ;    ;        ;         ;;  ;   ; ;                
;   ;   ;      ;    ;        ;         ;   ;;  ;;;                
;   ;   ;   ;;;;    ;     ;;;;         ;;;;;;   ;                 
;   ;   ;  ;   ;    ;    ;   ;         ;       ;;;                
;   ;; ;;  ;   ;    ;    ;   ;         ;       ; ;    ;;          
;    ;;;;   ;;;;    ;;;   ;;;;          ;;;;  ;   ;   ;;          
;                                                                 
;                                                                 
;                                                                 

(define grid3
  (let* ([m the-empty-grid]
         [m (matrix-set m 0 0 square-00)]
         [m (matrix-set m 0 2 square-02)]
         [m (matrix-set m 2 0 square-20)])
    m))

(define tile-to-add-to-grid-3-index 33)
(define tile-to-add-to-grid-3 (tile-index->tile tile-to-add-to-grid-3-index))

(define nu-grid3
  (let* ([m grid3]
         (neighbors  (map (cons-square m) (neighbors* m 1 0)))
         (nu-squares (add-square neighbors tile-to-add-to-grid-3 1 0))
         [m (matrix-set m 1 0 (caddr (first nu-squares)))]
         [m (matrix-set m 0 0 (caddr (second nu-squares)))]
         [m (matrix-set m 2 0 (caddr (third nu-squares)))])
    m))

(module+ test
  (check-equal? (add-new-square-update-neighbors grid3 tile-to-add-to-grid-3 1 0) nu-grid3))

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
  (define PLAYER-SIZE (quotient TILE-SIZE 5)) 

  #; {Square [Listof [List Color Port]] -> Pict}
  ;; if square is BLANK, p must be '()
  (define (square->pict square p)
    (define tile (or (and (equal? BLANK square) blank-tile) (square-tile square)))
    (tile+players->pict tile p))

  #; {Tile [Listof [List Color Port]] -> Pict}
  (define (tile+players->pict tile p)
    (match-define (list (list colors ports) ...) p)
    (define-values (pict _)
      (for/fold ([pict (tile->pict tile)][seen '()]) ([c colors][p ports])
        (define often (memf (curry equal? p) seen))
        (define jack (jack-o-lantern (- PLAYER-SIZE (* 3 (if often (length often) 0))) c))
        (values (add-player pict jack p) (cons p seen))))
    pict))

(module+ test
  (square->pict BLANK '())
  (define two (index->port 2))
  (square->pict square-00 '())
  (square->pict square-00 `[("blue" ,two)])
  (square->pict square-00 `[["blue" ,two] ["white" ,(index->port 1)]])
  (square->pict square-00 `[["blue" ,two] ["white" ,two]])
  (square->pict square-00 `[["blue" ,two] ["white" ,two] ["red" ,two]]))

(module+ homework
  (require (submod ".." picts))
  (provide tile+players->pict blank-tile))