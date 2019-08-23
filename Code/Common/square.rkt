#lang racket

;; ---------------------------------------------------------------------------------------------------
;; a data representation for squares on a board
;; -- a square is aware of neighbors but not the actual board size 

(provide
 SIZE

 #; {type Square = [Port -> Next]}
 #; {type Next = OPEN || WALL || (next port index index)}
 
 create-square 
 square-tile
 update-square

 looking-at
 
 OPEN
 WALL
 outside? 
 
 next-port
 next-x
 next-y)

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


(require (except-in Tsuro/Code/Common/tiles tile? table))
(require (except-in Tsuro/Code/Common/port-alphabetic port?))
  
(module+ test
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

(define WALL "wall") 
(define OPEN "open")

(define (outside? x)
  (member x (list WALL OPEN)))

#; {Square  = (U BLANK                   ;; an unoccupied, blank square 
                 (square Tile PortMap))} ;; a configured tile with connections to neighbors cached 
#; {PortMap = (Vectorof Next) :: [Port ->f Next]}
#; {Next    = (U
               WALL                      ;; periphery of board 
               OPEN                      ;; no neighboring tile yet 
               (next Port Index Index))} ;; points to a port on this or a neigboring square 

(define SIZE 10) ; Tsuro is played on a board with SIZE x SIZE configured tiles

(struct square [tile map]
  #:transparent
  #:property prop:procedure (lambda (this-square from) (vector-ref (square-map this-square) from)))

(struct next [port x y] #:transparent)

#; {[Listof [List Index Index]] Tile Index Index -> square}
;; create a square at (x,y) from Tile with current matrix n*
(define (create-square neighbors tile x y)
  (define portmap
    (for*/fold ((portmap (create-portmap x y))) ((g neighbors))
      (apply update-portmap portmap x y g)))
  (square tile portmap))


#; {Square Index Index Index Index -> square}
(define (update-square old x-old y-old x-new y-new)
  (square (square-tile old) (update-portmap (square-map old) x-old y-old x-new y-new)))

(module+ test ;; operating on squares
  (define t (tile-index->tile 33))
  (check-equal? (create-square '((2 0) (0 0)) t 1 0)
                (square t
                        (let* ([pm (create-portmap 1 0)]
                               [pm (update-portmap pm 1 0 2 0)]
                               [pm (update-portmap pm 1 0 0 0)])
                          pm)))

  (define tile34    (tile-index->tile 34))
  (define square-00 (create-square '() tile34 0 0))
  (check-equal? (update-square square-00 0 0 1 0)
                (square tile34 (update-portmap (square-map square-00) 0 0 1 0))))

  
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

#; {Port Index Index -> (values Integer Integer)}
(define (looking-at port x y)
  (case (port->direction port)
    [(NORTH) (values x (- y 1))]
    [(EAST)  (values (+ x 1) y)]
    [(SOUTH) (values x (+ y 1))]
    [(WEST)  (values (- x 1) y)]))
    
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