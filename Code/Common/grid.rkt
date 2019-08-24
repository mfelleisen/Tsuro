#lang racket

(require (only-in Tsuro/Code/Common/square index? SIZE looking-at square-tile))
(require (only-in Tsuro/Code/Common/tiles tile?))
(require (only-in Tsuro/Code/Common/port-alphabetic port?))
(require Tsuro/Code/Lib/or)

(provide
 #; {type Grid}
 
 add-new-square-update-neighbors
 neighbors*
 neighbor-locations

 the-empty-grid
 grid3)

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
(require (except-in Tsuro/Code/Common/tiles tile?))
(require (except-in Tsuro/Code/Common/port-alphabetic port?))
(require Tsuro/Code/Common/matrix)
(require Tsuro/Code/Lib/should-be-racket)
(require pict)

(module+ test
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

#; {Grid   = [Matrixof Square] :: {Index x Index}}
#; {Index   = [0 .. SIZE]}

(define the-empty-grid (build-matrix SIZE SIZE (λ (_i _j) BLANK)))

#; {Grid Tile Index Index -> Grid}
;; "update neighbors" means inform neighbor squares about the new one 
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
  (filter (match-lambda [`(,x ,y) (matrix-ref grid x y)]) (neighbor-locations x y)))

#; {Index Index -> [Listof Location]}
(define (neighbor-locations x y)
  (define all `((,x ,(- y 1)) (,x ,(+ y 1)) (,(- x 1) ,y) (,(+ x 1) ,y)))
  (filter (match-lambda [`(,x ,y) (and (index? x) (index? y))]) all))

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