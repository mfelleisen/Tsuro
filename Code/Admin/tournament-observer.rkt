#lang racket/gui

(require Tsuro/Code/Admin/observer-interfaces)

(provide
 (contract-out [show-games tournament-observer/c]))

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

(require Tsuro/Code/Common/board)
(require (submod Tsuro/Code/Common/board picts))
(require Tsuro/Code/Common/grid)
(require (submod Tsuro/Code/Common/tiles picts))
(require (submod Tsuro/Code/Common/tiles json))
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/rules)
(require Tsuro/Code/Lib/canvas-with-history)
(require (except-in pict table))

(require SwDev/Debugging/spy)

;                              
;          ;                   
;          ;                   
;          ;                   
;    ;;;   ; ;;    ;;;  ;     ;
;   ;   ;  ;;  ;  ;; ;; ;     ;
;   ;      ;   ;  ;   ;  ; ; ; 
;    ;;;   ;   ;  ;   ;  ; ; ; 
;       ;  ;   ;  ;   ;  ;; ;; 
;   ;   ;  ;   ;  ;; ;;  ;; ;; 
;    ;;;   ;   ;   ;;;    ; ;  
;                              
;                              
;                              

;; the scale factor for the Pict and the graphics setup 
(define PLAYER# 20)
(define FONT    22)
(define SCALE*  .99)
(define INSET   1)
(define SLEEP  .99)
(define COLORS '("pink" "purple" "turquoise" "sky"))

(define SAMPLE  (text (~a 99) 'roman FONT))
(define CELL    (rounded-rectangle (+ 8 (pict-width SAMPLE)) (+ 8 (pict-height SAMPLE))))
(define PWIDTH  (pict-width CELL))
(define PHEIGHT (pict-height CELL))

(define SEP 8)

(define to-int (compose inexact->exact round (curry * SCALE*)))

(define HEIGHT (to-int (+ SEP (* (+ PLAYER# 1) (+ SEP PHEIGHT)) SEP)))
(define WIDTH  (to-int (* PLAYER# (+ SEP PWIDTH SEP))))

(define EMPTY  (blank 100 100))

#; (N -> [N [Listof [Listof [List N]]] -> Void])
(define (show-games max-age)
  (define frame (new frame% [label "game observer"][width WIDTH][height HEIGHT]))
  (define canvas
    (new history-canvas% [parent frame] [style '(vscroll hscroll)]
         [inset INSET] [history (make-vector (* SIZE SIZE) EMPTY)] [empty EMPTY]))
  (send canvas show-scrollbars #t #t)
  (send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
  (send frame show #t)
  
  (define history (blank 1 1))
  (define (callback games)
    (set! history (hc-append (* 8 SEP) history (games->pict max-age games)))
    (send canvas set history)
    (sleep SLEEP))
  callback)

#; { N [Listof [Listof N]] : sorted -> Pict }
(define (games->pict max-age games)
  (define active (apply append games))
  (define active-players
    (for*/fold ([pict (blank PWIDTH HEIGHT)])
               ([i (in-range max-age)][age (in-value (- max-age i 1))] #:when (member age active))
      (pin-over pict SEP (age->height i) (player->pict age))))
  (for/fold ((pict active-players)) ([1game games][1color COLORS])
    (define omega (last 1game))
    (define y0 (- (age->height (- max-age (first 1game) 1)) 2))
    (define y1 (+ (age->height (- max-age (last 1game))) 2))
    (define gp (game->pict (- y1 y0) 1color))
    (pin-over pict (/ SEP 2) (- y0 2) gp)))

#; {N -> N}
(define (age->height i)
  (+ SEP (* i (+ PHEIGHT SEP))))

#; {N Color -> Pict}
(define (game->pict height color)
  (rounded-rectangle (+ PWIDTH SEP) height #:border-color color #:border-width 1))

#; {N -> Pict}
(define (player->pict age)
  (cc-superimpose CELL (text (~a age) 'roman 22)))

;                              
;       ;                      
;       ;                      
;       ;                      
;    ;;;;   ;;;  ;;;;;;   ;;;  
;   ;; ;;  ;;  ; ;  ;  ; ;; ;; 
;   ;   ;  ;   ;;;  ;  ; ;   ; 
;   ;   ;  ;;;;;;;  ;  ; ;   ; 
;   ;   ;  ;     ;  ;  ; ;   ; 
;   ;; ;;  ;     ;  ;  ; ;; ;; 
;    ;;;;   ;;;; ;  ;  ;  ;;;  
;                              
;                              
;                              

(module+ picts
  (provide demo)
  (define (demo)
    (define show (show-games PLAYER#))
    (show '[[19 18 17 16 15] [14 13 12 11 10] [9 8 7 6 5] [4 3 2 1 0]])
    (show '[[19 18 13 12 10] [9 3 2 1 0]])
    (show '[[19 3 2 1]])
    (show '[[19 3]]))
  (demo))
