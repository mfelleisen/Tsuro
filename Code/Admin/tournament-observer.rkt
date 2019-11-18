#lang racket/gui

(require Tsuro/Code/Admin/observer-interfaces)

(provide (contract-out [show-tournament tournament-observer/c]))

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

(require Tsuro/Code/Lib/canvas-with-history)
(require (except-in pict table))

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

(define PLAYER# 20)
(define SLEEP  .99)
(define INSET   1)

(define SCALE*  .99)

(define COLORS '("pink" "purple" "turquoise" "sky")) ;; of game borders 

;; describe the display of a player (by age) 
(define FONT    22)
(define SAMPLE  (text (~a 99) 'roman FONT))
(define CELL    (rounded-rectangle (+ 8 (pict-width SAMPLE)) (+ 8 (pict-height SAMPLE))))
(define PWIDTH  (pict-width CELL))
(define PHEIGHT (pict-height CELL))

(define SPACE   8) ;; between players, games, and rounds 

(define to-int (compose inexact->exact round (curry * SCALE*)))
(define HEIGHT (to-int (+ SPACE (* (+ PLAYER# 1) (+ SPACE PHEIGHT)) SPACE)))
(define WIDTH  (to-int (* PLAYER# (+ SPACE PWIDTH SPACE))))

(define EMPTY  (blank 100 100))

#; (N -> [N [Listof [Listof [List N]]] -> Void])
(define (show-tournament max-age)
  (define frame (new frame% [label "tournament observer"][width WIDTH][height HEIGHT]))
  (define history* (make-vector PLAYER# EMPTY))
  (define canvas
    (new history-canvas% [parent frame] [style '(vscroll hscroll)] [inset INSET] [history history*]))
  (send canvas show-scrollbars #t #t)
  (send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
  (send frame show #t)
  
  (define history (blank 1 1))
  (define (callback games)
    (define next (scale (games->pict max-age games) SCALE*))
    (set! history (hc-append (* 8 SPACE) history next))
    (send canvas set history)
    (sleep SLEEP))
  callback)

#; { N [Listof [Listof N]] : sorted -> Pict }
(define (games->pict maxage games)
  (define active (apply append games))
  (define p0 (if (empty? games) (text "no active players left") (blank PWIDTH HEIGHT)))
  (define active-players
    (for*/fold ([p p0]) ([i (in-range maxage)][a (in-value (- maxage i 1))] #:when (member a active))
      (pin-over p SPACE (age->height i) (player->pict a))))
  (for/fold ((pict active-players)) ([1game games][1color COLORS])
    (define omega (last 1game))
    (define y0 (- (age->height (- maxage (first 1game) 1)) 2))
    (define y1 (+ (age->height (- maxage (last 1game))) 2))
    (define gp (game->pict (- y1 y0) 1color))
    (pin-over pict (/ SPACE 2) (- y0 2) gp)))

#; {N -> N}
(define (age->height i)
  (+ SPACE (* i (+ PHEIGHT SPACE))))

#; {N Color -> Pict}
(define (game->pict height color)
  (rounded-rectangle (+ PWIDTH SPACE) height #:border-color color #:border-width 1))

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
    (define show (show-tournament PLAYER#))
    (show '[[19 18 17 16 15] [14 13 12 11 10] [9 8 7 6 5] [4 3 2 1 0]])
    (show '[[19 18 13 12 10] [9 3 2 1 0]])
    (show '[[19 3 2 1]])
    (show '[[19 3]]))
  (demo))
