#lang racket/gui

(require Tsuro/Code/Admin/observer-interfaces)

(provide
 (contract-out [show-turn game-observer/c]))

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

;; the scale factor for the Pict and the graphics setup 
(define SCALE-FACTOR 1)
(define SLEEP-TIME .99)

;; the graphics constants for the frame 
(match-define (list INSET WIDTH HEIGHT)
  (let* ([INSET  TILE-SIZE]
         [WIDTH  (+ INSET (* 2 TILE-SIZE) (* SIZE TILE-SIZE) INSET)]
         [HEIGHT (+ INSET (* 2 TILE-SIZE) (* (+ SIZE 1) TILE-SIZE) INSET)])
    (map (compose inexact->exact round (curry * SCALE-FACTOR)) (list INSET WIDTH HEIGHT))))

#; {-> (State Turn [U False State] -> Void)}
(define (show-turn)
  (define frame (new frame% [label "game observer"][width WIDTH][height HEIGHT]))
  (define canvas (new history-canvas% [parent frame] [style '(vscroll hscroll)]))
  (send canvas show-scrollbars #t #t)
  (send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
  (send frame show #t)
  (define (callback state turn-rep legal)
    (send canvas set (combine-turn-and-state state turn-rep legal))
    (sleep SLEEP-TIME))
  callback)

;                                                   
;   ;                                               
;   ;         ;            ;                        
;   ;                      ;                        
;   ; ;;    ;;;    ;;;   ;;;;;   ;;;    ;;;;  ;   ; 
;   ;;  ;     ;   ;   ;    ;    ;; ;;   ;;  ; ;   ; 
;   ;   ;     ;   ;        ;    ;   ;   ;      ; ;  
;   ;   ;     ;    ;;;     ;    ;   ;   ;      ; ;  
;   ;   ;     ;       ;    ;    ;   ;   ;      ; ;  
;   ;   ;     ;   ;   ;    ;    ;; ;;   ;      ;;   
;   ;   ;   ;;;;;  ;;;     ;;;   ;;;    ;       ;   
;                                               ;   
;                                              ;    
;                                             ;;    

(define history-canvas%
  (class canvas%
    (inherit on-paint refresh-now)

    (super-new [paint-callback (λ (_e dc) (draw-pict picture dc INSET INSET))])

    (define empty (blank WIDTH HEIGHT))

    (field [picture empty])
    (field [history (make-vector (* SIZE SIZE) empty)])
    (field [filled  0])
    (field [pointer 0])

    (define/public (set new-picture)
      (set! filled  (add1 filled))
      (set! pointer filled)
      (vector-set! history filled new-picture)
      (paint new-picture))

    (define/override (on-char e)
      (define direction 
        (case (send e get-key-code)
          [(right) (set! pointer (min (add1 pointer) 100))]
          [(left)  (set! pointer (max (sub1 pointer) 0))]
          [else    pointer]))
      (paint (vector-ref history pointer)))
      
    (define/private (paint new-picture)
      (set! picture new-picture)
      (refresh-now)
      (on-paint))))

#; {State Turn [U False State] -> Pict}
(define (combine-turn-and-state state turn-rep legal)
  (define avatars-pict (apply vc-append TILE-SIZE (map avatar->pict (survivors state))))
  (define turn-pict    (turn->pict turn-rep legal))
  (define state-pict   (state->pict (or legal state)))
  (define complete     (hc-append TILE-SIZE avatars-pict (vc-append TILE-SIZE turn-pict state-pict)))
  (scale complete SCALE-FACTOR))

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

(define (avatar->pict a (age ""))
  (define image  (filled-rectangle TILE-SIZE TILE-SIZE #:color a))
  (define tcolor (if (equal? a "black") "white" "black"))
  (define order  (colorize (text (~a age) 'roman (- (pict-height image) 2)) tcolor))
  (cc-superimpose image order))

#; {Turn [U False State] -> Pict}
(define (turn->pict turn-rep legal)
  (match-define   [list [list avatar age tile-spec] ti1 ti2] turn-rep)
  (define tile    (tile->pict (jsexpr->tile tile-spec)))
  (define choice  (text "chose to place the following tile"))
  (define from    (text (format "from the following given tile types")))
  (define t1-pict (tile->pict (tile-index->tile ti1)))
  (define t2-pict (tile->pict (tile-index->tile ti2)))
  (cond
    [(boolean? legal)
     (define stuff (text "which is illegal"))
     (hc-append 10 (avatar->pict avatar age) choice tile from t1-pict t2-pict stuff)]
    [else 
     (define stuff (text "which is legal and results in the following state"))
     (vl-append
      (hc-append 10 (avatar->pict avatar age) choice tile from t1-pict t2-pict)
      stuff)]))
  
;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (require (submod Tsuro/Code/Common/board json))
  (require (submod Tsuro/Code/Common/board test))

  (define (pick-a-state-and-a-turn name)
    (define choice  (list-ref CHOICES (or (secret name) (random N-CHOICES))))
    (match-define [list tag state (and action [list player [list tile-index _]])] choice)
    (define aturn `[[,player ,(second action)] ,tile-index ,tile-index])
    (values tag state aturn))

  #; {String -> (U False 1 2 3)}
  (define (secret name)
    (define n (regexp-match SECRET name))
    (and n (for/first ((c CHOICES) (i (in-naturals)) #:when (regexp-match (second n) (first c))) i)))

  (define BACKDOOR "backdoor-")
  (define SECRET   (pregexp (string-append BACKDOOR "(.*)")))
  (define CHOICES
    `(("good one"  ,good-intermediate-state-jsexpr ,(first good-state-actions))
      ("infinite"  ,state3-jsexpr                  ,state3-action-infinite)
      ("collision" ,collision-state-jsexpr         ,collision-action)))
  (define N-CHOICES (length CHOICES))

  (define-values [tag state turn0] (pick-a-state-and-a-turn "red"))
  (match-define [list [list avatar action] t1 t2] turn0)
  (define turn (list (list avatar 1 action) t1 t2))
  (define state-next (jsexpr->state state))

  (define show (show-turn))
  (show state-next turn (legal-take-turn state-next avatar t1 t2 action))
  (show state-next turn (legal-take-turn state-next avatar t1 t2 action)))