#lang racket

(require Tsuro/Code/Common/player-interface)
(require Tsuro/Code/Players/strategies)

(define internal%/c (class/c {init-field [strategy (instanceof/c strategy/c)]}))
(define internal-player (and/c internal%/c player%/c))

(provide
 (contract-out
  [player% internal-player]
  
  [bad-playing-as%    internal-player]
  [bad-playing-with%  internal-player]
  [bad-init-time%     internal-player]
  [bad-init-choice%   internal-player]
  [bad-turn-time%     internal-player]
  [bad-end-of-tournament% internal-player]))

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

(require Tsuro/Code/Players/first-s)
(require Tsuro/Code/Common/port)

(module+ test
  (require (submod ".."))
  (require (submod Tsuro/Code/Common/board test))
  (require Tsuro/Code/Common/port)
  (require rackunit))

;                              
;   ;                          
;   ;                          
;   ;                          
;   ;;;;   ;;;;    ;;;    ;;;  
;   ;; ;;      ;  ;   ;  ;;  ; 
;   ;   ;      ;  ;      ;   ;;
;   ;   ;   ;;;;   ;;;   ;;;;;;
;   ;   ;  ;   ;      ;  ;     
;   ;; ;;  ;   ;  ;   ;  ;     
;   ;;;;    ;;;;   ;;;    ;;;; 
;                              
;                              
;                              

;; internals of players
;; internally, the player is game mechanics while the strategy component makes game decisions 

(define player%
  (class object%
    (init-field strategy)

    (field (me "blue"))
    (field (other-players '()))

    [define/public (playing-as my-name)
      (set! me my-name)]
    
    [define/public (playing-with others)
      (set! other-players others)]
    
    #; (-> initial-player-on-tile*/c tile-index? tile-index? tile-index? init-action/c)
    (define/public (initial tiles-placed-so-far tile1 tile2 tile3)
      (define board (initialize tiles-placed-so-far))
      ;; checkable ~~ the placed players are among others; what's my position in the game
      (send strategy initial board tile1 tile2 tile3))

    #; (-> intermediate*/c tile-index? tile-index? turn-action/c)
    [define/public (take-turn tiles-placed-so-far tile1 tile2)
      (define board (intermediate tiles-placed-so-far))
      ;; optional: update _others_ because some may no longer be with us 
      (send strategy take-turn me board tile1 tile2)]

    #; (-> [listof string?] any)
    [define/public (end-of-tournament results)
      (void)]
    
    (super-new)))

(module+ test
  (define player (new player% [strategy (new first-strategy%)]))

  (define-syntax-rule (checks init0 spot1 (color p x y) ...)
    (let*-values ([(init spot) (values init0 spot1)]
                  [(init spot)
                   (let ([c (~a 'color)])
                     (check-equal? (send player initial init 1 2 3) (cons `(3 0) spot) c)
                     (values (cons (list* tile-00 c spot) init) (list (index->port p) x y)))]
                  ...)
      (check-equal? (send player initial init 1 2 3) (cons `(3 0) spot) "last one")))

  (define port-red (index->port 2))
  (checks '() `(,port-red 1 0) (red 2 3 0) (black 2 5 0) (blue 2 7 0) (white 4 9 0) (green 0 9 2))

  (check-equal? (send player take-turn inits-for-state-with-3-players 1 2) (list 1 0)))

;                              
;   ;                 ;        
;   ;                 ;        
;   ;                 ;        
;   ;;;;   ;;;;    ;;;;   ;;;  
;   ;; ;;      ;  ;; ;;  ;   ; 
;   ;   ;      ;  ;   ;  ;     
;   ;   ;   ;;;;  ;   ;   ;;;  
;   ;   ;  ;   ;  ;   ;      ; 
;   ;; ;;  ;   ;  ;; ;;  ;   ; 
;   ;;;;    ;;;;   ;;;;   ;;;  
;                              
;                              
;                              

(define-syntax-rule (define/override-m % m)
  (define %
    (class player%
      (super-new [strategy (new first-strategy%)])
      m)))

(define-syntax-rule (define/time % m) (define/override-m % (define/override (m . x) (let L () (L)))))

(define-syntax-rule (define/raise % m n) (define/override-m % (define/override (m . x) (raise n))))

(define/raise bad-playing-as% playing-as 0)
(define/raise bad-playing-with% playing-with 1)
(define/time bad-init-time% initial)

(define/override-m bad-init-choice% ;; chooses index not given 
  (define/override (initial is ti1 ti2 ti3)
    (list [list (bad-index (list ti1 ti2 ti3)) 0] (index->port 3) 0 0)))

(define/time bad-turn-time% take-turn)
(define/override-m bad-turn-choice% ;; chooses index not given 
  (define/override (take-turn ti1 ti2)
    [list (bad-index (list ti1 ti2)) 0]))

(define/raise bad-end-of-tournament% end-of-tournament 3)

(define (bad-index . t**)
  (for/first ((ti TILES#) #:when (boolean? (member ti t**)))
    ti))

