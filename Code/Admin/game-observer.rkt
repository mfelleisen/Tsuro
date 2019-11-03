#lang racket/gui

(provide
 #; {type Turn = [list [list avatar? [list tile-index? degree?]] tile1-index tile2-index]}
 #; {State Turn [U False State] -> Void}
 ;; accept information about the current state for a regular turn,
 ;; the action requested by the active avatar, and
 ;; the next state or #false if it is illegal 
 show)

(require Tsuro/Code/Common/board)
(require (submod Tsuro/Code/Common/board picts))
(require Tsuro/Code/Common/grid)
(require (submod Tsuro/Code/Common/tiles picts))
(require (submod Tsuro/Code/Common/tiles json))
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/rules)
(require (only-in pict scale filled-rectangle draw-pict text vc-append vl-append hc-append))

;; the scale factor for the Pict and the graphics setup 
(define SCALE-FACTOR 1)

;; the graphics constants for the frame 
(match-define (list INSET WIDTH HEIGHT)
  (let* ([INSET  TILE-SIZE]
         [WIDTH  (+ INSET (* 2 TILE-SIZE) (* SIZE TILE-SIZE) INSET)]
         [HEIGHT (+ INSET (* 2 TILE-SIZE) (* (+ SIZE 1) TILE-SIZE) INSET)])
    (map (compose inexact->exact round (curry * SCALE-FACTOR)) (list INSET WIDTH HEIGHT))))

#; {State Turn [U False State] -> Void}
(define (show state turn-rep legal)
  (define frame (new frame% [label "game observer"][width WIDTH][height HEIGHT]))
  (define (paint _e dc) (draw-pict (combine-turn-and-state state turn-rep legal) dc INSET INSET))
  (define canvas (new canvas% [parent frame] [style '(vscroll hscroll)] [paint-callback paint]))
  (send canvas show-scrollbars #t #t)
  (send canvas init-auto-scrollbars WIDTH HEIGHT 0. 0.)
  (send canvas on-paint)
  (send frame show #t))

#; {State Turn [U False State] -> Pict}
(define (combine-turn-and-state state turn-rep legal)
  (define avatars-pict (apply vc-append TILE-SIZE (map avatar->pict (survivors state))))
  (define turn-pict    (turn->pict turn-rep legal))
  (define state-pict   (state->pict state))
  (define complete     (hc-append TILE-SIZE avatars-pict (vc-append TILE-SIZE turn-pict state-pict)))
  (scale complete SCALE-FACTOR))

(define (avatar->pict a)
  (filled-rectangle TILE-SIZE TILE-SIZE #:color a))

#; {Turn [U False State] -> Pict}
(define (turn->pict turn-rep legal)
  (match-define   [list [list avatar tile-spec] ti1 ti2] turn-rep)
  (define tile    (tile->pict (jsexpr->tile tile-spec)))
  (define choice  (text "chose to place the following tile"))
  (define from    (text (format "from the following given tile types")))
  (define t1-pict (tile->pict (tile-index->tile ti1)))
  (define t2-pict (tile->pict (tile-index->tile ti2)))
  (define legal?  (text (format "which is ~a" (if (boolean? legal) "illegal" "legal"))))
  (hc-append 10 (avatar->pict avatar) choice tile from t1-pict t2-pict legal?))

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

  (define-values [tag state turn] (pick-a-state-and-a-turn "red"))
  (match-define [list [list avatar action] t1 t2] turn)
  (define state-next (jsexpr->state state))
  (show state-next turn (legal-take-turn state-next avatar t1 t2 action)))
  
