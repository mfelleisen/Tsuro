#lang racket/gui

(require Tsuro/Code/Common/board)
(require (submod Tsuro/Code/Common/board picts))
(require (submod Tsuro/Code/Common/tiles picts))
(require (submod Tsuro/Code/Common/tiles json))
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/rules)
(require (only-in pict filled-rectangle draw-pict text vc-append vl-append hc-append))

#;{type Turn = [list [list avatar? [list tile-index? degree?]] tile1-index tile2-index]}

;; the graphics (a bit of a hack)
(define INSET  (+ 20 TILE-SIZE))
(define WIDTH  (+ INSET (* 10 TILE-SIZE) INSET))
(define HEIGHT (+ INSET (* 11 TILE-SIZE) INSET))

#; {State Turn [U False State] -> Void}
(define (show state turn-rep legal)
  (define frame (new frame% [label "game observer"][width WIDTH][height HEIGHT]))
  (define (paint _e dc) (draw-pict (combine-turn-and-state state turn-rep legal) dc INSET INSET))
  (define canvas (new canvas% [parent frame] [paint-callback paint]))
  (send canvas on-paint)
  (send frame show #t))

#; {State Turn [U False State] -> Pict}
(define (combine-turn-and-state state turn-rep legal)
  (define avatars-pict (apply vc-append 20 (map avatar->pict (survivors state))))
  (define turn-pict    (turn->pict turn-rep legal))
  (define state-pict   (state->pict state))
  (vc-append 20 turn-pict (hc-append 10 avatars-pict state-pict)))

(define (avatar->pict a)
  (filled-rectangle 10 10 #:color a))

(avatar->pict "green")

#; {Turn [U False State] -> Pict}
(define (turn->pict turn-rep legal)
  (match-define [list [list avatar tile-spec] ti1 ti2] turn-rep)
  (define tile   (tile->pict (jsexpr->tile tile-spec)))
  (define choice (text (format "~a chose to place the following tile" avatar)))
  (define from   (text (format "from the following given tile types")))
  (define alts   (map tile->pict (map tile-index->tile (list ti1 ti2))))
  (define decision (text (format "which is ~a" (if (boolean? legal) "illegal" "legal"))))
  (vl-append (apply hc-append 10 choice tile from alts) decision))

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
  
