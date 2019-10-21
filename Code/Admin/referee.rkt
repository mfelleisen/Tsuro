#lang racket 

(define player* (listof referee-player/c))

(define MIN-PLAYERS 3)
(define MAX-PLAYERS 5)

(provide
 (contract-out
  (referee (->* [(and/c [listof player/c] (λ (l) (<= MIN-PLAYERS (length l) MAX-PLAYERS)))]
                (list/c [listof player*] player*)))))

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/player-interface)
(require Tsuro/Code/Common/rules)
(require (submod Tsuro/Code/Common/tiles json))
(require (submod Tsuro/Code/Common/board json))
(require Tsuro/Code/Common/port)

(require "../Lib/xsend.rkt")

(require SwDev/Debugging/spy)

(module+ test
  (require Tsuro/Code/Players/player)
  (require Tsuro/Code/Players/strategies)
  (require (submod Tsuro/Code/Common/board test))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define TILES (range TILES#))

#; {type Rankings = [Listof [Listof Player]]}

(struct internal [external color] #:transparent)
#; {type Internal = (internal Player Color)}

(define (referee external*)
  (define internal* (assign-avatars external*))
  (define cheaters0 (inform-about-self-and-others internal*))
  (match-define (list state0 cheaters remaining) (initial-placements (remove* cheaters0 internal*)))
  ;; there are always enough tiles left so remaining doesn't need to be refilled 
  (play-rounds state0 remaining (remove* cheaters internal*)))
  
(module+ test
  ;; testing remove* 
  (define playrs (list (internal 'dummy "red") (internal 'dummy "white") (internal 'dummy "green")))
  (check-equal? (remove* (list (third playrs) (first playrs)) playrs) (list (second playrs))))

  
;; ---------------------------------------------------------------------------------------------------
#; {[Listof Player] -> [Listof Internal]}
(define (assign-avatars external*)
  (for/list ((e external*) (c TOKEN-COLORS))
    (internal e c)))

(module+ test
  (check-equal? (assign-avatars '(1 2 3))
                (list
                 (internal 1 (first TOKEN-COLORS))
                 (internal 2 (second TOKEN-COLORS))
                 (internal 3 (third TOKEN-COLORS)))))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof Internal] -> [Listof Internal]}
(define (inform-about-self-and-others internal*)
  (define all-colors (map internal-color internal*))
  (for/fold ((cheaters '())) ((i internal*))
    (match-define (internal external color) i)
    (define void-failed (xsend external playing-as color))
    (cond
      [(failed? void-failed) (cons i cheaters)]
      [else (define void-failed (xsend external playing-with (remove color all-colors)))
            (if (failed? void-failed) (cons i cheaters) cheaters)])))

(module+ test
  (define strategy       (new first-strategy%))
  (define red-external   (new player% [strategy strategy]))
  (define red-internal   (internal red-external "red"))
  (define white-external (new player% [strategy strategy]))
  (define white-internal (internal white-external "white"))
  (define playah%        (class player% (super-new [strategy strategy]))))

(module+ test 
  (define bad-playing-as% (class playah% (super-new) (define/override (playing-as c) (raise 0))))
  (define bad-playing-with% (class playah% (super-new) (define/override (playing-with c) (raise 1))))
  
  (define inform0
    (list
     (internal (new bad-playing-as%) "white")
     red-internal 
     (internal (new bad-playing-with%) "green")))

  (check-equal? (inform-about-self-and-others inform0) (list (third inform0) (first inform0))))

;; ---------------------------------------------------------------------------------------------------
(define INIT# 3)

#; {[Listof Internal] -> (list State [Listof Internal] [Listof TileIndex])}
;; KNOWLEDGE there are enough tile types to initialize the board 
(define (initial-placements internal*)
  (define places0 '[])
  (define state0  (initialize places0))
  (define-values (_ state cheaters remaining)
    (for/fold ([initials places0][state state0][cheats '()][remaining TILES]) ([i internal*])
      (match-define (internal external color) i)
      (match-define [list (list tile1 tile2 tile3) remaining+1] (split-tiles remaining INIT#))
      (define choice-failed (xsend external initial initials tile1 tile2 tile3))
      (cond
        [(failed? choice-failed) (values initials state (cons i cheats) remaining+1)]
        [(legal-initial (initialize initials) color tile1 tile2 tile3 choice-failed)
         =>
         (λ (next) (values (cons (->initials color choice-failed) initials) next cheats remaining+1))]
        [else (values initials state (cons i cheats) remaining+1)])))
  (list state cheaters remaining))

#; {Color InitialAction -> Initial}
(define (->initials color ia)
  (match-define (list ti port x y) ia)
  (list (jsexpr->tile ti) color port x y))

(module+ test
  (define bad-init-time% (class playah% (super-new) (define/override (initial . x) (let L () (L)))))
  (define bad-init-choice%
    (class playah%
      (super-new)
      ;; this method relies on the specific strategy of using the last given tile
      (define/override (initial is ti1 ti2 ti3)
        (list [list (modulo (+ ti3 1) TILES#) 0] (index->port 3) 0 0))))

  (define initial0
    (list red-internal
          (internal (new bad-init-time%) "green")
          (internal (new bad-init-choice%) "white")))
  (define good-init (send/apply red-external initial '[] (take TILES 3)))
  (check-equal? (initial-placements initial0)
                (list (initialize (list (->initials (internal-color red-internal) good-init)))
                      (reverse (rest initial0))
                      (drop TILES 9))))

;; ---------------------------------------------------------------------------------------------------
(define TURN# 2)

#; {State [Listof TileIndex] [Listof Internal] -> (list Rankings [Listof Player])}
(define (play-rounds-old state0 tiles internal*)

  ;; *****************************************************************************
  ;; this fails to distinguish between ONE round and a COMPLETE GAME
  ;; *****************************************************************************

  (define finder (find-external internal*))
  (define-values (_ rankings cheats _2)
    (let/ec final 
      (for/fold ([state state0] [rankings '[]] [cheats '()] [remaining tiles]) ([i internal*])
        (match-define (internal external color) i)
        (match-define (list (list tile1 tile2) remaining+1) (split-tiles remaining TURN#))
        (define choice-failed (xsend external take-turn (state->intermediate* state) tile1 tile2))
        (cond
          [(failed? choice-failed) (values state rankings (cons (finder color) cheats) remaining+1)]
          [(legal-take-turn state color tile1 tile2 choice-failed)
           =>
           (λ (next)
             (define others (add-to rankings finder state next))
             (cond
               [(final? next)
                (define firsts (map finder (survivors next)))
                (final next (if (empty? firsts) others (cons firsts others)) cheats remaining+1)]
               [else (values next others cheats remaining+1)]))]
          [else (values state rankings (cons (finder color) cheats) remaining+1)]))))
  (list rankings cheats))

#; {State [Listof TileIndex] [Listof Internal] ->
          (list State [Listf TileIndex] [Listof Player] [Listof Player])}

(define (play-rounds state0 tiles internal*)
  (define finder (find-external internal*))
  (define-values (state-N ranked-in-this-round cheats remaining)
    (let/ec stop-round 
      (for/fold ([state state0] [rankings '[]] [cheats '()] [remaining tiles]) ([i internal*])
        (match-define (internal external color) i)
        (match-define (list (list tile1 tile2) remaining+1) (split-tiles remaining TURN#))

        ;; don't play unless avatar is alive 


        (define choice-failed (xsend external take-turn (state->intermediate* state) tile1 tile2))
        (cond
          [(failed? choice-failed) (values state rankings (cons (finder color) cheats) remaining+1)]
          [(legal-take-turn state color tile1 tile2 choice-failed)
           =>
           (λ (next)
             (define others (add-to rankings finder state next))
             ((if (empty? (survivors next)) stop-round values) next others cheats remaining+1))]
          [else (values state rankings (cons (finder color) cheats) remaining+1)]))))
  (list state-N remaining ranked-in-this-round cheats))

#; {Rankings [Color -> Player] State State -> Rankings}
(define (add-to rankings finder state-n state-n+1)
  (define survivors-n (apply set (survivors state-n)))
  (define survivors-1 (apply set (survivors state-n+1)))
  (cond
    [(set=? survivors-1 survivors-n) rankings]
    [else (append (map finder (set->list (set-subtract survivors-n survivors-1))) rankings)]))

#; {[Listof Internal] -> [ Color -> Player ]}
(define [(find-external internal*) color]
  (internal-external (first (memf (λ (i) (equal? (internal-color i) color)) internal*))))


(module+ test
  (define active0 (find-external '[]))
  (check-equal? (add-to '[] active0  good-intermediate-state good-intermediate-state++) '[])

  (define active1 (list red-internal))
  (define finder1 (find-external active1))
  (check-equal? (add-to '[] finder1 state-suicide state-suicide++) (list red-external))

  (define active2 `(,red-internal ,(internal (new playah%) "white") ,(internal (new playah%) "blue")))
  (define finder2 (find-external active2))
  (check-equal? (add-to '[] finder2 good-intermediate-state+ state-suicide)
                (map internal-external (rest active2))))

(module+ test
  (define bad-turn-time% (class playah% (super-new) (define/override (take-turn . x) (let L () (L)))))
  (define bad-turn-time (new bad-turn-time%))
  (define active3 (list (internal bad-turn-time "red")))
  (check-equal? (play-rounds state-suicide TILES active3)
                (list state-suicide (cddr TILES) '[] `[,bad-turn-time])
                "turn diverges")

  (define chosen-suicide-tiles (cons state-suicide-index TILES))
  (check-equal? (play-rounds state-suicide chosen-suicide-tiles active1)
                (list state-suicide (cddr chosen-suicide-tiles) '[]  `[,red-external])
                "chosen suicide of last player")

  (define forced-suicide-tiles (list state-suicide-index state-suicide-index))
  (define rankings `[[,red-external]])
  (define cheats   '[])
  (check-equal? (play-rounds state-suicide forced-suicide-tiles active1)
                (list state-suicide++ TILES `[,red-external] '[])
                "forced suicide of last player")

  (define two-rounds-suicide (append forced-suicide-tiles forced-suicide-tiles))
  (define active4 (list red-internal white-internal))
  ; (define state4  (add-tile collision-state "red" state-suicide-index))
  
  (require (submod Tsuro/Code/Common/board picts))
  (show-state (first (play-rounds collision-state two-rounds-suicide active4)) #:name "2 infinite")

  ;; ******************************************************************
  ;; not placing the tile when an avatar goes into an infinite loop
  ;; means that intermediate states no longer satisfy the "periphery"
  ;; condition ~~ I do wonder whether this is also the case with the
  ;; failed tests from last week

  ;; RETHINK the not place the tile
  
  ;; ******************************************************************


  #;
  (check-equal? (play-rounds collision-state++ two-rounds-suicide active4)
                (list (minus-player (minus-player collision-state++ "red") "white")
                      (append (cddr two-rounds-suicide) TILES)
                      (map internal-external active4)
                      '[])
                "one turn infinite loop of two players")
  )
  

;; ---------------------------------------------------------------------------------------------------
#; {[Listof TileIndex] N -> (List [Listof TileIndex] [Listof TileIndex])}
;; the second list has at least n elements 
(define (split-tiles r n)
  (define remaining (drop r n))
  (list (take r n) (if (>= (length remaining) n) remaining (append remaining TILES))))

(module+ test
  (check-equal? (split-tiles '(a b c d) 2) '[(a b) [c d]])
  (check-equal? (split-tiles '(a b c) 3) (list '(a b c) TILES)))

;; ---------------------------------------------------------------------------------------------------
(define BAD-PLACEMENT:fmt "~a broke the rules of placing initial avatars~a")
(define BAD-MOVE:fmt      "~a broke the take-turn rules\n [~e]")
(define XOTHER:fmt        "~a's 'other' method failed\n [~e]")
(define XSETUP1:fmt       "~a failed with the 'placement' method\n[~a]")
(define XSETUP2:fmt       "~a timed out for the 'placement' method\n[~a]")
(define XPLAY:fmt         "~a failed with the 'take-turn' method\n[~a]")