#lang racket 

(define player* (listof referee-player/c))

(define MIN-PLAYERS 3)
(define MAX-PLAYERS 5)

(provide
 (contract-out
  (referee (->* [(and/c [listof player/c] (λ (l) (<= MIN-PLAYERS (length l) MAX-PLAYERS)))]
                (values [listof player*] player*)))))

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
(define TILE-INDEX (range TILES#))

#; {type Rankings = [Listof [Listof Player]]}

(struct internal [external color] #:transparent)
#; {type Internal = (internal Player Color)}

(define (referee external*)
  (define internal* (assign-avatars external*))
  (define cheaters0 (inform-about-self-and-others internal*))
  (define-values (state0 cheaters remaining) (initial-placements (remove* cheaters0 internal*)))
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
#; {[Listof Internal] -> (values [Listof Internal])}
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
  (define strategy      (new first-strategy%))
  (define good-external (new player% [strategy strategy]))
  (define good-player   (internal good-external "red"))
  (define playah%       (class player% (super-new [strategy strategy]))))

(module+ test 
  (define bad-playing-as% (class playah% (super-new) (define/override (playing-as c) (raise 0))))
  (define bad-playing-with% (class playah% (super-new) (define/override (playing-with c) (raise 1))))
  
  (define inform0
    (list
     (internal (new bad-playing-as%) "white")
     good-player 
     (internal (new bad-playing-with%) "green")))

  (check-equal? (inform-about-self-and-others inform0) (list (third inform0) (first inform0))))

;; ---------------------------------------------------------------------------------------------------
(define INIT# 3)

#; {[Listof Internal] -> (values State [Listof Internal] [Listof TileIndex])}
;; KNOWLEDGE there are enough tile types to initialize the board 
(define (initial-placements internal*)
  (define places0 '[])
  (define state0  (initialize places0))
  (define-values (_ state cheaters remaining)
    (for/fold ([initials places0][state state0][cheats '()][remaining TILE-INDEX]) ([i internal*])
      (match-define (internal external color) i)
      (match-define [list (list tile1 tile2 tile3) remaining+1] (split-tiles remaining INIT#))
      (define choice-failed (xsend external initial initials tile1 tile2 tile3))
      (cond
        [(failed? choice-failed) (values initials state (cons i cheats) remaining+1)]
        [(legal-initial (initialize initials) color tile1 tile2 tile3 choice-failed)
         =>
         (λ (next) (values (cons (->initials color choice-failed) initials) next cheats remaining+1))]
        [else (values initials state (cons i cheats) remaining+1)])))
  (values state cheaters remaining))

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
    (list good-player
          (internal (new bad-init-time%) "green")
          (internal (new bad-init-choice%) "white")))
  (define good-init (send/apply good-external initial '[] (take TILE-INDEX 3)))
  (check-equal? (let-values [([x y z] (initial-placements initial0))] (list x y z))
                (list (initialize (list (->initials (internal-color good-player) good-init)))
                      (reverse (rest initial0))
                      (drop TILE-INDEX 9))))

;; ---------------------------------------------------------------------------------------------------
(define TURN# 2)

#; {State [Listof TileIndex] [Listof Internal] -> (values Rankings [Listof Player])}
(define (play-rounds state0 tiles internal*)
  (define-values (_ rankings cheats _2)
    (let/ec final 
      (for/fold ([state state0] [rankings '[]] [cheats '()] [remaining tiles]) ([i internal*])
        (match-define (internal external color) i)
        (match-define (list (list tile1 tile2) remaining+1) (split-tiles remaining TURN#))
        (define intermediates (state->jsexpr state))

        ;; *****************************************************************************
        ;; state->jsexpr is NOT exactly what I need because I don't get a port back.
        ;; *****************************************************************************

        (define choice-failed (send external take-turn intermediates tile1 tile2))
        (cond
          [(failed? choice-failed) (values state rankings (cons i cheats) remaining+1)]
          [(legal-take-turn state color tile1 tile2 choice-failed)
           =>
           (λ (next)
             (if (final? next)
                 (final (values next (add-to rankings internal* state next) cheats remaining+1))
                 (values next (add-to rankings internal* state next) cheats remaining+1)))]
          [else (values state rankings (cons i cheats) remaining+1)]))))
  (list rankings cheats))

#; {Rankings [Listof Internal] State State -> Rankings}
(define (add-to rankings internal* state-n state-n+1)
  (define survivors-n (apply set (survivors state-n)))
  (define survivors-1 (apply set (survivors state-n+1)))
  (cond
    [(set=? survivors-1 survivors-n) rankings]
    [else
     (define newly-gone (set->list (set-subtract survivors-n survivors-1)))
     (define next-rank (map (find-external internal*) newly-gone))
     (cons next-rank rankings)]))

#; {[Listof Internal] -> [ Color -> Player ]}
(define [(find-external internal*) color]
  (internal-external (first (memf (λ (i) (equal? (internal-color i) color)) internal*))))

(module+ test
  (check-equal? (add-to '[] '[]  good-intermediate-state good-intermediate-state++) '[])

  (define active1 (list good-player))
  (check-equal? (add-to '[] active1 state-suicide state-suicide++) (list (list good-external)))

  (define active2 (list good-player (internal (new playah%) "white") (internal (new playah%) "blue")))
  (check-equal? (add-to '[] active2 good-intermediate-state+ state-suicide)
                (list (map internal-external (rest active2)))))

(module+ test
  (check-equal? (play-rounds state-suicide TILE-INDEX active1) (list '[] (list good-player))))

;; ---------------------------------------------------------------------------------------------------
#; {[Listof TileIndex] N -> (List [Listof TileIndex] [Listof TileIndex])}
;; the second list has at least n elements 
(define (split-tiles r n)
  (list (take r n) (if (> (length r) n) (drop r n) (append r TILE-INDEX))))

(module+ test
  (check-equal? (split-tiles '(a b c d) 2) '[(a b) [c d]])
  (check-true (>= (length (second (split-tiles '(a b c) 3))) 3)))

;; ---------------------------------------------------------------------------------------------------
(define BAD-PLACEMENT:fmt "~a broke the rules of placing initial avatars~a")
(define BAD-MOVE:fmt      "~a broke the take-turn rules\n [~e]")
(define XOTHER:fmt        "~a's 'other' method failed\n [~e]")
(define XSETUP1:fmt       "~a failed with the 'placement' method\n[~a]")
(define XSETUP2:fmt       "~a timed out for the 'placement' method\n[~a]")
(define XPLAY:fmt         "~a failed with the 'take-turn' method\n[~a]")