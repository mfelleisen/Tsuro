#lang racket/gui

;; how many unique tiles are there, and what do they look like 

(require Tsuro/Lib/or)

;; ---------------------------------------------------------------------------------------------------
#; {Configuration Configuration -> Boolean}
(define (configuration-equality c1 c2-0 -equal?)
  (define (veq? a b) (equal? (struct->vector a) (struct->vector b)))
  (or~ (veq? c1 c2-0)
       #:let c2-1 (rotate c2-0)
       (veq? c1 c2-1)
       #:let c2-2 (rotate c2-1)
       (veq? c1 c2-2)
       #:let c2-3 (rotate c2-2)
       (veq? c1 c2-3)))

#; {Configuration -> Configuration}
(define (rotate c0)
  (match-define (configuration e1 e2 e3 e4) c0)
  (create-configuration (rotate-edge e1) (rotate-edge e2) (rotate-edge e3) (rotate-edge e4)))

#; {Edge Edge Edge Edge -> Configuration}
(define (create-configuration . edges)
  (apply configuration (sort edges < #:key edge-from)))

#; {Edge -> Edge}
(define (rotate-edge e)
  (match-define (edge from to) e)
  (define 90from (90degrees from))
  (define 90to   (90degrees to))
  (if (< 90from 90to) (edge 90from 90to) (edge 90to 90from)))

#; {Port -> Port}
(define (90degrees x)
  (modulo (+ x 2) 8))

(struct configuration [north east south west]
  #:transparent
  #:methods gen:equal+hash
  [(define equal-proc configuration-equality)
   (define hash-proc  (λ _ 42))  ;; consistent with equal 
   (define hash2-proc (λ _ 42))])

(struct edge (from to) #:transparent)

(define PORTS (build-list 8 identity))

#; {Port   = [0 .. 7]}
#; {Config = [config Edge Edge Edge Edge]
           where
           (1) every Port occurs once and no Port occurs twice
           (2) the four Edges are sorted according in ascending order according to their from Port}
#; {Edge   = (edge Port Port)
           where
           (*) the first Port is strictly smaller than the second}

;; A configuration represents a tile in a specific orientation. 

;; ---------------------------------------------------------------------------------------------------
;; generate all unique tlle configurations

;; see https://math.stackexchange.com/questions/1088313/how-many-different-tsuro-tiles-can-exist
;; for a group-theoretic justification of this number 

#; {[Listof Port] -> [Setof Config]}
;; for the given 2 * N ports, sorted in ascending order, create partial with N edges
;; generative: by removing 2 ports, we get closer to the empty case 
(define (all-configs)
  (define (all ports)
    (cond
      [(empty? ports) '(())]
      [else
       (define head (first ports))
       (define tail (rest ports))
       (append-map (λ (next) (map (curry cons (edge head next)) (all (remove next tail)))) tail)]))
  
  (for/set ((lc (all PORTS)))
    (apply configuration lc)))

(define all-tile-types (all-configs))

(unless (= (set-count all-tile-types) 35)
  (error 'tiles "wrong number of tiles generated: ~a" (set-count all-tile-types)))

;; ---------------------------------------------------------------------------------------------------
;; drawing tiles

(require pict)

(define PORT-COLOR "red")
(define TILE (rectangle 90 90))
(define PORTS-LOCATIONS
  '((28 00)
    (58 00)
    (86 28)
    (86 58)
    (58 86)
    (28 86)
    (00 58)
    (00 28)))

#; {Configuration -> Pict}
(define (configuration->pict c0)
  (match-define (configuration (edge a b) (edge c d) (edge e f) (edge g h)) c0)
  (define-values (sq dots) (make-tile-with-ports))
  (for/fold ((sq sq)) ((from-to (list (list a b) (list c d) (list e f) (list g h))))
    (match-define `(,from ,to) from-to)
    (define dot1 (list-ref dots from))
    (define dot2 (list-ref dots to))
    (pin-line sq dot1 cc-find dot2 cc-find #:color "blue")))

#; {-> (values Pict [Listof Pict])}
;; compute the tile with unique port points 
(define (make-tile-with-ports)
  (define (dt) (filled-rectangle 4 4 #:color PORT-COLOR))
  (for/fold ((sq TILE) (dots '())) ((x-y (reverse PORTS-LOCATIONS)))
    (match-define `(,x ,y) x-y)
    (define dot (dt))
    (values (pin-over sq x y dot) (cons dot dots))))

;; ---------------------------------------------------------------------------------------------------
(define frame (new frame% [label "hello"][width 800][height 800]))
(define canvas
  (new canvas%
       [parent frame]
       [paint-callback (λ (e dc) (draw-pict (configuration->pict (set-first all-tile-types)) dc 0 0))]))
(send frame show #t)
