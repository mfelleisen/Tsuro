#lang racket/gui

;; how many unique tiles are there, and what do they look like 

(require Tsuro/Code/Lib/or)

;; ---------------------------------------------------------------------------------------------------
;; data representation of tiles

#| The information world consists of tiles with ports and connections between ports. 

   A blank tile looks as follows: 

        0    1
    +---|----|---+
    |            |
  7 -            - 2
    |            |
  6 -            - 3
    |            |
    +---|----|---+
        5    4

   Each labeled line crossing a border is a _port_.
   A complete configuration connects four pairs of disttinct ports with each other:

    +---|----|---+    
    |  /     |   |    
    --      +-----    
    |      / |   |    
    ------/--|----    
    |    /   |   |    
    +---|----|---+    
   
   When two tile configurations are the same after a rotation of 90, 180, or 270 degrees,
   they are considered equivalent. Here is a 90 degree rotation of the above configuration: 

   +---|----|---+
   |   |     \  |
   ----|-      --
   |   |  \     |
   ----|---\-----
   |   |    |   |
   +---|----|---+
|#

(define PORTS (build-list 8 identity))

#; {Port   = [0 .. 7]}
#; {Config = [config [List Edge Edge Edge Edge]]
           where
           (1) every Port occurs once and no Port occurs twice
           (2) the four Edges are sorted according in ascending order according to their from Port}
#; {Edge   = (list Port Port)
           where
           (*) the first Port is strictly smaller than the second}

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
  (match-define (configuration (list e1 e2 e3 e4)) c0)
  (create-configuration (map rotate-edge (list e1 e2 e3 e4))))

#; {[List Edge Edge Edge Edge] -> Configuration}
(define (create-configuration edges)
  (configuration (sort edges < #:key edge-from)))

#; {Edge -> Edge}
(define (rotate-edge e)
  (define 90from (90degrees (edge-from e)))
  (define 90to   (90degrees (edge-to e)))
  (if (< 90from 90to) (edge 90from 90to) (edge 90to 90from)))

#; {Port -> Port}
(define (90degrees x)
  (modulo (+ x 2) 8))

(struct configuration [lo4edges]
  #:transparent
  #:methods gen:equal+hash
  [(define equal-proc configuration-equality)
   (define hash-proc  (λ _ 42))  ;; consistent with equal 
   (define hash2-proc (λ _ 42))])

; (struct edge (from to) #:transparent)
(define edge list)
(define edge-from first)
(define edge-to second)

;; ---------------------------------------------------------------------------------------------------
;; generate all unique tlle configurations

;; see https://math.stackexchange.com/questions/1088313/how-many-different-tsuro-tiles-can-exist
;; for a group-theoretic justification of this number 
;; see https://en.wikipedia.org/wiki/Group_(mathematics)#Second_example:_a_symmetry_group
;; for some basic background on symmetric groups (which is what these tiles are called)

#; {-> [Setof Config]}
;; create the set of distinnct tile configurations 
(define (all-configs)
  #; {[Listof Port] -> [Setof Config]}
  ;; for the given 2 * N ports, sorted in ascending order, create partial with N edges
  ;; GENERATIVE by removing 2 ports, we get closer to the empty case 
  (define (all ports)
    (cond
      [(empty? ports) '(())]
      [else
       (define head (first ports))
       (define tail (rest ports))
       (append-map (λ (next) (map (curry cons (edge head next)) (all (remove next tail)))) tail)]))
  
  (for/set ((lc (all PORTS)))
    (configuration lc)))

(define all-tile-types (all-configs))

(unless (= (set-count all-tile-types) 35)
  (error 'tiles "wrong number of tiles generated: ~a" (set-count all-tile-types)))

;; ---------------------------------------------------------------------------------------------------

(require pict)

;; ---------------------------------------------------------------------------------------------------
;; converting a configuration into a pict 

(define TILE-WIDTH 90)
(define TILE-HEIGHT TILE-WIDTH)

(define PORT-COLOR "red")
(define TILE (rectangle TILE-WIDTH TILE-HEIGHT))

(define PORTS-LOCATIONS ;; somehow connect to PORTS
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
  (define-values (sq dots) (make-tile-with-ports))
  (for/fold ((sq sq)) ((edge (configuration-lo4edges c0)))
    (define from (edge-from edge))
    (define to   (edge-to edge))
    (define dot1 (list-ref dots from))
    (define dot2 (list-ref dots to))
    (pin-curve sq dot1 dot2)))

(define (d+ pict sub-pict finder dx dy)
  (define-values (x y) (finder pict sub-pict))
  (values (+ dx x) (+ dy y)))

#; {Pict Pict Pict -> Pict}
(define (pin-curve sq dot1 dot2)
  (cc-superimpose sq
                  (dc (λ (dc dx dy)
                        (define-values (x1 y1) (d+ sq dot1 cc-find dx dy))
                        (define-values (x2 y2) (d+ sq dot2 cc-find dx dy))
                        (define-values (xc yc) (d+ sq sq cc-find dx dy))
                        (define old-pen (send dc get-pen))
                        ;; ------------------------------------------------------------------
                        (send dc set-pen (new pen% [width 3] [color "blue"]))
                        (send dc draw-spline x1 y1 xc yc x2 y2)
                        ;; ------------------------------------------------------------------
                        (send dc set-pen old-pen))
                      TILE-WIDTH TILE-HEIGHT)))

#; {-> (values Pict [Listof Pict])}
;; compute the tile with unique port points 
(define (make-tile-with-ports)
  (define (dt) (filled-rectangle 4 4 #:color PORT-COLOR))
  (for/fold ((sq TILE) (dots '())) ((x-y (reverse PORTS-LOCATIONS)))
    (match-define `(,x ,y) x-y)
    (define dot (dt))
    (values (pin-over sq x y dot) (cons dot dots))))

;; ---------------------------------------------------------------------------------------------------
;; drawing all tiles in a set into a drawing context 

(define INSET  (+ 20 TILE-WIDTH))
(define WIDTH  (+ INSET (* 10 TILE-WIDTH) INSET))
(define HEIGHT (+ INSET (* 10 TILE-HEIGHT) INSET))

#;{ [Setof Configuration] (Instanceof Canvas%) -> Void }
(define (draw-tiles soc:config dc)
  (define loc:pict (for/list ((c (in-set soc:config))) (scale (configuration->pict c) 1.0)))
  (define one (colorize (first loc:pict) "red"))  (draw-pict one dc 10 150)
  (define two (colorize (second loc:pict) "red")) (draw-pict one dc (- WIDTH TILE-WIDTH 10) 150)
  (define full
    (let loop ([l loc:pict][n (length loc:pict)])
      (cond
        [(< n 9) (apply hc-append l)]
        [else (vl-append (apply hc-append (take l 10)) (loop (drop l 10) (- n 10)))])))
  (draw-pict full dc INSET INSET))

(define (main)
  (define frame (new frame% [label "hello"][width WIDTH][height HEIGHT]))
  (define canvas
    (new canvas%
         [parent frame]
         [paint-callback (λ (e dc) (draw-tiles all-tile-types dc))]))
  (send frame show #t))

(main)
