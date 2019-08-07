#lang racket/gui

(provide
 TILE-SIZE
 PORTS
 
 configuration
 edge
 
 configuration->pict)

;; ---------------------------------------------------------------------------------------------------
;; how many unique tiles are there, and what do they look like 

(require Tsuro/Code/Common/port-alphabetic) #;(require Tsuro/Code/Common/port-digits)
(require Tsuro/Code/Lib/or)
(require Tsuro/Code/Lib/finder)
(require pict)

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

   Each labeled line crossing a border is a _port_. Ports can be called anything; see port-signature. 

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
  (create-configuration (map rotate-edge (configuration-lo4edges c0))))

#; {[List Edge Edge Edge Edge] -> Configuration}
(define (create-configuration edges)
  (configuration (sort edges <-port #:key edge-from)))

#; {Edge -> Edge}
(define (rotate-edge e)
  (define 90from (90degrees (edge-from e)))
  (define 90to   (90degrees (edge-to e)))
  (if (<-port 90from 90to) (edge 90from 90to) (edge 90to 90from)))

(struct configuration [lo4edges]
  #:transparent
  #:methods gen:equal+hash
  [(define equal-proc configuration-equality)
   (define hash-proc  (λ _ 42))  ;; consistent with equal 
   (define hash2-proc (λ _ 42))])

#; {simulate (struct edge (from to) #:prefab)}
(define edge list)
(define edge-from first)
(define edge-to second)

;; two unique examples
(define one-from (map index->port '(0 2 4 6)))
(define one-to   (map index->port '(1 3 5 7)))
(define one (configuration (map list one-from one-to)))

(define two-from (map index->port '(0 1 2 4)))
(define two-to   (map index->port '(6 7 3 5)))
(define two (configuration (map list two-from two-to)))

(define 90two-from (map index->port '(0 1 4 6)))
(define 90two-to   (map index->port '(2 3 5 7)))
(define 90two (configuration (map list 90two-from 90two-to)))

(equal? 90two two)

90two
two

;; ---------------------------------------------------------------------------------------------------
;; converting a configuration into a pict: make every size depend on the TILE-SIZE

(define TILE-SIZE 90) (unless (= (remainder TILE-SIZE 30) 0) (error 'TILE-SIZE "bad choice for size"))
(define TILE (rectangle TILE-SIZE TILE-SIZE))

(define PORT-COLOR "red")
(define PORT-RADIUS (/ TILE-SIZE 30))
(define PORT-DIAMETER (* PORT-RADIUS 2))
(define (PORT) (filled-ellipse PORT-DIAMETER PORT-DIAMETER #:color PORT-COLOR))

(define 0-THIRD 0)
(define 1-THIRD (/ TILE-SIZE 3))
(define 2-THIRD (* 2 1-THIRD))
(define 3-THIRD TILE-SIZE )

(define PORT-LOCATIONS
  `((,(- 1-THIRD PORT-RADIUS) ,(- 0-THIRD PORT-RADIUS))
    (,(- 2-THIRD PORT-RADIUS) ,(- 0-THIRD PORT-RADIUS))
    (,(- 3-THIRD PORT-RADIUS) ,(- 1-THIRD PORT-RADIUS))
    (,(- 3-THIRD PORT-RADIUS) ,(- 2-THIRD PORT-RADIUS))
    (,(- 2-THIRD PORT-RADIUS) ,(- 3-THIRD PORT-RADIUS))
    (,(- 1-THIRD PORT-RADIUS) ,(- 3-THIRD PORT-RADIUS))
    (,(- 0-THIRD PORT-RADIUS) ,(- 2-THIRD PORT-RADIUS))
    (,(- 0-THIRD PORT-RADIUS) ,(- 1-THIRD PORT-RADIUS))))

(define EDGE-COLORS
  (for/list ((___e (configuration-lo4edges one)) (j(in-naturals)))
    (define i (+ j 1)) ;; these numbers might as well be random 
    (make-color (modulo (* i 111) 256) (modulo (* i 77) 256) (modulo (* i 33) 256))))

#; {Configuration -> Pict}
(define (configuration->pict c0)
  (define-values (sq dots) (make-tile-with-ports))
  (for/fold ((sq sq)) ((edge (configuration-lo4edges c0)) (color EDGE-COLORS))
    (define from (port->index (edge-from edge)))
    (define to   (port->index (edge-to edge)))
    (define dot1 (list-ref dots from))
    (define dot2 (list-ref dots to))
    (pin-curve sq dot1 dot2 color)))

#; {Pict Pict Pict (Instanceof Color%) -> Pict}
(define (pin-curve sq dot1 dot2 color)
  #; { (Instanceof DC<%) Integer Integer -> Void }
  ;; add a spline from dot1 to dot2 via the center of sq 
  (define (draw-connections dc dx dy)
    (define old-pen (send dc get-pen))
    ;; --------------------------------------------------
    (define-values (x1 y1) (d+ sq dot1 cc-find dx dy))
    (define-values (x2 y2) (d+ sq dot2 cc-find dx dy))
    (define-values (xc yc) (d+ sq sq cc-find dx dy))
    (send dc set-pen (new pen% [width 3] [color color]))
    (send dc draw-spline x1 y1 xc yc x2 y2)
    ;; --------------------------------------------------
    (send dc set-pen old-pen))
  (define connections (dc draw-connections TILE-SIZE TILE-SIZE))
  (cc-superimpose sq connections))

#; {-> (values Pict [Listof Pict])}
;; generate the tile with unique port points 
(define (make-tile-with-ports)
  (for/fold ((sq TILE) (dots '())) ((x-y (reverse PORT-LOCATIONS)))
    (match-define `(,x ,y) x-y)
    (define dot (PORT))
    (values (pin-over sq x y dot) (cons dot dots))))

;; ---------------------------------------------------------------------------------------------------
;; visualize
#;
(vc-append 
 (apply hc-append (map (λ (c) (scale (configuration->pict c) 3.0)) (list one two one)))
 (apply hc-append (map (λ (c) (scale (configuration->pict c) 3.0)) (list one two one))))


(map (λ (c) (scale (configuration->pict c) 3.0)) (list two 90two))