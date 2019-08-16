#lang racket

;; determine the set of unique Tsuro tiles 

(define TILES# 35)

(provide
 #; {type TileIndex = [0 .. TILES#]}
 #; {type Edge = [List Port Port]}

 TILES#
 
 #; [-> [Listof [List Nat Pict [List Edge Edge Edge Edge]]]]
 all-tiles->table

 #; {TileIndex -> Tile}
 tile-index->tile

 ;; [Setof Tile]
 all-tile-types)

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/port-alphabetic)

(module+ test (require rackunit))

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
    (create-tile lc)))

(define all-tile-types (all-configs))

(module+ test 
  (check-equal? (set-count all-tile-types) TILES# "wrong number of tiles generated: ~a"))

(define (all-tiles->table)
  (for/list ((t (in-list (set->list all-tile-types))) (i (in-naturals)))
    (list i (tile->pict t) (vector-ref (struct->vector t) 1))))

(define table (all-tiles->table))
(define (tile-index->tile i)
  (define r (list-ref table i))
  (create-tile (third r)))
