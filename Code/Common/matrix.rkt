#lang racket

(provide
 build-matrix
 matrix-set
 matrix-ref
 matrix->rectangle)

(require (only-in htdp/matrix build-matrix matrix->rectangle))
(require (prefix-in htdp: (only-in htdp/matrix matrix-set matrix-ref)))

(define (matrix-ref m x y) (htdp:matrix-ref m y x))
(define (matrix-set m x y new) (htdp:matrix-set m y x new))