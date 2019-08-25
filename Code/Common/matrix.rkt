#lang racket

(provide
 build-matrix
 matrix-set
 matrix-ref
 matrix->rectangle
 matrix-where)

(require (only-in htdp/matrix build-matrix matrix->rectangle))
(require (prefix-in htdp: (only-in htdp/matrix matrix-set matrix-ref)))
(module+ test (require rackunit))

(define (matrix-ref m x y) (htdp:matrix-ref m y x))
(define (matrix-set m x y new) (htdp:matrix-set m y x new))

(define (matrix-map m f)
  (let loop ([l (matrix->rectangle m)][x 0])
    (cond
      [(empty? l) '()]
      [else
       (define row (first l))
       (define picts
         (for/list ((n (in-list row)) (y (in-naturals)))
           (f n x y)))
       (append picts (loop (rest l) (+ x 1)))])))

(define (matrix-where m p f)
  (let loop ([l (matrix->rectangle m)][x 0])
    (cond
      [(empty? l) '()]
      [else
       (define row (first l))
       (define picts
         (for/list ((n (in-list row)) (y (in-naturals)) #:when (p n x y))
           (f n y x)))
       (append picts (loop (rest l) (+ x 1)))])))

(module+ test
  (define M (build-matrix 2 2 (λ (y x) (list x y))))
  (check-equal? (matrix-map M (λ (x y z) x)) '((0 0) (1 0) (0 1) (1 1)))

  (define K (matrix-set M 0 0 #f))
  (check-equal? (matrix-where K (λ (x y z) x) (λ (x y z) x)) '((1 0) (0 1) (1 1))))
