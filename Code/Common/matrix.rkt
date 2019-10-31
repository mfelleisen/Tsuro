#lang racket

(define N natural-number/c)
(define matrix? any/c)
(define (<-cols m) (and/c N (</c (matrix-cols m))))
(define (<-rows m) (and/c N (</c (matrix-rows m))))

(provide
 (contract-out
  [build-matrix (-> (and/c N (>/c 0)) (and/c N (>/c 0)) (-> N N any) matrix?)]

  ;; these operations are NOT the ordinary Matrix operations but Cartesian point access operations. 
  [matrix-set   (->i ([m matrix?] [x (m) (<-cols m)] [y (m) (<-rows m)] [new any/c]) (r matrix?))]
  [matrix-ref   (->i ([m matrix?] [x (m) (<-cols m)] [y (m) (<-rows m)]) (r any/c))]

  ;; but these guys use plain matrix orientation
  ;; this mess must be cleaned up eventually 
  [matrix-clip  (->i ([m matrix?] [x (m) (<-rows m)] [y (m) (<-cols m)]) (r matrix?))]
  [matrix-andmap (-> matrix? (-> any/c N N any) boolean?)]
  [matrix-fold   (-> matrix? any/c (-> any/c any/c any) (-> list? any) any)]
  [matrix-map    (-> matrix? (-> any/c N N any) matrix?)]))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require (submod ".."))
  (require (prefix-in H: (only-in htdp/matrix matrix-set matrix-ref build-matrix  matrix->rectangle)))
  (require rackunit))

(require SwDev/Debugging/spy)

;; ---------------------------------------------------------------------------------------------------
#; {type [Matrix X] = [NEListof [NEListof X]]}

;; ---------------------------------------------------------------------------------------------------
(define (build-matrix x y f)
  (build-list x (λ (ix) (build-list y (λ (iy) (f ix iy))))))

(define (matrix-rows m) (length m))
(define (matrix-cols m) (or (empty? m) (length (first m))))

;; ---------------------------------------------------------------------------------------------------
(define (matrix-ref m x y)
  (define row (list-ref m y))
  (list-ref row x))

;; ---------------------------------------------------------------------------------------------------
(define (matrix-set m x y new)
  (define row (list-ref m y))
  (define row+1 (replace row x new))
  (replace m y row+1))

(define (replace M i new)
  (append (take M i) (list new) (drop M (+ i 1))))

;; ---------------------------------------------------------------------------------------------------
(define (matrix-clip m0 x y)
  (for/list ([row (in-list m0)][i (in-range x)])
    (for/list ((n row) (j y)) n)))

(define (matrix-fold squares e row-f col-f)
  (foldr row-f e (map col-f squares)))

(define (matrix-map m f)
  (for/list ([row (in-list m)][x (in-naturals)])
    (for/list ((n (in-list row)) (y (in-naturals)))
      (f n x y))))

(define (matrix-andmap m f)
  (for/and ((row (in-list m)) (x (in-naturals)))
    (for/and ((n (in-list row)) (y (in-naturals)))
      (f n x y))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define M (build-matrix 2 3 (λ (x y) (list y x))))
  (define H:M (H:build-matrix 2 3 (λ (x y) (list y x))))

  (define Mclipped (build-matrix 1 2 (λ (x y) (list y x))))
  (check-equal? (matrix-clip M 1 2) Mclipped)
  
  (check-equal? (H:matrix-ref H:M 1 2) (matrix-ref M 2 1) "1 2")
  
  (define M+1   (matrix-set M 0 1 'a))
  (define H:M+1 (H:matrix-set H:M 1 0 'a))

  (check-equal? (H:matrix-ref H:M+1 1 0) (matrix-ref M+1 0 1) "2 1")
  (check-equal? (matrix-map M (λ (M-at-x-y y z) M-at-x-y)) M)
  
  (check-true (matrix-andmap M (λ (p x y) (>= (apply + p) 0))))
  (check-false (matrix-andmap M (λ (p x y) (not (= (first p) 0))))))
