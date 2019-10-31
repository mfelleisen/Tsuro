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
  
  [matrix-clip  (->i ([m matrix?] [x (m) (<-rows m)] [y (m) (<-cols m)]) (r matrix?))]
  [matrix->rectangle (-> matrix? (listof list?))]
  [matrix-andmap (-> matrix? (-> any/c N N any) boolean?)]
  [matrix-where  (-> matrix? (-> any/c N N any) (-> any/c N N any) (listof any/c))]))

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

(define (matrix->rectangle m) m)

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
  (let loop ([i 0][m m0])
    (cond
      [(empty? m) '()]
      [(>= i x) '()]
      [else (cons (for/list ((j y) (n (first m))) n) (loop (+ i 1) (rest m)))])))

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

(define (matrix-andmap m f)
  (let loop ([l (matrix->rectangle m)][x 0])
    (cond
      [(empty? l) #true]
      [else
       (define row (first l))
       (define picts
         (for/and ((n (in-list row)) (y (in-naturals)))
           (f n x y)))
       (and picts (loop (rest l) (+ x 1)))])))

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

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define M (build-matrix 2 3 (λ (x y) (list y x))))
  (define H:M (H:build-matrix 2 3 (λ (x y) (list y x))))

  (define Mclipped (build-matrix 1 2 (λ (x y) (list y x))))
  (check-equal? (matrix-clip M 1 2) Mclipped)

  (check-equal? (matrix->rectangle M) (H:matrix->rectangle H:M))
  (check-equal? (H:matrix-ref H:M 1 2) (matrix-ref M 2 1) "1 2")
  
  (define M+1   (matrix-set M 0 1 'a))
  (define H:M+1 (H:matrix-set H:M 1 0 'a))

  (check-equal? (H:matrix-ref H:M+1 1 0) (matrix-ref M+1 0 1) "2 1")
  (check-equal? (matrix->rectangle M+1) (H:matrix->rectangle H:M+1) "2 1/complete")
  (check-equal? (matrix-map M (λ (M-at-x-y y z) M-at-x-y)) (apply append M))

  (define K (matrix-set M 0 0 #f))
  (check-equal? (matrix-where K (λ (M-at-x-y x y) M-at-x-y) (λ (M-at-x-y x y) (list x y)))
                (apply append (rest (first K)) (rest K)))
  
  (check-true (matrix-andmap M (λ (p x y) (>= (apply + p) 0))))
  (check-false (matrix-andmap M (λ (p x y) (not (= (first p) 0))))))
