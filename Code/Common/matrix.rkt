#lang racket

(define N natural-number/c)
(define matrix? any/c)
(define (good-x m) (and/c N (</c (length m))))
(define (good-y m) (and/c N (</c (length (first m)))))

(provide
 (contract-out
  [build-matrix (-> N N (-> N N any) matrix?)]
  [matrix-set   (->i ([m matrix?] [x (m) (good-x m)] [y (m) (good-y m)] [new any/c]) (r matrix?))]
  [matrix-ref   (->i ([m matrix?] [x (m) (good-x m)] [y (m) (good-y m)]) (r any/c))]
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

#; {type [Matrix X] = [Listof [Listof X]]}

(module+ matrix+
  (require (prefix-in H: (only-in htdp/matrix matrix-set matrix-ref build-matrix  matrix->rectangle)))
  (require rackunit)

  (struct matrix2 [x y rec vec] #:transparent)
  #; {type [Matrix X] = (matrix n m (Vector-of X))}

  (define (build-matrix x y f)
    (define r (build-list x (λ (ix) (build-list y (λ (iy) (f ix iy))))))
    (define m (apply vector (apply append r)))
    (matrix2 x y r m))

  (define (matrix-ref m x y)
    (match-define (matrix2 x0 y0 _ v) m)
    (vector-ref v (+ (* x (+ y 1)) y)))

  (define matrix->rectangle matrix2-rec)

  (define M (build-matrix 2 3 (λ (x y) (list y x))))
  (define H:M (H:build-matrix 2 3 (λ (x y) (list y x))))

  M
  
  (check-equal? (matrix->rectangle M) (H:matrix->rectangle H:M))
  (check-equal? (H:matrix-ref H:M 1 2) (matrix-ref M 1 2) "1 2")
  
  ; (define M+1   (matrix-set M 1 0 'a))

  

  )
  

;; ---------------------------------------------------------------------------------------------------
(define (build-matrix x y f)
  (build-list x (λ (ix) (build-list y (λ (iy) (f ix iy))))))

(define (matrix->rectangle m) m)

;; ---------------------------------------------------------------------------------------------------
(define (matrix-ref m x y)
  (list-ref (list-ref m x) y))

;; ---------------------------------------------------------------------------------------------------
(define (matrix-set m x y new)
  (define row (list-ref m x))
  (define row+1 (replace row y new)#;
    (if (= y 0) (cons new (rest row)) (append (take row y) (list new) (drop row y))))
  (replace m x row+1))

(define (replace M i new)
  (append (take M i) (list new) (drop M (+ i 1))))

;; ---------------------------------------------------------------------------------------------------
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
           (f n x y)))
       (append picts (loop (rest l) (+ x 1)))])))


;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define M (build-matrix 2 3 (λ (x y) (list y x))))
  (define H:M (H:build-matrix 2 3 (λ (x y) (list y x))))
  
  (check-equal? (matrix->rectangle M) (H:matrix->rectangle H:M))
  (check-equal? (H:matrix-ref H:M 1 2) (matrix-ref M 1 2) "1 2")
  
  (define M+1   (matrix-set M 1 0 'a))
  (define H:M+1 (H:matrix-set H:M 1 0 'a))

  (check-equal? (H:matrix-ref H:M+1 1 0) (matrix-ref M+1 1 0) "2 1")
  (check-equal? (matrix->rectangle M+1) (H:matrix->rectangle H:M+1) "2 1/complete")
  (check-equal? (matrix-map M (λ (M-at-x-y y z) M-at-x-y)) (apply append M))

  (define K (matrix-set M 0 0 #f))
  (check-equal? (matrix-where K (λ (M-at-x-y x y) M-at-x-y) (λ (M-at-x-y x y) (list x y)))
                ;; because M lists x and y in reverse 
                (map reverse (apply append (rest (first K)) (rest K))))
  
  (check-true (matrix-andmap M (λ (p x y) (>= (apply + p) 0))))
  (check-false (matrix-andmap M (λ (p x y) (not (= (first p) 0))))))
