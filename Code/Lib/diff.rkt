#lang racket

(provide diff)

(require "spy.rkt")

(module+ test
  (require rackunit))

(define (diff tree1 tree2)
  
  (define (check string=? x y accu)
    (if (string=? x y) #f (list x y accu)))

  (define (check-long vector-length vector->list x y accu)
    (or (if (= (vector-length x) (vector-length y)) #f (list x y accu))
        (for/or ((a (vector->list x)) (b (vector->list y)) (i (in-naturals)))
          (diff a b (cons i accu)))))

  (define (diff [tree1 tree1][tree2 tree2][Pth '()])
    (match* (tree1 tree2)
      [('() '()) #f]
      [((? number? x) (? number? y))   (check = x y Pth)]
      [((? symbol? x) (? symbol? y))   (check eq? x y Pth)]
      [((? string? x) (? string? y))   (check string=? x y Pth)]
      [((? char? x)   (? char? y))     (check eq? x y Pth)]
      [((? boolean? x) (? boolean? y)) (check eq? x y Pth)]
      [((? vector? x) (? vector? y))   (check-long vector-length vector->list x y (cons 'vector Pth))]
      [((? list? x)   (? list? y))     (check-long length values x y (cons 'list Pth))]
      [((? struct? x) (? struct? y))
       (define xv (struct->vector x))
       (define yv (struct->vector y))
       (diff xv yv (cons (list (vector-ref xv 0) (vector-ref yv 0)) Pth))]
      ;; let struct equality kick in first 
      [((? procedure? x)   (? procedure? y)) (check eq? x y Pth)]
      [(_   _) (list tree1 tree2)]))

  (diff))

(module+ test

  (check-false  (diff 1 1))
  (check-equal? (diff 1 2) (list 1 2 '()))

  (struct in  () #:transparent)
  (check-false (diff (in) (in)))

  (struct out ())
  (define ss (list (out) (out)))
  (check-equal? (apply diff ss) ss))