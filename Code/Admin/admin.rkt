#lang racket

(module+ test (require rackunit))

(define GAME# 5)

#; {[Listof Player] -> [Listof [Listof Player]]}
;; lop contains GAME# or more players 
(define (prepare-games lop)
  ;; GEN REC
  (define first-game (take lop GAME#))
  (let loop ([lop (drop lop GAME#)][games (list first-game)])
    (case (length lop)
      [(0) (reverse games)]
      [(1)
       (define lop-1 (append (first games) lop))
       (list* (take lop-1 (- GAME# 2)) (drop lop-1 (- GAME# 2)) (rest games))]
      [(2)
       (define lop-1 (append (first games) lop))
       (list* (take lop-1 (- GAME# 1)) (drop lop-1 (- GAME# 1)) (rest games))]
      [(3 4) (reverse (cons lop games))]
      [else (loop (drop lop GAME#) (cons (take lop GAME#) games))])))

(module+ test
  (check-equal? (prepare-games '(a b c d e f)) '[(a b c) (d e f)])
  (check-equal? (prepare-games '(a b c d e f g)) '[(a b c d) (e f g)])
  (check-equal? (prepare-games '(a b c d e f g h)) '[(a b c d e) (f g h)])
  (check-equal? (prepare-games '(a b c d e f g h i j)) '[(a b c d e) (f g h i j)]))