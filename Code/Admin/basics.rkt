#lang racket

(provide
 enough?
 distinct?
 
 MIN-PLAYERS
 MAX-PLAYERS)

;; -----------------------------------------------------------------------------
(module+ test
  (require rackunit))

;; -----------------------------------------------------------------------------
#; {[Listof X] -> Boolean}
(define (enough? l)
  (<= MIN-PLAYERS (length l) MAX-PLAYERS))

#; {[Listof X] -> Boolean}
(define (distinct? names)
  (= (set-count (apply seteq names)) (length names)))

(define MIN-PLAYERS 3)
(define MAX-PLAYERS 5)

;; -----------------------------------------------------------------------------
(module+ test

  (check-true (enough? '(a b c)))
  (check-false (enough? '(a b c d e f)))
  (check-false (enough? '(a b)))
  
  (check-true (distinct? '(a b c)))
  (check-false (distinct? '("a" "b" "c" "a")))

  (check-true (distinct? (list (box 1) (box 1) (box 1))))
  (define same (box 1))
  (check-false (distinct? (list same (box 1) same))))