#lang racket

(provide
  #; { Pict Pict (Pict Pict -> (values Integer Integer)) Integer Integer -> (values Integer Integer)}
  ;; find sub-pict in pict via finder, then add dx and dy to each coordinate
  d+)

;; ---------------------------------------------------------------------------------------------------
(define (d+ pict sub-pict finder dx dy)
  (define-values (x y) (finder pict sub-pict))
  (values (+ dx x) (+ dy y)))
