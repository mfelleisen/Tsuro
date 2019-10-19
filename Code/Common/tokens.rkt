#lang racket

(provide
 TOKEN-COLORS

 ;; all tokens come in a distinct color
 color?)

;; -----------------------------------------------------------------------------
(define TOKEN-COLORS '("white" "black" "red" "green" "blue")) ;; colors only
(define (color? x) (cons? (member x TOKEN-COLORS)))