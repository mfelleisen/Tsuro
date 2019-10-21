#lang racket

(provide
 AVATAR-COLORS

 ;; all tokens come in a distinct color
 color?)

;; -----------------------------------------------------------------------------
(define AVATAR-COLORS '("white" "black" "red" "green" "blue")) ;; colors only
(define (color? x) (cons? (member x AVATAR-COLORS)))