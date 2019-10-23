#lang racket

(provide
 AVATAR-COLORS

 ;; all tokens come in a distinct color
 avatar?)

;; -----------------------------------------------------------------------------
(define AVATAR-COLORS '("white" "black" "red" "green" "blue")) ;; colors only
(define (avatar? x) (cons? (member x AVATAR-COLORS)))