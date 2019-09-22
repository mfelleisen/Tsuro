#lang racket

;; what actions can the player perform after being handed the current board state

(provide
 init-action/c
 turn-action/c)

;; -----------------------------------------------------------------------------
(require Tsuro/Code/Common/grid)
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/port)

;; -----------------------------------------------------------------------------
(define init-action/c (list/c (list/c tile-index? degree?) port? index? index?))

(define turn-action/c (list/c tile-index? degree?))