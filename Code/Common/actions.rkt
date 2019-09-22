#lang racket

;; what actions can the player perform after being handed the current board state

(provide
 init-action/c
 turn-action/c)

;; -----------------------------------------------------------------------------
(require Tsuro/Code/Common/board)
(require Tsuro/Code/Common/tiles)

;; -----------------------------------------------------------------------------
(define init-action/c player-on-tile/c)

(define turn-action/c (list/c tile-index? degree?))