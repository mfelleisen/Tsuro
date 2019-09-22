#lang racket

;; ---------------------------------------------------------------------------------------------------
;; A player gets handed a board representation and responds with an action.

;; An init-action/c action specifies a tile placement
;; and on which port of the tile the avatar is placed.
;; It is legal if the tile
;; -- is placed at the periphery of the board
;; -- does not neighbor an already placed tile in a cardinal direction
;; -- the player faces an empty square on the board.

;; INCONSISTENCY: here I specify a tile, including the rotation, instead of an index plus a rotation. 

;; ---------------------------------------------------------------------------------------------------
;; A  turn-action/c merely specifies a tile index and a rotation.
;; It is legal if placing the tile does not lead to
;; -- the suicide of the player's avatar 
;; -- the infinite looping of any avatar, including the player's
;; -- the collision of two (or more) player avatars on the same spot.

