#lang racket

;; a "DSL" for writing down initialization and intermediate boards for tests 

(provide
 ;; SYNTAX 
 #; (board-from-tiles (S ...) ...)
 #; (S =  #f
       || I
       || (I PlayerName _on_ Port))
 #; {I = 0 .. TILES#}
 ;; creates a list of tile placements from which initialze and intermediate (in board) creates states 
 board-from-tiles on)

;; ---------------------------------------------------------------------------------------------------
(require Tsuro/Code/Common/tiles)
(require Tsuro/Code/Common/distinct-tiles)
(require Tsuro/Code/Common/port-alphabetic)
(require (for-syntax syntax/parse))

;; ---------------------------------------------------------------------------------------------------
(begin-for-syntax
  (define-syntax-class degree [pattern 90][pattern 180][pattern 270])
    
  (define-syntax-class tile-index/or-tile-index-with-player
    (pattern (~datum #f)
             #:with square #'#f)
    (pattern (index (~optional (~seq #:rotate r:degree) #:defaults ([r #'0])) name (~literal on) port)
             #:declare name  (expr/c #'string?)
             #:declare port  (expr/c #'port?)
             #:declare index (expr/c #'(</c TILES#))
             #:with tile   #'(rotate-tile (tile-index->tile index) #:degree r)
             #:with square #'`(,tile ,name ,port.c))
    (pattern index
             #:declare index (expr/c #'(</c TILES#))
             #:with tile   #'(tile-index->tile index.c)
             #:with square #'`(,tile))))

(define-syntax (on stx) (raise-syntax-error 'on "used out of context" stx))

(define-syntax (board-from-tiles stx)
  (syntax-parse stx
    [(_ (t:tile-index/or-tile-index-with-player ...) ...)
     #'(board-from-tiles/proc (list (list t.square ...) ...))]))
 
(define (board-from-tiles/proc rectangle)
  (define (f row i)
    (for/list ((cell (in-list row)) (j (in-naturals)) #:when cell)
      (append cell (list j i))))
  (apply append (for/list ((row (in-list rectangle)) (i (in-naturals))) (f row i))))