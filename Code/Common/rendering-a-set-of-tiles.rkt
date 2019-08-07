#lang racket/gui

(require Tsuro/Code/Common/tiles)
(require pict)

;; ---------------------------------------------------------------------------------------------------
;; drawing all tiles in a set into a drawing context 

(define INSET  (+ 20 TILE-SIZE))
(define WIDTH  (+ INSET (* 10 TILE-SIZE) INSET))
(define HEIGHT (+ INSET (* 10 TILE-SIZE) INSET))

#;{ [Setof Configuration] (Instanceof Canvas%) -> Void }
(define (draw-tiles soc:config dc)
  (define loc:pict (for/list ((c (in-set soc:config))) (scale (configuration->pict c) 1.0)))
  (define one (colorize (first loc:pict) "red"))  (draw-pict one dc 10 150)
  (define two (colorize (second loc:pict) "red")) (draw-pict one dc (- WIDTH TILE-SIZE 10) 150)
  (define full
    (let loop ([l loc:pict][n (length loc:pict)])
      (cond
        [(< n 9) (apply hc-append l)]
        [else (vl-append (apply hc-append (take l 10)) (loop (drop l 10) (- n 10)))])))
  (draw-pict full dc INSET INSET))

;; ---------------------------------------------------------------------------------------------------
;; run 
;; -> Void 
(define (main)
  (define frame (new frame% [label "hello"][width WIDTH][height HEIGHT]))
  (define canvas
    (new canvas%
         [parent frame]
         [paint-callback (λ (e dc) (draw-tiles all-tile-types dc))]))
  (send frame show #t))

(main)
