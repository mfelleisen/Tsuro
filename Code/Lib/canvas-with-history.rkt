#lang racket/gui

(provide
 (contract-out
  [history-canvas%
   (class/c
    (init-field
     [empty   pict?]            ;; the default image 
     [history (vectorof pict?)] ;; the maximal history 
     [inset   positive?])
    [set
     ;; add the given picture to this canvas at the end of the history 
     (->m pict? any/c)])]))

;; ---------------------------------------------------------------------------------------------------
(require pict)

;; ---------------------------------------------------------------------------------------------------
(define history-canvas%
  (class canvas% (init-field empty inset history)
    (inherit on-paint refresh-now)

    (super-new [paint-callback (λ (_e dc) (draw-pict picture dc inset inset))])
    
    (field [picture empty])
    (field [filled  0])
    (field [pointer 0])

    (define/public (set new-picture)
      (set! filled  (add1 filled))
      (set! pointer filled)
      (vector-set! history filled new-picture)
      (paint new-picture))

    (define/override (on-char e)
      (define direction 
        (case (send e get-key-code)
          [(right) (set! pointer (min (add1 pointer) 100))]
          [(left)  (set! pointer (max (sub1 pointer) 0))]
          [else    pointer]))
      (paint (vector-ref history pointer)))
      
    (define/private (paint new-picture)
      (set! picture new-picture)
      (refresh-now)
      (on-paint))))