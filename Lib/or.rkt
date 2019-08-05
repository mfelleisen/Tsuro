#lang racket

;; an or form that supports a kind of local definition of identifiers and values 

(provide
 ;; SYNTAX
 ;; e = ... | (or~ {e | #:let x e}*}
 ;; a #:let x e phrase evaluates e, binds the value to x, and
 ;; evaluates the rest of the sub-expressions in this lexical context
 or~)

;; -----------------------------------------------------------------------------
(require (for-syntax syntax/parse))
(module+ test (require rackunit))

;; -----------------------------------------------------------------------------
(define-syntax (or~ stx)
  (syntax-parse stx
    [(_)
     #'#f]
    [(_ (~datum #:let) (~var x id) (~var rhs expr) rst ...)
     #'(let ([x rhs]) (or~ rst ...))]
    [(_ e1 e2 ...)
     #'(or e1 (or~ e2 ...))]))

(module+ test 
  (check-equal? (or~) (or))
  (check-equal? (or~ #:let x 1 x) (or 1))
  (check-equal? (or~ #f #:let x #f x) (or #f #f))
  (check-equal? (or~ #f #:let x 1 x #f) (or #f 1 #f)))