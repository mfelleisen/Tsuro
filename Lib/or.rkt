#lang racket

(provide or~)

(require (for-syntax syntax/parse))

(define-syntax (or~ stx)
  (syntax-parse stx
    [(_)
     #'#f]
    [(_ (~datum #:let) (~var x id) (~var rhs expr) rst ...)
     #'(let ([x rhs]) (or~ rst ...))]
    [(_ e1 e2 ...)
     #'(or e1 (or~ e2 ...))]))

(or~)
(or~ #:let x 1 x)
(or~ #f #:let x #f x)
(or~ #f #:let x #t x #f)
