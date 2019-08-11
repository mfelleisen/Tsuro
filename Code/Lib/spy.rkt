#lang racket

(define-syntax spy
  (lambda (exp)
    (syntax-case exp ()
      [(spy e)
       (with-syntax ([the-expr (syntax e)]
                     [the-src (syntax-source (syntax e))]
                     [the-loc (syntax-position #'e)])
         (syntax
          (let ([val e])
            (printf "Expression~n~a~nat ~a:~a~nevaluated to~n~v~n~n"
                    (quote the-expr)
                    (quote the-src)
                    (quote the-loc)
                    val)
            val)))])))

(+ 4 (spy (+ 1 2)))