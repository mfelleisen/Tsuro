#lang scribble/manual

@(require Tsuro/Code/Common/tiles)
@(require (only-in racket ~a))

@title{Tsuro Sw Dev F19 Tile Types}

@(define tbl table)
@(define row (car tbl))
@(define (list-join l x)
   (if (null? l)
       '()
       (cons (car l)
             (let loop ([l (cdr l)])
               (cond
                 [(null? l) '()]
                 [else (list* x (car l) (loop (cdr l)))])))))

@tabular[
 #:sep @hspace[5]
 #:row-properties '(bottom-border bottom)
 @(list-join
    (cons
     (list @t{Tile Index}       @t{... as Image}  @t{... as Text}  )
     (map (λ (row) (list (t (~a (car row))) (cadr row) (t (~a (caddr row))))) tbl))
    @list[ @t{ }  @t{ }  @t{ }])]

