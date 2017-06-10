#lang htdp/isl

(define (f x) x)

(cons f '())

(f f)

(cons f (cons 10 (cons (f 10) '())))

