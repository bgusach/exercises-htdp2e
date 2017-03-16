#lang htdp/bsl

(define-struct vec [x y])
; A Vec is a structure (make-vec PositiveNumber PositiveNumber)

; Any -> ...
(define (checked-make-vec x y)
  (cond
    [(not (number? x)) (error "make-vec: x must be a number ")]
    [(< x 0) (error "make-vec: x must be a positive number")]
    [(not (number? y)) (error "make-vec: y must be a number ")]
    [(< y 0) (error "make-vec: y must be a positive number")]
    [else (make-vec x y)]
    ))


(checked-make-vec 1 1)
(checked-make-vec -1 -1)
(checked-make-vec "lol" "troll")
