#lang htdp/isl+

; ### Functions

((lambda (x) x) (lambda (x) x))

((lambda (x) (x x)) (lambda (x) x))

((lambda (x) (x x)) (lambda (x) (x x)))