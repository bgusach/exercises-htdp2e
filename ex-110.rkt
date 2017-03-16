#lang htdp/bsl

(define (checked-area-of-disk r)
  (cond
    [(not(number? r)) (error "area-of-disk: number expected")]
    [(< r 0) (error "area-of-disk: number must be positive")]
    [else (area-of-disk r)]
    ))


(define (area-of-disk r)
  (* 
    3.141592 
    (sqr r)
    ))

(checked-area-of-disk 10)
(checked-area-of-disk "ten")
