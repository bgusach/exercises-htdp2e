#lang htdp/bsl


(define (light? x)
  (cond
    [(string? x) 
     (or
       (string=? x "red")
       (string=? x "green")
       (string=? x "yellow")
       )]
    [else #false]
    ))


(define (light=? v1 v2)
  (cond
    [(not (light? v1)) (error "light=?: first value is not light?")]
    [(not (light? v2)) (error "light=?: second value is not light?")]
    [else (string=? v1 v2)]
    ))

