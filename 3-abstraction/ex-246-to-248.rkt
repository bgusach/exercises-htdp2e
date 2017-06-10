#lang htdp/isl


(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else 
      (cond
        [(R (first l) t)
         (cons (first l)
           (extract R (rest l) t)
           )]
        [else (extract R (rest l) t)]
        )]))

(extract < '(6 4) 5)
(extract < '(8 4) 5)


; Number Number -> Boolean
; is the area of a square with side x larger than c??
(define (squared>? x c)
  (> (* x x) c))

(squared>? 3 10)
(squared>? 4 10)
(extract squared>? '(3 4 5) 10)

