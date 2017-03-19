#lang htdp/bsl

; A List-of-amounts is one of   <-------------
; - '()                                      |
; - (cons PositiveNumber List-of-amounts)    |
;                         ^                  |
;                         |-------------------


; List-of-amounts -> PositiveNumber
; sums all the amounts of a list
(check-expect (sum '()) 0)
(check-expect (sum (cons 10 '())) 10)
(check-expect (sum (cons 5 (cons 10 '()))) 15)
(define (sum loa)
  (cond
    [(empty? loa) 0]
    [(cons? loa)
     (+ (first loa) (sum (rest loa)))]
    ))


(require test-engine/racket-tests)
(test)

