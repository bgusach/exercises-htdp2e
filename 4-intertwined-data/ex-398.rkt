#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ==================== Exercise 398 ====================

; [List-of Number] [List-of Number] -> Number
; Calculates the value of a linear combination for a given
; input
(check-expect (value '() '()) 0)
(check-expect (value '(0) '(5)) 0)
(check-expect (value '(1) '(5)) 5)
(check-expect (value '(2 3) '(5 10)) 40)
(define (value lc vals)
  (cond
    [(and (empty? lc) (empty? vals)) 0]
    [(and (cons? lc) (cons? vals))
     (+
       (* (first lc) (first vals))
       (value (rest lc) (rest vals))
       )]
    [else
      (error "lengths dont match!")
      ]))

; =================== End of exercise ==================


(test)

