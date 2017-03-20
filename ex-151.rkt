#lang htdp/bsl

; A N is one of:
; - 0
; - (add1 N)


; N Number -> Number
; Computes n * x

(check-within (multiply 0 1) 0 0.0001)
(check-within (multiply 1 2) 2 0.0001)
(check-within (multiply 2 4.5) 9 0.0001)
(check-within (multiply 3 20) 60 0.0001)

(define (multiply n x)
  (cond
    [(zero? n) 0]
    [else 
      (+ x (multiply (sub1 n) x))]
    ))

(require test-engine/racket-tests)
(test)
