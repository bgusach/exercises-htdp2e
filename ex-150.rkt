#lang htdp/bsl

; A N is one of:
; - 0
; - (add1 N)


; N  -> Number
; Computes n + pi

(check-within (add-to-pi 0) pi 0.0001)
(check-within (add-to-pi 1) (+ 1 pi) 0.0001)
(check-within (add-to-pi 2) (+ 2 pi) 0.0001)

(define (add-to-pi n)
  (cond
    [(zero? n) pi]
    [else (add1 (add-to-pi (sub1 n)))]
    ))

(require test-engine/racket-tests)
(test)
