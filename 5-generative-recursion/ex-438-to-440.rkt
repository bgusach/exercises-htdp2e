#lang htdp/isl+

(require test-engine/racket-tests)
(require racket/base)


; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m
(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)
(define (gcd-structural n m)
  (local
    ((define (greatest-divisor-<= i)
       (cond
         ; [(= i 1) 1]  ; this check is unnecessary
         [(= (remainder n i) (remainder m i) 0) i]
         [else (greatest-divisor-<= (sub1 i))]
         )))

    ; -- IN --
    (greatest-divisor-<= (min n m))
    ))
  

; ==================== Exercise 438 ====================

; Q: In your words: how does greatest-divisor-<= work?
; A: First, it takes the small one of the numbers (the 
;    greatest common divisor of two numbers cannot be 
;    greater than one of them). Then starting from the
;    small number, it checks all the numbers all the
;    way down to 1... and if anything satisfies the gcd
;    condition, it is returned. In the worst case 1 is
;    a gcd, so the algorithm always terminates.

; =================== End of exercise ==================




; ==================== Exercise 439 ====================

(time (gcd-structural 101135853 45014640))

; =================== End of exercise ==================


; Insight (euclidian algorithm): for two natural numbers, 
; L for large and S for small, the greatest common divisor 
; is equal to the greatest common divisor of S and the 
; remainder of L divided by S.
; I.e.:
;     (gcd L S) == (gcd S (remainder L S))

(check-expect (gcd-generative 6 25) 1)
(check-expect (gcd-generative 18 24) 6)
(define (gcd-generative n m)
  (local
    ((define (clever-gcd L S)
       (cond
         [(zero? S) L]
         [else (clever-gcd S (remainder L S))]
         )))
    ; -- IN --
    (clever-gcd (max n m) (min n m))
    ))


; ==================== Exercise 440 ====================

(time (gcd-generative 101135853 45014640))

; =================== End of exercise ==================


(test)

