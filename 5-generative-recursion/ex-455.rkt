#lang htdp/isl+

(require test-engine/racket-tests)



; ==================== Exercise 452 ====================

; ### Constants

(define EPS 0.001)

; ### Functions

(define (func0 x)
  3
  )

(define (func1 x)
  (* 3 x)
  )

(define (func2 x)
  (* 3 x x)
  )

; [Number -> Number] Number -> Number
(check-within (slope func0 5) 0 EPS)
(check-within (slope func1 5) 3 EPS)
(check-within (slope func2 2) 12 EPS)
(define (slope f r1)
  (/ 
    (-
      (f (+ r1 EPS))
      (f (- r1 EPS))
      )
    (* 2 EPS)
    ))


; =================== End of exercise ==================

(test)

