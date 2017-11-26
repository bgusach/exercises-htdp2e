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


; [Number -> Number] Number -> Number
; Finds the root of the tangent of `f` at `r1`
(check-error (root-of-tangent func0 5))
(check-within (root-of-tangent func1 5) 0 EPS)
(check-within (root-of-tangent func2 2) 1 EPS)
(define (root-of-tangent f r1)
  (-
    r1
    (/
      (f r1)
      (slope f r1)
      )))

; =================== End of exercise ==================

(test)

