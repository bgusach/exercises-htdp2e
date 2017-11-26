#lang htdp/isl+

(require test-engine/racket-tests)



; ==================== Exercise 455 ====================

; ### Constants

(define EPS 0.0001)

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




; ==================== Exercise 455 ====================

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


(define (poly x)
  (* (- x 2) (- x 4))
  )

; [Number -> Number] Number -> Number
; Returns a zero for the function `f`. `r1` is the initial guess
; Generation: repeatedly generates improved guesses
(check-within (newton sin 1) 0 EPS)
(check-within (newton poly 1) 2 EPS)
(check-within (newton poly 5) 4 EPS)
(check-error (newton poly 3))  ; slope == 0, error
; (check-within (newton poly 3.00001) 4 EPS)  ; computationally expensive
; (check-within (newton poly #i3.0) 4 EPS)  ; endless recursion
(define (newton f r1)
  (cond
    [(<= (abs (f r1)) EPS) r1]
    [else (newton f (root-of-tangent f r1))]
    ))


; ==================== Exercise 457 ====================


; Number Number -> N
; Calculates how many months it takes to double any
; amount of money at a given monthly interest rate.
; Rounds up to get a natural amount of months
(check-within (double-amount 2) 1 0.01)
(check-within (double-amount 1.01) 70 0.01)
(define (double-amount rate)
  (ceiling
    (/
      (log 2)
      (log rate)
      )))


; =================== End of exercise ==================

(test)

