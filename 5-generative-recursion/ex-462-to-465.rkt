#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations
 
; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand-side variable coefficients 
; and b is the right-hand side
 
; A Solution is a [List-of Number]
 
(define M ; an SOE 
  '((2 2  3 10) ; an Equation 
    (2 5 12 31)
    (4 1 -2  1)
    ))
 
(define S '(1 1 2)) ; a Solution


; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e)))
  )
 

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e))
  )

; ==================== Exercise 462 ====================


; [List-of Number] Solution -> Number
; Returns the result of applying a solution to the LHS
; of an Equation
(check-expect (plug-in '(1 4 -1) '(3 0 10)) -7)
(define (plug-in lhs sol)
  (for/sum [(i lhs) (j sol)]
    (* i j)
    ))


; SOE Solution -> Boolean
; Given a SOE and a solution, checks whether
; the solution satisfies all the equations of the SOE
(check-expect (check-solution M S) #t)
(check-expect (check-solution M '(1 2 3)) #f)
(define (check-solution soe sol)
  (local
    ((define (equation-satisfied? eq)
       (=
         (plug-in (lhs eq) sol)
         (rhs eq)
         )))

    ; -- IN --
    (andmap equation-satisfied? soe)
    ))

; =================== End of exercise ==================




; ==================== Exercise 463 ====================

(define SOE-2
  '((2 2 3 10)
    (0 3 9 21)
    (0 0 1 2)
    ))

(check-expect (check-solution SOE-2 S) #t)

; =================== End of exercise ==================




; ==================== Exercise 464 ====================

(define SOE-3
  '((2 2 3 10)
    (0 3 9 21)
    (0 -3 -8 -19)
    ))

(check-expect (check-solution SOE-3 S) #t)

; =================== End of exercise ==================




; ==================== Exercise 465 ====================

; Equation Equation -> Equation
; Subtracts a multiple of eq-1 from eq-2, so that
; the first coefficient of the result is zero.
; Then, the equation is dropped after dropping the leading
; zero.
(check-expect (subtract (first M) (second M)) '(3 9 21))
(check-expect (subtract '(2 4 8 2) '(6 9 21 3)) '(-3 -3 -3))
(define (subtract eq-1 eq-2)
  (local
    ((define factor (/ (first eq-2) (first eq-1))))

    ; -- IN --
    (for/list [(a (rest eq-1)) (b (rest eq-2))]
      (- b (* a factor))          
      )))

; =================== End of exercise ==================

(test)
