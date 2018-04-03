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




; ==================== Exercise 466 ====================

; A TM is an [NEList-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix


; SOE -> TM
; triangulates the given system of equations 
(check-expect 
  (triangulate M)
  '((2 2 3 10)
    (  3 9 21)
    (    1  2)
    ))
(check-error
  (triangulate
    '((2  3  3 8)
      (2  3 -2 3)
      (4 -2  2 4)
      )))
(define (triangulate M)
  (match M
    [(cons eq '()) M]
    [(cons head tail)
      (cons
        head
        (triangulate
          (for/list [(eq tail)]
            (subtract head eq)
            )))]))

; =================== End of exercise ==================




; ==================== Exercise 467 ====================

(define SOE-4
  '((2  3  3 8)
   (2  3 -2 3)
   (4 -2  2 4)
   ))

; SOE -> TM
; triangulates the given system of equations 
; Termination: as long as the SOE has a solution, the function returns. 
(check-expect 
  (triangulate.v2 M)
  '((2 2 3 10)
    (  3 9 21)
    (    1  2)
    ))
(check-expect
  (triangulate.v2 SOE-4)
  '((2  3  3   8)
    (  -8 -4 -12)
    (     -5  -5))
  )
(define (triangulate.v2 M)
  (match (rotate-till-non-zero M)
    [(cons eq '()) M]
    [(cons head tail)
      (cons
        head
        (triangulate.v2
          (for/list [(eq tail)]
            (subtract head eq)
            )))]))

; Check that original SOE and the triangulated one
; are equivalent
(check-expect 
  (check-solution 
    SOE-4
    '(1 1 1)
    )
  #t
  )


(check-expect 
  (check-solution 
    (triangulate.v2 SOE-4)
    '(1 1 1)
    )
  #t
  )

; SOE -> SOE
; Rotates the matrix until one equation starts with non-zero value
; Termination: function returns if at least one equation starts
; with non-zero value. Otherwise ¯\_(ツ)_/¯
(check-expect
  (rotate-till-non-zero 
    '((0 3 4)
      (5 4 2)
      ))
  '((5 4 2)
    (0 3 4)
    ))
(define (rotate-till-non-zero M)
  (if
    (zero? (first (first M)))
    (rotate-till-non-zero 
      (append (rest M) (cons (first M) '()))
      )
    M
    ))

; =================== End of exercise ==================




; ==================== Exercise 468 ====================

; SOE -> TM
; triangulates the given system of equations 
; Termination: as long as the SOE has a solution, the function returns. 
(check-expect 
  (triangulate.v3 M)
  '((2 2 3 10)
    (  3 9 21)
    (    1  2)
    ))
(check-expect
  (triangulate.v3 SOE-4)
  '((2  3  3   8)
    (  -8 -4 -12)
    (     -5  -5))
  )
(check-error
  (triangulate.v3 '((0 0 0) (0 0)))
  )
(define (triangulate.v3 M)
  (match (rotate-till-non-zero.v2 M)
    [(cons eq '()) M]
    [(cons head tail)
      (cons
        head
        (triangulate.v3
          (for/list [(eq tail)]
            (subtract head eq)
            )))]))


; SOE -> SOE
; Rotates the matrix until one equation starts with non-zero value
; Termination: function returns always. If no equation starts with
; non-zero value, an error is signaled.
(check-expect
  (rotate-till-non-zero.v2
    '((0 3 4)
      (5 4 2)
      ))
  '((5 4 2)
    (0 3 4)
    ))
(define (rotate-till-non-zero.v2 M)
  (local
    (; SOE SOE -> SOE
     (define (rotate acc matrix-rest)
       (cond
         [(empty? matrix-rest)
           (error "No equation start with non-zero")
           ]
         [(zero? (first (first matrix-rest)))
           (rotate 
             (cons (first matrix-rest) acc) 
             (rest matrix-rest)
             )]
         [else 
           (cons
             (first matrix-rest) 
             (append (rest matrix-rest) acc)
             )])))

    ; -- IN -- 
    (rotate '() M)
    ))

; =================== End of exercise ==================




; ==================== Exercise 469 ====================

(define TM
  '((2 2 3 10)
    (  3 9 21)
    (    1  2)
    ))

(define TM-SOL
  '(1 1 2)
  )

; TM -> Solution
; Given a triangular matrix, returns a solution
(check-expect (solve TM) TM-SOL)
(define (solve M)
  (local
    ((define (reduce eq sol)
       (local
         ((define eq-lhs (lhs eq))
          (define first-coeff (first eq-lhs))
          (define eq-rhs (rhs eq))
          (define partial (plug-in (rest eq-lhs) sol))
          )
         
         ; -- IN --
         (cons
           (/ (- eq-rhs partial) first-coeff)
           sol
           ))))

    ; -- IN --
    (foldr reduce '() M)
    ))


; =================== End of exercise ==================




; ==================== Exercise 470 ====================

; SOE -> Solution
; Resolves a SOE
(check-expect (gauss M) S)
(define (gauss M)
  (solve (triangulate M))
  )

; =================== End of exercise ==================

(test)

