#lang htdp/isl+

(require racket/base)
(require test-engine/racket-tests)


; ==================== Exercise 451 ====================

; ### Data Definitions

(define-struct table [length array])
; A Table is a structure:
;   (make-table N [N -> Number])

(define table1 (make-table 3 (λ (i) i)))
 
; N -> Number
(define (a2 i)
  (if 
    (= i 0)
    pi
    (error "table2 is not defined for i != 0")
    ))
 
(define table2 (make-table 1 a2))
(define table3 (make-table 25 (λ (x) (- x 20))))

; ### Functions

; Table N -> Number
; Looks up the ith value in array of t
(define (table-ref t i)
  ((table-array t) i)
  )


; Table -> N
; Returns the first root of the table by performing a linear search
; Condition: tab must be monotonically increasing
(check-expect (find-linear table1) 0)
(check-error (find-linear table2))
(check-expect (find-linear table3) 20)
(check-expect (find-linear table4) 1)
(define (find-linear tab)
  (local
    ((define len (table-length tab))
     (define (fl tab idx)
       (cond
         [(= idx len) (error "table exhausted, no root found")]
         [else
           (if
             ; NOTE: I assume one value of the table is exactly zero.
             (zero? (table-ref tab idx))
             idx
             (fl tab (add1 idx))
             )])))

    ; -- IN --
    (fl tab 0)
    ))

; Edge case: zero is on right boundary
(define table4 (make-table 2 (λ (x) (- x 1))))

(define table5 (make-table 2 (λ (x) (- x 2))))

; Edge: table with only one item, and item is a zero
(define table6 (make-table 1 (λ (_) 0)))

; Table -> N
; Returns the first root of the table by performing a linear search
;
; Precondition: table must be monotonically increasing
; Generation: the generation step consists in calculating the middle point of
; the interval, and the value of the table at that point. If the middle point
; is a zero, the problem is solved, otherwise we can determine on which side of
; the subinterval the zero is, since the table increases monotonically. The
; subinterval becomes the interval for the next recursion step. If the middle
; point is between two natural numbers, we arbitrarily use the smaller one.
; Termination: The size of the interval is divided by two on each step, so we
; exponentially converge to an interval [n, (+ n 1)], in which case both
; boundaries are explicitly checked for zeroes. If none found, an error will be
; raised. The edge case in which the table has only one element is also
; covered.
(check-expect (find-binary table1) 0)
(check-error (find-binary table2))
(check-expect (find-binary table3) 20)
(check-expect (find-binary table4) 1)
(check-error (find-binary table5))
(check-expect (find-binary table6) 0)
(define (find-binary tab)
  (local
    ((define (search-zero a b)
       (local
         ((define exhausted-interval (<= (abs (- a b)) 1))
          (define mid (quotient (+ a b) 2))
          (define val@mid (table-ref tab mid))
          )

         ; -- IN --
         (cond
           [exhausted-interval
             (cond
               [(zero? (table-ref tab a)) a]
               [(zero? (table-ref tab b)) b]
               [else (error "The table contains no zeros")]
               )]
           [(negative? val@mid) (search-zero mid b)]
           [(zero? val@mid) mid]
           [(positive? val@mid) (search-zero a mid)]
           ))))

    ; -- IN --
    (search-zero 0 (sub1 (table-length tab)))
    ))

; Q: Imagine a table with 1024 slots and the root at 1023. 
; How many calls to find are needed in find-linear and 
; find-binary, respectively? 
;
; A: find-linear needs exactly 1024 steps to find the zero,
; whereas find-binary needs the logarithm in base 2 of 1024,
; i.e. 10 steps. In other words, logarithmic complexity beats
; the hell of out linear complexity.

; =================== End of exercise ==================

(test)

