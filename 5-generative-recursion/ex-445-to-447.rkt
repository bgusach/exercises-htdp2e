#lang htdp/isl+

(require racket/base)
(require test-engine/racket-tests)


; ### Constants
(define EPSILON 0.001)

; ### Data Definitions

; ### Functions

; [Number -> Number] Number Number -> Number
; Finds R such that f has a root in [R, (+ R EPSILON)]
; Preconditions:
; - The function f must be continuous in [left, right]
; - (f left) and (f right) must be on opposite sides of the x-axis
; 
; Generation: This function searchs the root by cutting in halves the
; interval, and checking where the root must be.
; 
; Termination: Given the preconditions are satisfied, the Intermediate
; Value Theorem guarantees there is a root, and whatever EPSILON may be,
; at some point we will hit the termination.
; 
(define (find-root f left right)
  (if
    (<= (- right left) EPSILON)
    left
    (local
      ((define middle (/ (+ left right) 2))
       (define mid-value (f middle))
       (define left-value (f left))
       (define right-value (f right))
       (define zero-on-left-half
         (or 
           (<= left-value 0 mid-value)
           (<= mid-value 0 left-value)
           )))

      ; -- IN --
      (if
        zero-on-left-half
        (find-root f left middle)
        (find-root f middle right)
        ))))


; ==================== Exercise 445 ====================

; Number -> Number
; Just a simple polynomial expression
(define (poly x)
  (* (- x 2) (- x 4))
  )

; =================== End of exercise ==================




; ==================== Exercise 446 ====================

(define res0 (find-root poly 3 6))

(check-within res0 4 EPSILON)

(display 
  (format 
    "The root of poly in [3 6] is ~a\n" 
    (real->decimal-string res0)
    ))

; NOTE: let's try to find the first root as well
(define res1 (find-root poly 1 3))
(check-within res1 2 EPSILON)

(display 
  (format 
    "The root of poly in [1 3] is ~a\n" 
    (real->decimal-string res1)
    ))

; =================== End of exercise ==================




; ==================== Exercise 447 ====================

(display 
  (format 
    "The root of poly found in [1 6] is ~a\n" 
    (real->decimal-string (find-root poly 1 6))
    ))

; NOTE: this seem to work, but actually it does not satisfy
; the requirement of (f left) and (f right) being on opposite
; sides of the x-axis.

; =================== End of exercise ==================

(test)

