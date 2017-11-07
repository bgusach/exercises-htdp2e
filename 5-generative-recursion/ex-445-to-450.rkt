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
; sides of the x-axis: (f 1) and (f 6) are both positive.

; =================== End of exercise ==================




; ==================== Exercise 448 ====================

; Q: The find-root algorithm terminates for all (continuous)
;    f, left, and right for which the assumption holds. Why? 
;    Formulate a termination argument
; A: Because the algorithms terminates when the distance between
;    left and right gets small enough, and on each iteration that
;    distance is divided by two, and rather sooner than later, 
;    the algorithm will terminate. This function represents how
;    the size of the interval decreases:
;
;      (define (interval-size initial iteration)
;        (* initial (exp 1/2 iteration))
;        )
;
;    And this function calculates how many steps are necessary 
;    to hit the termination:
;
;      (define (termination-step initial-size)
;        ()
;        )

; Termination: Given the preconditions are satisfied, the Intermediate
; Value Theorem guarantees there is a root, and whatever EPSILON may be,
; at some point we will hit the termination.
; 

; =================== End of exercise ==================




; ==================== Exercise 449 ====================

; NOTE: the local suggestion was already implemented
; in previous exercises


; [Number -> Number] Number Number -> Number
(check-within (find-root.v2 poly 3 6) 4 EPSILON)
(check-within (find-root.v2 poly 1 3) 2 EPSILON)
(define (find-root.v2 f left right)
  (local
    ((define (find-root left f@l right f@r)
      (if
        (<= (- right left) EPSILON)
        left
        (local
          ((define middle (/ (+ left right) 2))
           (define f@m (f middle))
           (define zero-on-left-half
             (or (<= f@l 0 f@m) (<= f@m 0 f@l))
             ))

          ; -- IN --
          (if
            zero-on-left-half
            (find-root left f@l middle f@m)
            (find-root middle f@m right f@r)
            )))))

    ; -- IN --
    (find-root left (f left) right (f right))
    ))


; Q: How many recomputations of (f left) does this 
; design maximally avoid?
;
; A: In the edge case of `left` being a zero, the value is
; calculated only once, instead of once per recursion (how
; many times the function recurs is a logarithmic function
; of EPSILON)

; =================== End of exercise ==================




; ==================== Exercise 450 ====================

; [Number -> Number] Number Number -> Number
; Asume:
; - (f left) and (f right) on different sides of x-axis
; - f is continous in [left, right]
; - f is monotonically increasing in [left, right]
(check-within (find-root.v2 poly 3 6) 4 EPSILON)
(define (find-root.v3 f left right)
  (if
    (<= (- right left) EPSILON)
    left
    (local
      ((define middle (/ (+ left right) 2))
       (define f@m (f middle))
       )

      ; -- IN --
      (if
        (positive? f@m)
        (find-root.v3 left middle)
        (find-root.v3 right middle)
        ))))

; =================== End of exercise ==================

(test)

