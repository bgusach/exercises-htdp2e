#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ==================== Exercise 458 ====================

; ### Data Definitions
(define EPS 0.1)

; ### Functions
(define (const-fn x) 20)
(define (linear-fn x) (* 2 x))
(define (quadratic x) (* 3 (sqr x)))

 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
(check-within (integrate-k const-fn 12 22) 200 EPS)
(check-within (integrate-k linear-fn 0 10) 100 EPS)
; (check-within (integrate-k quadratic 0 10) 1000 EPS)
(define (integrate-k f a b) 
  (*
    (- b a)
    (+ (f a) (f b))
    1/2
    ))

; Q: Which of the three tests fails and by how much?
; A: The third one, the quadradic function. Fails by
; a 50%. The kepler integration is too approximate.

; =================== End of exercise ==================




; ==================== Exercise 459 ====================

; ### Data Definitions
(define R 160)
(define EPS-2 0.01)

; ### Functions
 
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
(check-within (integrate-r const-fn 12 22) 200 EPS-2)
(check-within (integrate-r linear-fn 0 10) 100 EPS-2)
(check-within (integrate-r quadratic 0 10) 1000 EPS-2)
(define (integrate-r f a b) 
  (local
    ((define WIDTH (/ (- b a) R))
     (define OFFSET (+ a (* WIDTH 1/2)))
     )

    ; -- IN --
    (for/sum ([i R])
      (* WIDTH 
        (f (+ OFFSET (* i WIDTH))) 
        ))))


; =================== End of exercise ==================




; ==================== Exercise 460 ====================

(define EPS-3 0.01)

; [Number -> Number] Number Number -> Number
(check-within (integrate-dc const-fn 12 22) 200 EPS-2)
(check-within (integrate-dc linear-fn 0 10) 100 EPS-2)
(check-within (integrate-dc quadratic 0 10) 1000 EPS-2)
(define (integrate-dc f a b) 
  (local
    ((define WIDTH (- b a))
     (define MID (* (+ a b) 1/2))
     )

    ; -- IN --
    (if 
      (< WIDTH EPS-3)
      (integrate-k f a b)
      (+
        (integrate-dc f a MID)
        (integrate-dc f MID b)
        ))))

; =================== End of exercise ==================




; ==================== Exercise 461 ====================

; [Number -> Number] Number Number -> Number
(check-within (integrate-adaptative const-fn 12 22) 200 EPS-2)
(check-within (integrate-adaptative linear-fn 0 10) 100 EPS-2)
(check-within (integrate-adaptative quadratic 0 10) 1000 EPS-2)
(define (integrate-adaptative f a b) 
  (local
    ((define WIDTH (- b a))
     (define MID (* (+ a b) 1/2))
     (define LEFT-TRAP (integrate-k f a MID))
     (define RIGHT-TRAP (integrate-k f MID b))
     )

    ; -- IN --
    (if
      (< (abs (- LEFT-TRAP RIGHT-TRAP)) (* WIDTH EPS-3))
      (integrate-k f a b)
      (+ (integrate-adaptative f a MID) 
         (integrate-adaptative f MID b)
         ))))

; =================== End of exercise ==================

(test)

