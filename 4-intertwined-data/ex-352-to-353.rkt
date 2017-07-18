#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ### Data definitions
(define-struct add [left right])
(define-struct mul [left right])


; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)

; ==================== Exercise 352 ====================

; BSL-var-expr Symbol Number -> BSL-var-expr
; Replaces all occurrences of `sym` with `val` within `ex`
(check-expect (subst 4 'x 0) 4)
(check-expect (subst 'x 'x 0) 0)
(check-expect (subst (make-add 'x 5) 'x 2) (make-add 2 5))
(check-expect 
  (subst (make-mul 'x (make-add 'x 5)) 'x 2)
  (make-mul 2 (make-add 2 5))
  )
(define (subst ex sym val)
  (match
    ex
    [(? number?) ex]
    [(? symbol?) (if (symbol=? ex sym) val ex)]
    [(add l r) (make-add (subst l sym val) (subst r sym val))]
    [(mul l r) (make-mul (subst l sym val) (subst r sym val))]
    ))

; =================== End of exercise ==================




; ==================== Exercise 353 ====================

; BSL-var-expr -> Boolean
; Returns whether `ex` is strictly a numeric expression
; or in other words, whether ex is also a BSL-expr
(check-expect (numeric? 'x) #false)
(check-expect (numeric? 4) #true)
(check-expect (numeric? (make-add 4 5)) #true)
(check-expect (numeric? (make-add 4 'x)) #false)
(check-expect (numeric? (make-mul 4 8)) #true)
(check-expect (numeric? (make-mul 4 (make-add 3 'y))) #false)
(define (numeric? ex)
  (match
    ex
    [(? number?) #true]
    [(add l r) (andmap numeric? (list l r))]
    [(mul l r) (andmap numeric? (list l r))]
    [else #false]
    ))

; =================== End of exercise ==================

(test)

