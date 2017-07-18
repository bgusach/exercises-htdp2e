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

(test)

