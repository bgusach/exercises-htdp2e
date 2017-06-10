#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

; ### Functions

(define (return-zero x)
  0
  )


(define (id x)
  x
  )

; (Number -> Any) -> Boolean
(check-expect (function=at-1.2-3-and-5.775? return-zero) #true)
(check-expect (function=at-1.2-3-and-5.775? id) #false)
(define (function=at-1.2-3-and-5.775? fn)
  (and
    (equal? (fn 1.2) (fn 3))
    (equal? (fn 3) (fn 5.775))
    ))


; Question: is it possible to define function=?
; Answer: no it is not. To do so we would have to input all
; possible data in the universe into the functions, and
; check that the results are equivalent.

(test)

