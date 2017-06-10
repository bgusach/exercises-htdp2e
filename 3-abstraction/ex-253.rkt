#lang htdp/isl

(require test-engine/racket-tests)

; ### Functions

; [Number -> Boolean]
; examples:
even?
odd?
negative?
(define (is-exactly-42 n)
  (= n 42)
  )

; [Boolean String -> Boolean]
(define (useless-function b str)
  #false
  )


; [Number Number Number -> Number]
(define (get-smallest-from-3 a b c)
  (min (min a b) c)
  )


; [Number -> [List-of Number]]
(define (forty-twoer n)
  (make-list n 42)
  )


; [[List-of Number] -> Boolean]
(define (contains-42 lon)
  (member 42 lon)
  )

 
(test)

