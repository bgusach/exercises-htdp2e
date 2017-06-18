#lang htdp/isl+

(require test-engine/racket-tests)


; Lambda syntax:
; (lambda (variable variable ...) expression)

; Q: are the following phrases legal lambda expressions?

; - (lambda (x y) (x y y))
; A: Yes. It is a function that accepts two parameters, and calls
; the first one as a function, passing as arguments twice the second
; one
(check-expect
  ((lambda (x y) (x y y)) + 2)
  4
  )

; - (lambda () 10)
; A: No. There must be at least one variable

; - (lambda (x) x)
; A: Yes. A function that accepts one value and returns it as-is.
(check-expect
  ((lambda (x) x) "hola")
  "hola"
  )

; - (lambda (x y) x)
; A: Yes. A function that accepts two values and returns the first one.
(check-expect
  ((lambda (x y) x) "hola" "amigo")
  "hola"
  )

; - (lambda x 10)
; A: No. Parenthesis around parameters missing.

(test)

