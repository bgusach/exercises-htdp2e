#lang htdp/bsl

; ### Constants
(define RATIO-F2C 5/9)
(define OFFSET-F2C -32)

; ### Data Definitions
; Celsius is a Number > -273
; Fahrenheit is a Number > -460


; ### Functions
; Fahrenheit -> Celsius
; converts from Fahrenheit to Celsius degrees
(check-within (f2c 212) 100 0.01)

(define (f2c f)
  (*
    (+ f OFFSET-F2C)
    RATIO-F2C
    ))

(require test-engine/racket-tests)
(test)

