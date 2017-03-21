#lang htdp/bsl

; ### Constants
(define RATIO-F2C 5/9)
(define OFFSET-F2C 32)

; ### Data Definitions
; Celsius is a Number > -273
; Fahrenheit is a Number > -460


; ### Functions
; Fahrenheit -> Celsius
; converts from Fahrenheit to Celsius degrees
(check-within (f-to-c 212) 100 0.01)

(define (f-to-c f)
  (*
    (- f OFFSET-F2C)
    RATIO-F2C
    ))

; List-of-fahrenheit -> List-of-celsius
; converts a list of F to C degrees
(check-member-of (f-to-c* '()) '())
(check-member-of (f-to-c* (cons 100 '())) (cons (f-to-c 100) '()))
(define (f-to-c* lof)
  (cond
    [(empty? lof) '()]
    [else 
      (cons
        (f-to-c (first lof))
        (f-to-c* (rest lof))
        )]))


(require test-engine/racket-tests)
(test)

