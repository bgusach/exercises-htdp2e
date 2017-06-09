#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

; A [Maybe X] is one of:
; - #false
; - X

; A [Maybe String] is one of:
; - #false
; - String
; Interpretation: either a #false or a String

; A [Maybe [List-of String]] is one of:
; - #false
; - [List-of String]
; Interpretation: either a list of strings or #false

; A [List-of [Maybe String]] is one of:
; - '()
; - (cons [Maybe String] [List-of [Maybe String]])
; Interpretation: a list containing any number of strings or #false's


; ### Functions

; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s
; or #false otherwise
(check-expect (occurs "a" (list "b" "a" "d" "e")) (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #false)
(define (occurs s los)
  (cond
    [(empty? los) #false]
    [(string=? (first los) s) (rest los)]
    [else (occurs s (rest los))]
    ))

(test)

