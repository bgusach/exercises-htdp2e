#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

; ### Functions

; Number Number -> Boolean
; is the area of a square with side x larger than c??
(define (squared>? x c)
  (> (* x x) c))

(squared>? 3 10)
(squared>? 4 10)
(squared>? 5 10)

(test)

