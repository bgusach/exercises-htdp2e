#lang htdp/isl

(require test-engine/racket-tests)

; ### Functions

; [X] [X -> Number] [NEList-of X] -> X
; Finds the item in lox that maximizes fn.
; If (argmax f (list a b c d ...)) == y,
; then (>= (f y) (f a)), (>= (f y) (f b)), ...
;
; (define (argmax fn lox) ...)
(check-expect
  (argmax string-length '("hey" "there" "I" "like" "potatoes"))
  "potatoes"
  )


; [X] [X -> Number] [NEList-of X] -> X
; Finds the item in lox that minimizes fn
; If (argmin f (list a b c d ...)) == y,
; then (<= (f y) (f a)), (<= (f y) (f b)), ...
;
; (define (argmin fn lox) ...)
(check-expect
  (argmin string-length '("hey" "there" "I" "like" "potatoes"))
  "I"
  )

(test)
