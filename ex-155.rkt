#lang htdp/bsl

; Note: this data definition is not exactly as suggested in the book
; A RD is one of:
; - (make-rd String #false)
; - (make-rd String RD)
(define-struct rd [colour subdoll])

; RD -> String
; Extracts the colour of the innermost doll
(check-expect (inner (make-rd "red" #false)) "red")
(check-expect (inner (make-rd "red" (make-rd "blue" #false))) "blue")
(check-expect 
  (inner (make-rd "white" (make-rd "red" (make-rd "blue" #false))))
  "blue"
  )


(define (inner rd)
  (cond
    [(false? (rd-subdoll rd)) (rd-colour rd)]
    [else (inner (rd-subdoll rd))]
    ))


(require test-engine/racket-tests)
(test)

