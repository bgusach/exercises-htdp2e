#lang htdp/bsl

; Note: this data definition is not exactly as suggested in the book
; A RD is one of:
; - (make-rd String #false)
; - (make-rd String RD)
(define-struct rd [colour subdoll])

; RD -> String
; Extracts all the colours used by a RD and its subdolls
(check-expect (colour (make-rd "red" #false)) "red")
(check-expect (colour (make-rd "red" (make-rd "blue" #false))) "red, blue")
(check-expect 
  (colour (make-rd "white" (make-rd "red" (make-rd "blue" #false))))
  "white, red, blue"
  )

(define (colour rd)
  (cond
    [(false? (rd-subdoll rd)) (rd-colour rd)]
    [else
      (string-append 
        (rd-colour rd)
        ", "
        (colour (rd-subdoll rd)))
      ]))

(require test-engine/racket-tests)
(test)

