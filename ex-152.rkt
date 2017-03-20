#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A N is one of:
; - 0
; - (add1 N)

(define test-square (square 20 "solid" "olive"))

; N Image -> Image
; Generates a new image that consists of n times img placed vertically
(check-expect (image-width (col 0 test-square)) 0)
(check-expect (col 1 test-square) test-square)
(check-expect (col 2 test-square) (above test-square test-square))
(define (col n img)
  (cond
    [(zero? n) (square 0 "solid" "black")]  ; "null" image
    [else (above img (col (sub1 n) img))]
    ))


; N Image -> Image
; Generates a new image that consists of n times img placed vertically
(check-expect (image-width (row 0 test-square)) 0)
(check-expect (row 1 test-square) test-square)
(check-expect (row 2 test-square) (beside test-square test-square))
(define (row n img)
  (cond
    [(zero? n) (square 0 "solid" "black")]  ; "null" image
    [else (beside img (row (sub1 n) img))]
    ))


(define (render img)
  img)


(define (main img)
  (big-bang
    img
    [to-draw render]
    ))

(require test-engine/racket-tests)
(test)

(main (col 10 test-square))
(main (row 4 test-square))

