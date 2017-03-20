#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A N is one of:
; - 0
; - (add1 N)

; A Balloon is a Posn

; A List-of-balloons is one of:
; - '()
; - (cons Balloon List-of-balloons)


(define my-square (square 10 "outline" "black"))
(define balloon (circle 3 "solid" "red"))


; N Image -> Image
; Generates a new image that consists of n times img placed vertically
(check-expect (image-width (col 0 my-square)) 0)
(check-expect (col 1 my-square) my-square)
(check-expect (col 2 my-square) (above my-square my-square))
(define (col n img)
  (cond
    [(zero? n) (square 0 "solid" "black")]  ; "null" image
    [else (above img (col (sub1 n) img))]
    ))


; N Image -> Image
; Generates a new image that consists of n times img placed vertically
(check-expect (image-width (row 0 my-square)) 0)
(check-expect (row 1 my-square) my-square)
(check-expect (row 2 my-square) (beside my-square my-square))
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

(define hall (row 10 (col 18 my-square))) 

; List-of-balloons Image -> Image
; Adds a list of balloons to the passed image
(define (add-balloons lob img)
  (cond
    [(empty? lob) img]
    [else 
      (place-image
        balloon
        (posn-x (first lob))
        (posn-y (first lob))
        (add-balloons (rest lob) img)
        )]))


(main 
  (add-balloons 
    (cons 
      (make-posn 10 10)
      (cons 
        (make-posn 30 70)
        '()))
    hall
    ))

(require test-engine/racket-tests)
(test)

