#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)


(define ROWS 18)
(define COLS 10)

; A N is one of:
; - 0
; - (add1 N)

; A Balloon is a Posn

; A List-of-balloons is one of:
; - '()
; - (cons Balloon List-of-balloons)


(define-struct riot-status [n-balloon lob])
; RiotStatus is a structure (make-riot-status N List-of-balloons)
; interpretation: (make-riot-status n l) means that there are
; n balloons to be thrown and l have been already thrown

; A WorldStatus is a RiotStatus


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


; WorldStatus -> Image
(define (render ws)
  (add-balloons (riot-status-lob ws) hall)
  )


(define hall (row COLS (col ROWS my-square))) 

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


; WorldStatus -> WorldStatus
(define (tock ws)
  (cond
    [(zero? (riot-status-n-balloon ws)) ws]
    [else 
      (make-riot-status
        (sub1 (riot-status-n-balloon ws))
        (cons 
          (make-posn 
            (* 10 (random COLS))
            (* 10 (random ROWS))
            ) 
          (riot-status-lob ws)
          ))]))


(define (main ws)
  (big-bang
    ws
    [to-draw render]
    [on-tick tock 1]
    ))


(main (make-riot-status 10 '()))

(require test-engine/racket-tests)
(test)

