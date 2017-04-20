#lang htdp/bsl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; CONSTANTS
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))



; DATA DEFINITIONS

; Polygon is either:
; - (list Posn Posn Posn)
; - (cons Posn Polygon)


(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)
    ))


(define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)
    ))


; FUNCTIONS

; Polygon -> Image
(define (render-world ws)
  (render-poly BACKGROUND ws)
  )


(define 
  rendered-triangle-p
  (scene+line
    (scene+line
      (scene+line BACKGROUND 20 10 20 20 "red")
      20 20 30 20 "red")
    30 20 20 10 "red")
  )

; Image Polygon -> Image
(check-expect (render-poly BACKGROUND triangle-p) rendered-triangle-p)
(define (render-poly img poly)
  (connect-dots 
    img 
    poly
    (first poly)
    ))


; Image NELP Posn -> Image
; Connects a list of points on top of an image, connecting the last point of nelp
; with the third argument `extra-point`
(define (connect-dots img nelp extra-point)
  (cond
    [(empty? (rest nelp))
     (render-line 
       img
       (first nelp)
       extra-point
       )]

    [else 
      (render-line
        (connect-dots img (rest nelp) extra-point)
        (first nelp)
        (second nelp)
       )]))


; Image Posn Posn -> Image 
; renders a line from p to q into img
(define (render-line img p q)
  (scene+line
    img
    (posn-x p) (posn-y p) (posn-x q) (posn-y q)
    "red"
    ))


; Non-empty-list-of-anything -> Anything
; Returns the last element of a non empty list
(check-expect (last (list 1 2 3)) 3)
(check-expect (last (list 1)) 1)
(define (last loa)
  (cond
    [(empty? (rest loa)) (first loa)]
    [else (last (rest loa))]
    ))


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    ))


; TEST & MAIN CALL
(test)
(main 
  (list 
    (make-posn 50 50)
    (make-posn 50 100)
    (make-posn 100 100)
    (make-posn 100 50)
    ))
