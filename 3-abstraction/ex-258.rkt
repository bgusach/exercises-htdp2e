#lang htdp/isl

(require 2htdp/image)
(require 2htdp/universe)

; CONSTANTS
(define BACKGROUND-WIDTH 300)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))


; DATA DEFINITIONS

; Polygon is either:
; - (list Posn Posn Posn)
; - (cons Posn Polygon)


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


; Image Polygon -> Image
(define (render-poly img poly)
  (local
    (
      ; Image NELP -> Image
      ; Connects a list of points on top of an image
      (define (connect-dots img nelp)
        (cond
          [(empty? (rest nelp)) img]
          [else 
            (render-line
              (connect-dots img (rest nelp))
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
      (define (last loa)
        (cond
          [(empty? (rest loa)) (first loa)]
          [else (last (rest loa))]
          ))

     )
     (connect-dots img (cons (last poly) poly))
    ))


(define (main ws)
  (big-bang 
    ws
    [to-draw render-world]
    ))


(main square-p)

