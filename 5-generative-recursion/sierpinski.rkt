#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)

; ### Constants
(define SMALL 4)

; ### Functions

; Number -> Image
; Returns a triangle with side `side`
(define (make-triangle side)
  (triangle side 'outline 'red)
  )


; Number -> Image
; Creates a Sierpinksi triangle of size `side`
; by generating one for half a `side` and placing
; one copy above two copies
; 
; Termination: edge/trivial case is based on a minimum side size,
; which will always be reached because on each iteration the
; size is divided by two.
(define (sierpinski side)
  (cond
    [(<= side SMALL) (make-triangle side)]
    [else
      (local
        ((define subtriangle 
           (sierpinski (/ side 2))
           ))
        ; -- IN
        (above
          subtriangle
          (beside subtriangle subtriangle)
          ))]))


(define (main ws)
  (big-bang 
    ws
    [to-draw sierpinski]
    ))

(main 1000)

