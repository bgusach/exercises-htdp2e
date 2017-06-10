#lang htdp/isl

(require 2htdp/image)
(require test-engine/racket-tests)

; ### Constants
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))

; ### Data Definitions

; ### Functions

; [List-of Number] -> Number
(check-expect (product '(1 2 3 4)) (* 1 2 3 4))
(define (product l)
  (fold2 l * 1)
  )

	
; [List-of Posn] -> Image
; Places a dot for each posn on top of an empty image
(check-expect (image* '()) emt)
(check-expect 
  (image* (list (make-posn 0 0) (make-posn 2 3)))
  (place-dot (make-posn 2 3) (place-dot (make-posn 0 0) emt))
  )
(define (image* l)
  (fold2 l place-dot emt)
  )


; [List-of A] [A B -> B] -> B
(define (fold2 items fold-fn acc)
  (cond
    [(empty? items) acc]
    [else
      (fold2
        (rest items)
        fold-fn
        (fold-fn (first items) acc)
        )]))
 

; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img
     ))
 
(test)

