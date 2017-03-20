#lang htdp/bsl

(require 2htdp/image)

; MaybeImage is one of:
; - Image
; - #false

; Makes a square for testing purposes
(define (my-square x)
  (square x "solid" "green")
  )

; Checks whether there is an image with the right size
 
(check-expect (ill-sized? '() 10) #false)
(check-expect (ill-sized? (cons (my-square 10) '()) 10)  #true)
(check-expect (ill-sized? (cons (my-square 15) (cons (my-square 10) '())) 10)  #true)
(check-expect (ill-sized? (cons (my-square 10) '()) 5) #false)
 
(define (ill-sized? loi n)
  (cond
    [(empty? loi) #false]
    [else 
      (or 
        (and 
          (= (image-width (first loi)) n) 
          (= (image-height (first loi)) n)
          )
        (ill-sized? (rest loi) n)
        )]))

(require test-engine/racket-tests)
(test)
