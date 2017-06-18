#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/image)


; ### Data Definitions

; ### Functions

; [A] [List-of A] [List-of A] -> [List-of A]
(check-expect (append-from-fold '(1) '()) '(1))
(check-expect (append-from-fold '(1 2) '(3)) '(1 2 3))
(define (append-from-fold a b)
  (foldr cons b a)
  )


; Q: What happens if you replace foldr with foldl?
; A: The result list contains the elements of the first list
;    in reversed order, and then the elements of the second list.
;    See function test-append-from-foldl

; [A] [List-of A] [List-of A] -> [List-of A]
(check-expect (test-append-from-foldl '(1 2) '(3)) '(2 1 3))
(define (test-append-from-foldl a b)
  (foldl cons b a)
  )


; [List-of Number] -> Number
; Returns the sum of all numbers in the list
(check-expect (sum '(1 2 3 4)) 10)
(define (sum lon)
  (foldr + 0 lon)
  ; NOTE: foldl works too. Sum is commutative
  )


; [NEList-of Number] -> Number
; Returns the product of all numbers in the list
(check-expect (product '(1 2 3 4)) 24)
(define (product lon)
  (foldl * 1 lon)
  ; NOTE: foldr works as well. Product is commutative
  )


(define sq1 (square 100 "solid" "blue"))
(define sq2 (square 100 "solid" "yellow"))

; [List-of Image] -> Image
; Composes horizontally from left to right the list of images
(check-expect (compose-h '()) empty-image)
(check-expect (compose-h (list sq1 sq2)) (beside sq1 sq2))
(define (compose-h loi)
  (foldl 
    (λ (img acc) (beside acc img)) 
    empty-image 
    loi
    ))


; Q: Can you use the other fold function?
; A: Yes, but the images are merged in the reversed order


; [List-of Image] -> Image
; Stacks a list of image. The first one of the list is on the
; bottom of the stack.
(check-expect (compose-v '()) empty-image)
(check-expect (compose-v (list sq1 sq2)) (above sq2 sq1))
(define (compose-v loi)
  (foldl above empty-image loi)
  )


(test)

