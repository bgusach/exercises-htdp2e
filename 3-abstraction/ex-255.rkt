#lang htdp/isl

; ### Functions

; [List-of Number] [Number -> Number] -> [List-of Number]
(define (map-n lon fn)
  lon
  )

; [List-of String] [String -> String] -> [List-of String]
(define (map-s los fn)
  los
  )


; [A] [List-of A] [A -> A] -> [List-of A]

; Alternatively, there is a more abstract and useful signature
; that does not force the type of elements to be the same:
; [A B] [List-of A] [A -> B] -> [List-of B]
(define (map-abs l fn)
  l
  )
