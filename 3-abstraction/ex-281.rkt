#lang htdp/isl+

; Write down a lambda expression that:

; consumes a number and decides whether it is less than 10;
(lambda (x) (< 10))


; multiplies two given numbers and turns the result into a string;
(lambda (x y) (number->string (* x y)))


; consumes two inventory records and compares them by price;
(define-struct IR [name price])
(lambda (r1 r2) (<= (IR-price r1) (IR-price r2)))


; consumes a natural number and produces 0 if it is even and 1 if odd;
(lambda (n) (if (even? n) 0 1))


; adds a red dot at a given Posn to a given Image.
(lambda (posn img) 
  (place-image DOT (posn-x posn) (posn-y posn) img)
  )
