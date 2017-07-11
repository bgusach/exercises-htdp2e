#lang htdp/isl+

; NOTE: Nothing implemented here. Just open in DrRacket

; ### Functions
; Bound occurrences:
; - p1 (top-level scope): twice in p3
; - x (local scope): twice
; - y (local scope): twice
(define (p1 x y)
  (+ 
    (* x y)
    (+ 
      (* 2 x)
      (+ (* 2 y) 22)
      )))
 

; Bound occurrences:
; - p2 (top-level scope): once in p3
; - x (local scope): twice
(define (p2 x)
  (+ (* 55 x) (+ x 11)))
 

; Bound occurrences:
; - p3 (top-level scope): none
; - x (local scope): three times
(define (p3 x)
  (+ 
    (p1 x 0)
    (+ (p1 x 1) (p2 x))
    ))
