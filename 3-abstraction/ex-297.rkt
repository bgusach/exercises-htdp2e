#lang htdp/isl+

(require test-engine/racket-tests)

; ### Functions

(define (mk-circle centre-x centre-y r)
  ; Posn -> Boolean
  (λ (p) 
    (<=
      (distance-between centre-x centre-y p)
      )))

; Number Number Posn -> Number
; Determines the distance between (x, y) and p
(check-expect (distance-between 0 0 (make-posn 0 0)) 0)
(check-expect (distance-between 1 0 (make-posn 0 0)) 1)
(check-within (distance-between 0 0 (make-posn 1 1)) (sqrt 2) 0.001)
(define (distance-between x y p)
  (sqrt
    (+
      (sqr (- (posn-x p) x))
      (sqr (- (posn-y p) y))
      )))


(test)

