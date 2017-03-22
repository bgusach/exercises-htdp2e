#lang htdp/bsl

; ### Constants


; ### Data Definitions
; List-of-posn is one of:
; - '()
; - (cons (make-posn x y) List-of-posn)


; ### Functions
; List-of-posn -> Number
; Computes the sum of all x coordinates of the passed posns
(check-expect (sum '()) 0)
(check-expect 
  (sum 
    (cons (make-posn 2 3)
      (cons (make-posn 10 0)
        (cons (make-posn 3 0)
          (cons (make-posn 9 1) '())
          ))))
  24)
(define (sum lop)
  (cond
    [(empty? lop) 0]
    [else 
      (+
        (posn-x (first lop))
        (sum (rest lop))
        )]))


(require test-engine/racket-tests)
(test)

