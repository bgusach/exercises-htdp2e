#lang htdp/bsl

; ### Constants
(define Y-DELTA 1)

; ### Data Definitions

; ### Functions
; List-of-posns -> List-of-posns
; Moves each Posn of the list in the Y coordinate by Y-DELTA
(check-expect (translate '()) '())
(check-expect 
  (translate (cons (make-posn 0 0) '()))
  (cons (make-posn 0 Y-DELTA) '())
  )
(define (translate lop)
  (cond
    [(empty? lop) '()]
    [else 
      (cons
        (make-posn 
          (posn-x (first lop))
          (+ 
            (posn-y (first lop))
            Y-DELTA
            ))
        (translate (rest lop))
        )]))

(require test-engine/racket-tests)
(test)

