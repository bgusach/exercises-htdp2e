#lang htdp/bsl

; ### Constants
(define MIN-X 0)
(define MAX-X 100)
(define MIN-Y 0)
(define MAX-Y 200)


; ### Functions

; Posn -> Boolean
; Checks whether the passed Posn is within values
(check-expect (legal-posn? (make-posn MIN-X MIN-Y)) #true)
(check-expect (legal-posn? (make-posn MIN-X (- MIN-Y 1))) #false)
(define (legal-posn? p) 
  (and
    (>= (posn-x p) MIN-X)
    (<= (posn-x p) MAX-X)
    (>= (posn-y p) MIN-Y)
    (<= (posn-y p) MAX-Y)
    ))

; List-of-posn -> List-of-posn
; Filters out all Posns of which x is not in [0, 100] and y in [0, 200]
(check-expect (legal '()) '())
(check-expect 
  (legal 
    (cons (make-posn 0 0)
      (cons (make-posn 50 1000) '()) 
      ))
  (cons (make-posn 0 0) '())
  )
(define (legal lop)
  (cond
    [(empty? lop) '()]
    [else 
      (if 
        (legal-posn? (first lop))
        (cons (first lop) (legal (rest lop)))
        (legal (rest lop))
        )]))

(require test-engine/racket-tests)
(test)


