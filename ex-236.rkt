#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

; ### Functions

; Lon -> Lon
; add 1 to each item on l
(check-expect (add1* '(0 1 2 3)) '(1 2 3 4))
(define (add1* l)
  (add* 1 l)
  )
	
; Lon -> Lon
; adds 5 to each item on l
(check-expect (plus5 '(0 1 2 3)) '(5 6 7 8))
(define (plus5 l)
  (add* 5 l)
  )

; Number Lon -> Lon
(define (add* n lon)
  (cond
    [(empty? lon) '()]
    [else
     (cons
       (+ n (first lon))
       (add* n (rest lon))
       )]))

; Lon -> Lon
; Subtracts 2 from each item on l
(check-expect (sub2 '(0 1 2 3)) '(-2 -1 0 1))
(define (sub2 l)
  (add* -2 l)
  )
 
(test)

