#lang htdp/bsl

; Son (set of Numbers)

; A Son.L is one of: 
; – empty 
; – (cons Number Son.L)

	
; A Son.R is one of: 
; – empty 
; – (cons Number Son.R)
; 
; Constraint If s is a Son.R, 
; no number occurs twice in s.


; Son is used when it 
; applies to Son.L and Son.R


; Number Son -> Boolean
; is x in s
(check-expect (in? 5 '()) #false)
(check-expect (in? 5 (cons 2 '())) #false)
(check-expect (in? 5 (cons 5 (cons 2 '()))) #true)

(define (in? x s)
  (member? x s)
  )


; Number Son -> Son
; adds x to s
(check-expect (in? 5 (set+.L 5 '())) #true)
(check-expect (in? 5 (set+.L 5 (cons 2 '()))) #true)
(check-expect (in? 5 (set+.L 5 (cons 5 (cons 2 '())))) #true)
(check-expect (in? 9 (set+.L 5 (cons 2 '()))) #false)

(define (set+.L x s)
  (cons x s)
  )


; Number Son -> Son
; adds x to s
(check-expect (in? 5 (set+.L 5 '())) #true)
(check-expect (in? 5 (set+.L 5 (cons 2 '()))) #true)
(check-member-of
    (set+.R 5 (cons 5 '())) 
    (cons 5 '()) ; 5 is not added again
    )

(define (set+.R x s)
  (if
    (in? x s)
    s
    (cons x s)
    ))

(require test-engine/racket-tests)
(test)

