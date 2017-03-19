#lang htdp/bsl

; A List-of-numbers is one of
; - '()
; - (cons PositiveNumber List-of-numbers)


; A List-of-numbers is one of   <-----
; - '()                              |
; - (cons Number List-of-numbers)    |
;                         ^          |
;                         |-----------


; List-of-numbers -> Number
; Checks whether all the numbers in the list are positive
(check-expect (pos? '()) #true)
(check-expect (pos? (cons 10 '())) #true)
(check-expect (pos? (cons 5 (cons -10 '()))) #false)
(define (pos? lon)
  (cond
    [(empty? lon) #true]
    [(cons? lon)
     (and (>= (first lon) 0) (pos? (rest lon)))]
    [else #false]
    ))


; List-of-amounts -> PositiveNumber
; sums all the amounts of a list
(define (sum loa)
  (cond
    [(empty? loa) 0]
    [(cons? loa)
     (+ (first loa) (sum (rest loa)))]
    ))


; List-of-amounts -> Number
; checked version of sum
(check-expect (checked-sum '()) 0)
(check-expect (checked-sum (cons 10 '())) 10)
(check-error (checked-sum "lol"))
(check-error (checked-sum (cons -5 (cons 10 '()))))
(define (checked-sum lon) 
  (cond
    [(pos? lon) (sum lon)]
    [else (error "pos?: lon should be a list of amounts")]
    ))


(require test-engine/racket-tests)
(test)

