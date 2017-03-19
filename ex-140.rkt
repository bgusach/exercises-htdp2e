#lang htdp/bsl

; A List-of-bools is one of
; - '()
; - (cons Boolean List-of-bools)


; Boolean -> Boolean
; Returns whether the passed value is #true
(define (true? v)
  (eq? v #true)
  )


; List-of-bools -> Boolean
; Returns whether all the elements of the list are true, or in other words
; there is no #false in the list
(check-expect (all-true '()) #true)
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(define (all-true lob)
  (cond
    [(empty? lob) #true]
    [(cons? lob) 
     (and
       (true? (first lob))
       (all-true (rest lob))
       )]))


(require test-engine/racket-tests)
(test)

