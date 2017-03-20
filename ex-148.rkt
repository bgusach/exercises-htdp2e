#lang htdp/bsl

; A NEList-of-bools is one of
; - (cons Boolean '())
; - (cons Boolean NEList-of-bools)


; NEList-of-bools -> Boolean
; Returns whether all the elements of the list are true
(check-expect (all-true (cons #true '())) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(define (all-true lob)
  (cond
    [(empty? (rest lob)) (first lob)]
    [else
     (and
       (first lob)
       (all-true (rest lob))
       )]))


(require test-engine/racket-tests)
(test)

