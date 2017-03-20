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


; NEList-of-bools -> Boolean
; Returns whether at least one element of the list is #true

(check-expect (one-true (cons #true '())) #true)
(check-expect (one-true (cons #false '())) #false)
(check-expect (one-true (cons #true (cons #true '()))) #true)
(check-expect (one-true (cons #true (cons #false '()))) #true)

(define (one-true lob)
  (cond
    [(empty? (rest lob)) (first lob)]
    [else
      (or
        (first lob)
        (one-true (rest lob))
        )]))



(require test-engine/racket-tests)
(test)

