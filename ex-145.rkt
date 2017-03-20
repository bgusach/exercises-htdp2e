#lang htdp/bsl

; Temperature is a Number greater than -273

; NotEmptyTempList is one of:
; - (cons Temperature '())
; - (cons Temperateure NotEmptyTempList)


; NotEmptyTempList -> Boolean

(check-expect (sorted? (cons 100 '())) #true)
(check-expect (sorted? (cons 3 (cons 1 '()))) #true)
(check-expect (sorted? (cons 1 (cons 3 '()))) #false)

(define (sorted? l)
  (cond
    [(empty? (rest l)) #true]
    [else
      (and
        (>= 
          (first l)
          (first (rest l))
          )
        (sorted? (rest l))
        )]))


(require test-engine/racket-tests)
(test)
