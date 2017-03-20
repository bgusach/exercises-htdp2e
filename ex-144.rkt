#lang htdp/bsl

; Temperature is a Number greater than -273

; NotEmptyTempList is one of:
; - (cons Temperature '())
; - (cons Temperateure NotEmptyTempList)

; TempList -> Number
; Computes the average of the temperatures

(check-expect (temp-avg (cons 10 '()))  10)
(check-expect (temp-avg (cons 10 (cons 20 (cons 30 '())))) 20)

(define (temp-avg l)
  (/ (sum l) (how-many? l))
  )


; NotEmptyTempList -> Number
; Sums all the temperatures of a non empty temp list
(define (sum l)
  (cond
    [(empty? (rest l)) (first l)]
    [else 
      (+ 
        (first l)
        (sum (rest l))
        )]))


; NotEmptyTempList -> Number
; Counts how many elements a non empty list has

(check-expect (how-many? (cons 100 '())) 1)
(check-expect (how-many? (cons 30 (cons 100 '()))) 2)

(define (how-many? l)
  (cond
    [(empty? (rest l)) 1]
    [else
      (+ 1 (how-many? (rest l)))
      ]))


(require test-engine/racket-tests)
(test)
