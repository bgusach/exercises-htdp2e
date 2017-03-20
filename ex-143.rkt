#lang htdp/bsl

; Temperature is a Number greater than -273

; TempList is one of:
; - '()
; - (cons Temperature TempList)


; TempList -> Number
; Computes the average of the temperatures

(check-expect (temp-avg (cons 10 '()))  10)
(check-expect (temp-avg (cons 10 (cons 20 (cons 30 '())))) 20)

(define (temp-avg l)
  (/ (sum l) (how-many l))
  )


; TempList -> Number
; Checked version of temp-avg

(check-error (checked-temp-avg '()) "temp-avg: cannot calculate average of empty list")
(check-expect (checked-temp-avg (cons 10 (cons 20 (cons 30 '())))) 20)

(define (checked-temp-avg l)
  (cond
    [(zero? (how-many l)) (error "temp-avg: cannot calculate average of empty list")]
    [else (temp-avg l)]
    ))


; Alternatively, instead of making a checked version of the function, we 
; can reduce the domain of `temp-avg` by making a non-empty list data definition

; NotEmptyTempList is one of:
; - (cons Temperature '())
; - (cons Temperateure NotEmptyTempList)


; List-of-Number -> Number
; Sums all the numbers of a list
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
      (+
        (first l)
        (sum (rest l)))]))


; List-of-Any -> Number
; Counts how many elements a list has
(define (how-many l)
  (cond
    [(empty? l) 0]
    [else (+ 1 (how-many (rest l)))]))


(require test-engine/racket-tests)
(test)
