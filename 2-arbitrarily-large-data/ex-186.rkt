#lang htdp/bsl

(require test-engine/racket-tests)

; Temperature is a Number greater than -273

; NotEmptyTempList is one of:
; - (cons Temperature '())
; - (cons Temperateure NotEmptyTempList)


; NotEmptyTempList -> Boolean

(check-expect (sorted>? (cons 100 '())) #true)
(check-expect (sorted>? (cons 3 (cons 1 '()))) #true)
(check-expect (sorted>? (cons 1 (cons 3 '()))) #false)


(define (sorted>? l)
  (cond
    [(empty? (rest l)) #true]
    [else
      (and
        (>= 
          (first l)
          (first (rest l))
          )
        (sorted>? (rest l))
        )]))


; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-satisfied (sort> (list 1 10 2 9 3 56)) sorted>?)
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]
    ))
 

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else 
      (if 
        (>= n (first l))
        (cons n l)
        (cons (first l) (insert n (rest l)))
        )]))


; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort>/bad l)
  (list 9 8 7 6 5 4 3 2 1 0)
  )

; Q: Can you formulate a test case that shows sort>/bad 
; is not a sorting function? 
; A:
(check-expect (sort>/bad (list 4 9 0)) (list 9 4 0))

; Q: Can you use check-satisfied to formulate this test case?
; A: Unless we hardcode the input list into the function that has 
;    to be satisfied, no, we cannot.

(test)
