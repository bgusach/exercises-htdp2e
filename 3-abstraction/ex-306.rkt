#lang htdp/isl

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (fn1 5) '(0 1 2 3 4))
(define (fn1 n)
  (for/list ([x n]) x)
  )


; creates the list (list 1 ... n) for any natural number n
(check-expect (fn2 5) '(1 2 3 4 5))
(define (fn2 n)
  (for/list ([it (in-range 1 (add1 n) 1)]) it)
  )


; creates the list (list 1 1/2 ... 1/n) for any natural number n
(check-expect (fn3 5) '(1 1/2 1/3 1/4 1/5))
(define (fn3 n)
  (for/list ([it (in-range 1 (add1 n) 1)]) (/ 1 it))
  )

; creates the list of the first n even numbers
(check-expect (fn4 5) '(0 2 4 6 8))
(define (fn4 n)
  (for/list ([it n]) (* 2 it))
  )

; creates a diagonal square of 0s and 1s
(check-expect 
  (fn5 3) 
  '((1 0 0)
    (0 1 0)
    (0 0 1)
    ))
(define (fn5 n)
  (for/list ([i n]) 
    (for/list ([j n])
      (if (= i j) 1 0)
      )))

; tabulate data
(check-expect (tabulate add1 2) '(3 2 1))
(define (tabulate fn n)
  (for/list ([it (add1 n)])
    (fn (- n it))
    ))

(test)

