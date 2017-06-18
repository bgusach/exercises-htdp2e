#lang htdp/bsl+

(require test-engine/racket-tests)


(check-expect 
  (cons "a" (cons "b" (cons "c" (cons "d" '()))))
  (list "a" "b" "c" "d")
  )


(check-expect
  (cons (cons 1 (cons 2 '())) '())
  (list (list 1 2))
  )


(check-expect
  (cons "a" (cons (cons 1 '()) (cons #false '())))
  (list "a" (list 1) #false)
  )


(check-expect
  (cons (cons "a" (cons 2 '())) (cons "hello" '()))
  (list (list "a" 2) "hello")
  )


(check-expect
  (cons 
    (cons 1 (cons 2 '()))
      (cons 
        (cons 2 '()) 
        '()))
  (list (list 1 2) (list 2))
  )


(test)
