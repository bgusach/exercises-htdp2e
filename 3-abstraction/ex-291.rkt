#lang htdp/isl+

(require test-engine/racket-tests)

; ### Functions

; [A B] [A -> B] [List-of A] -> [List-of B]
(check-expect (map-via-fold add1 '()) '())
(check-expect (map-via-fold add1 '(1 2 3)) '(2 3 4))
(define (map-via-fold fn l)
  (foldr (λ (it acc) (cons (fn it) acc)) '() l)
  )

(test)

