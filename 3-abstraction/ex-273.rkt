#lang htdp/isl

(require test-engine/racket-tests)

; ### Data Definitions

; ### Functions

; [A B] [A -> B] [List-of A] -> [List-of B]
(check-expect (map-clone add1 '()) '())
(check-expect (map-clone add1 '(1 2 3)) '(2 3 4))
(define (map-clone fn l)
  (local
    (; [A B] A B -> B
     (define (merge x acc)
       (cons (fn x) acc)
       ))

    (foldr merge '() l)
    ))

(test)

