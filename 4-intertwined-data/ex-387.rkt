#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ==================== Exercise 387 ====================

; [List-of Symbol] [List-of Number] 
;    -> [List-of (cons Symbol (cons Number '()))]
(check-expect (cross '() '()) '())
(check-expect (cross '(a) '()) '())
(check-expect (cross '() '(1)) '())
(check-expect 
  (cross '(a b c) '(1 2))
  '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2))
  )
(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else 
      (append
        (combine (first los) lon)
        (cross (rest los) lon)
        )]))


; [X Y] X [List-of Y] -> [List-of (cons X (cons Y '()))]
(check-expect (combine 1 '()) '())
(check-expect (combine 1 '(a)) '((1 a)))
(check-expect (combine 1 '(a b)) '((1 a) (1 b)))
(define (combine el lst)
  (match
    lst
    ['() '()]
    [(cons head tail)
      (cons
        (list el head)
        (combine el tail)
        )]))


; =================== End of exercise ==================

(test)

