#lang htdp/bsl+

(require test-engine/racket-tests)


(check-expect
  (list 0 1 2 3 4 5)
  (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '()))))))
  )


(check-expect
  (list (list "he" 0) (list "it" 1) (list "lui" 14))
  (cons
    (cons "he" (cons 0 '()))
    (cons
      (cons "it" (cons 1 '()))
      (cons
        (cons "lui" (cons 14 '()))
        '()
        ))))


(check-expect
  (list 1 (list 1 2) (list 1 2 3))
  (cons
    1
    (cons 
      (cons 1 (cons 2 '()))
      (cons
        (cons 1 (cons 2 (cons 3 '())))
        '()
        ))))


(test)
