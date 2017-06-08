#lang htdp/bsl+

(require test-engine/racket-tests)

; Example 1
(define quoted1 '(1 "a" 2 #false 3 "c"))
(define with-lists1 (list 1 "a" 2 #false 3 "c"))
(define consed1 (cons 1 (cons "a" (cons 2 (cons #false (cons 3 (cons "c" '())))))))
(check-expect quoted1 with-lists1)
(check-expect quoted1 consed1)

; Example 2
(define quoted2 '())
; NOTE: I would not say there is too much to do here. It is already the
; symbol for the empty list

; Example 3
(define 
  quoted3
  '(("alan" 1000)
    ("barb" 2000)
    ("carl" 1500)
    ("dawn" 2300)
    ))

(define
  with-lists3
  (list
    (list "alan" 1000)
    (list "barb" 2000)
    (list "carl" 1500)
    (list "dawn" 2300)
    ))


(check-expect quoted3 with-lists3)

(test)

