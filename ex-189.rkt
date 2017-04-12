#lang htdp/bsl+

; ### Constants

; ### Data Definitions

; ### Functions
; Number List-of-numbers -> Boolean
; Determines whether a given number is present in a sorted> list of numbers
(check-expect (search-sorted 1 '()) #false)
(check-expect (search-sorted 1 (list 3 2 1)) #true)
(check-expect (search-sorted 8 (list 3 2 1)) #false)
; NOTE: I think this is how the exercise is intended to be resolved
; but a sorted list calls for a binary search instead, although
; it is not so easy to implement it in a linked list
(define (search-sorted n alon)
  (cond
    [(empty? alon) #false]
    [(< (first alon) n) #false]
    [(= (first alon) n) #true]
    [else (search-sorted n (rest alon))]
     ))

(require test-engine/racket-tests)
(test)

