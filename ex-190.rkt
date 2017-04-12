#lang htdp/bsl+

; ### Constants

; ### Data Definitions

; ### Functions

; List-of-1strings -> List-of-list-of-1-strings
; Returns all the prefixes for the passed lo1s
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a")) (list (list "a")))
(check-expect 
  (prefixes (list "a" "b" "c")) 
  (list 
    (list "a" "b" "c")
    (list "a" "b")
    (list "a")
    ))

(define (prefixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [else
      (cons
        lo1s
        ; NOTE: dropping recursively the last item of a singly-linked list
        ; is not the most efficient way to do this, but I guess this is
        ; the expected solution to the exercise
        (prefixes (drop-last lo1s))
        )]))


; List-of-any -> List-of-any
; Given a list with **at least** one item, it returns a new list
; where the last item is missing
(check-expect (drop-last (list "a")) '())
(check-expect (drop-last (list "a" "b" "c")) (list "a" "b"))

(define (drop-last loa)
  (cond
    [(empty? loa) (error "drop-last: can't drop last item of empty list")]
    [(empty? (rest loa)) '()]
    [else
      (cons
        (first loa)
        (drop-last (rest loa))
        )]))
                        

; List-of-1strings -> List-of-list-of-1-strings
; Returns all the suffixes for the passed lo1s
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a")) (list (list "a")))
(check-expect 
  (suffixes (list "a" "b" "c")) 
  (list 
    (list "a" "b" "c")
    (list "b" "c")
    (list "c")
    ))

(define (suffixes lo1s)
  (cond
    [(empty? lo1s) '()]
    [else
      (cons
        lo1s
        (suffixes (rest lo1s))
        )]))


(require test-engine/racket-tests)
(test)

