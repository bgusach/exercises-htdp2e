#lang htdp/isl+

(require test-engine/racket-tests)


; ==================== Exercise 433 ====================

; [List-of 1String] N -> [List-of String]
; Bundles chunks of s into strings of length n
; idea take n items and drop n at a time
;
; Termination: (bundle non-empty-list 0) would loop forever
; and therefore an error is signaled in this case.
(check-expect 
  (bundle (explode "abcdefg") 3)
  (list "abc" "def" "g")
  )
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())
(check-error (bundle '("a") 0))
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [(zero? n) (error "Bundle size cannot be 0 if list not empty")]
    [else
      (cons 
        (implode (take s n)) 
        (bundle (drop s n) n)
        )]))

 
; [List-of X] N -> [List-of X]
; Keep the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else 
      (cons 
        (first l) 
        (take (rest l) (sub1 n))
        )]))
 

; [List-of X] N -> [List-of X]
; Remove the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]
    ))
 

; =================== End of exercise ==================




; ==================== Exercise 434 ====================

; [List-of Number] Number -> [List-of Number]
(check-expect (smallers '(5 3 1) 5) '(5 3 1))
(define (smallers l n)
  (cond
    [(empty? l) '()]
    [else 
      (if 
        (<= (first l) n)
        (cons (first l) (smallers (rest l) n))
        (smallers (rest l) n)
        )]))

; Q: What can go wrong when this version is used with the 
;    quick-sort< definition from Recursion that Ignores Structure? 
; A: This `smallers` function performs a less-than-or-equal 
;    comparision, causing the function call (smallers '(5 3 1) 5) 
;    to return '(5 3 1). 
;    This function is used in the quick-sort< to generate a subproblem 
;    (numbers smaller than the pivot), and may return the very list 
;    that was passed. E.g. if quick-sort< is passed '(5 3 1),
;    takes 5 as pivot and passes '(5 3 1) 5 to the `smallers` function,
;    that list will be passed again to quick-sort< and will recur
;    forever.
;
;    In other words: the generative step can create the same initial 
;    problem, resulting in endless recursion.

; =================== End of exercise ==================




; ==================== Exercise 435 ====================:

(check-expect (quick-sort< '(9 3 1 1 1 1 6)) '(1 1 1 1 3 6 9))
(check-expect (quick-sort< '()) '())
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [else
      (local
        ((define pivot (first alon))
         (define pivot-equals (filter (λ (x) (= x pivot)) alon))
         (define pivot-diffs (filter (λ (x) (not (= x pivot))) alon))
         )
        ; -- IN --
        (append
          (quick-sort< (smallers pivot-diffs pivot))
          pivot-equals
          (quick-sort< (largers pivot-diffs pivot))
          ))]))


(define (largers l n)
  (cond
    [(empty? l) '()]
    [else 
      (if 
        (>= (first l) n)
        (cons (first l) (largers (rest l) n))
        (largers (rest l) n)
        )]))


; =================== End of exercise ==================


(test)

