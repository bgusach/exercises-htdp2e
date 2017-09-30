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





(test)

