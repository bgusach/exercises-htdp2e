#lang htdp/isl

(require test-engine/racket-tests)

; ### Functions
; Nelon -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (local ((define smallest-in-rest (inf.v2 (rest l))))
       (if (< (first l) smallest-in-rest)
           (first l)
           smallest-in-rest))]))

(inf.v2 '(10 4 1))


; Nelon -> Number
; determines the largest 
; number on l
(define (sup.v2 l)
  (local
    ((define head (first l))
     (define tail (rest l)))

    ; -- IN --
    (cond
      [(empty? tail) head]
      [else
        (local 
          ((define biggest-in-rest (sup.v2 tail)))

          ; -- IN --     
          (if 
            (> head biggest-in-rest)
            head
            biggest-in-rest
            ))])))


(sup.v2 '(1 5 2))

(test)

