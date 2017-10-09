#lang htdp/isl+

(require test-engine/racket-tests)
(require racket/base)


; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m
(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)
(define (gcd-structural n m)
  (local
    ((define (greatest-divisor-<= i)
       (cond
         ; [(= i 1) 1]  ; this check is unnecessary
         [(= (remainder n i) (remainder m i) 0) i]
         [else (greatest-divisor-<= (sub1 i))]
         )))

    ; -- IN --
    (greatest-divisor-<= (min n m))
    ))
  

; ==================== Exercise 438 ====================

; Q: In your words: how does greatest-divisor-<= work?
; A: First, it takes the small one of the numbers (the 
;    greatest common divisor of two numbers cannot be 
;    greater than one of them). Then starting from the
;    small number, it checks all the numbers all the
;    way down to 1... and if anything satisfies the gcd
;    condition, it is returned. In the worst case 1 is
;    a gcd, so the algorithm always terminates.

; =================== End of exercise ==================




; ==================== Exercise 439 ====================

(time (gcd-structural 101135853 45014640))

; =================== End of exercise ==================


; Insight (euclidian algorithm): for two natural numbers, 
; L for large and S for small, the greatest common divisor 
; is equal to the greatest common divisor of S and the 
; remainder of L divided by S.
; I.e.:
;     (gcd L S) == (gcd S (remainder L S))

(check-expect (gcd-generative 6 25) 1)
(check-expect (gcd-generative 18 24) 6)
(define (gcd-generative n m)
  (local
    ((define (clever-gcd L S)
       (cond
         [(zero? S) L]
         [else (clever-gcd S (remainder L S))]
         )))
    ; -- IN --
    (clever-gcd (max n m) (min n m))
    ))


; ==================== Exercise 440 ====================

(time (gcd-generative 101135853 45014640))

; =================== End of exercise ==================




; ==================== Exercise 441 ====================

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

(define (smallers l n)
  (cond
    [(empty? l) '()]
    [else 
      (if 
        (<= (first l) n)
        (cons (first l) (smallers (rest l) n))
        (smallers (rest l) n)
        )]))

(require racket/trace)
(trace quick-sort<)

; (quick-sort< '(10 6 8 9 14 12 3 11 14 16 2))

; (append
;   (quick-sort< '(6 8 9 3 2))
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append
;     (quick-sort< '(3 2))
;     '(6)
;     (quick-sort< '(8 9))
;     )
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append
;     (append
;       (quick-sort< '(2))
;       '(3)
;       (quick-sort< '())
;       )
;     '(6)
;     (quick-sort< '(8 9))
;     )
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append
;     (append '(2) '(3) '())
;     '(6)
;     (quick-sort< '(8 9))
;     )
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append
;     '(2 3)
;     '(6)
;     (quick-sort< '(8 9))
;     )
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append
;     '(2 3)
;     '(6)
;     (append
;       (quick-sort< '())
;       '(8)
;       (quick-sort< '(9))
;       ))
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append
;     '(2 3)
;     '(6)
;     (append '() '(8) '(9))
;     )
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append
;     '(2 3)
;     '(6)
;     '(8 9)
;     )
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   '(2 3 6 8 9)
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   '(2 3 6 8 9)
;   '(10)
;   (append
;     (quick-sort< '(12 11))
;     '(14 14)
;     (quick-sort< '(16))
;     ))

; (append
;   '(2 3 6 8 9)
;   '(10)
;   (append
;     (append
;       (quick-sort< '(11))
;       '(12)
;       (quick-sort< '())
;       )
;     '(14 14)
;     (quick-sort< '(16))
;     ))

; (append
;   '(2 3 6 8 9)
;   '(10)
;   (append
;     (append '(11) '(12) '())
;     '(14 14)
;     (quick-sort< '(16))
;     ))

; (append
;   '(2 3 6 8 9)
;   '(10)
;   (append
;     '(11 12)
;     '(14 14)
;     (quick-sort< '(16))
;     ))

; (append
;   '(2 3 6 8 9)
;   '(10)
;   (append
;     '(11 12)
;     '(14 14)
;     '(16)
;      ))

; (append
;   '(2 3 6 8 9)
;   '(10)
;   '(11 12 14 14 16)
;   )

; '(2 3 6 8 9 10 11 12 14 14 16)


; Q: How many recursive applications of quick-sort< are required? 
; A: 12 (13 in total)

; Q: How many recursive applications of the append function?
; A: 5 (6 in total)

; Q: Suggest a general rule for a list of length n.
; A: for each element in the list:
;    ~1 quick-sort< call 
;    ~.5 append calls


(quick-sort< '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))

; (append
;   (quick-sort< '())
;   '(1)
;   (quick-sort< '(2 3 4 5 6 7 8 9 10 11 12 13 14))
;   )

; (append '() '(1)
;   (quick-sort< '(2 3 4 5 6 7 8 9 10 11 12 13 14))
;   )

; (append '() '(1)
;   (append
;     (quick-sort< '())
;     '(2)
;     (quick-sort< '(3 4 5 6 7 8 9 10 11 12 13 14))
;     ))

; (append '() '(1)
;   (append '() '(2)
;     (append
;       (quick-sort< '())
;       '(3)
;       (quick-sort< '(4 5 6 7 8 9 10 11 12 13 14))
;       )))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append
;         (quick-sort< '())
;         '(4)
;         (quick-sort< '(5 6 7 8 9 10 11 12 13 14))
;         ))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append
;           (quick-sort< '())
;           '(5)
;           (quick-sort< '(6 7 8 9 10 11 12 13 14))
;           )))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append '() '(5)
;           (append
;             (quick-sort< '())
;             '(6)
;             (quick-sort< '(7 8 9 10 11 12 13 14))
;             ))))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append '() '(5)
;           (append '() '(6)
;             (append
;               (quick-sort< '())
;               '(7)
;               (quick-sort< '(8 9 10 11 12 13 14))
;               )))))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append '() '(5)
;           (append '() '(6)
;             (append '() '(7)
;               (append
;                 (quick-sort< '())
;                 '(8)
;                 (quick-sort< '(9 10 11 12 13 14))
;                 )))))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append '() '(5)
;           (append '() '(6)
;             (append '() '(7)
;               (append '() '(8)
;                 (append
;                   (quick-sort< '())
;                   '(9)
;                   (quick-sort< '(10 11 12 13 14))
;                   ))))))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append '() '(5)
;           (append '() '(6)
;             (append '() '(7)
;               (append '() '(8)
;                 (append '() '(9)
;                   (append
;                     (quick-sort< '())
;                     '(10)
;                     (quick-sort< '(11 12 13 14))
;                     )))))))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append '() '(5)
;           (append '() '(6)
;             (append '() '(7)
;               (append '() '(8)
;                 (append '() '(9)
;                   (append '() '(10)
;                     (append
;                       (quick-sort< '())
;                       '(11)
;                       (quick-sort< '(12 13 14))
;                       ))))))))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append '() '(5)
;           (append '() '(6)
;             (append '() '(7)
;               (append '() '(8)
;                 (append '() '(9)
;                   (append '() '(10)
;                     (append '() '(11)
;                       (append
;                         (quick-sort< '())
;                         '(12)
;                         (quick-sort< '(13 14))
;                         )))))))))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append '() '(5)
;           (append '() '(6)
;             (append '() '(7)
;               (append '() '(8)
;                 (append '() '(9)
;                   (append '() '(10)
;                     (append '() '(11)
;                       (append '() '(12)
;                         (append
;                           (quick-sort< '())
;                           '(13)
;                           (quick-sort< '(14))
;                           ))))))))))))

; (append '() '(1)
;   (append '() '(2)
;     (append '() '(3)
;       (append '() '(4)
;         (append '() '(5)
;           (append '() '(6)
;             (append '() '(7)
;               (append '() '(8)
;                 (append '() '(9)
;                   (append '() '(10)
;                     (append '() '(11)
;                       (append '() '(12)
;                         (append '() '(13) '(14))
;                         ))))))))))))


; Q: How many recursive applications of quick-sort< are required? 
; A: 25 (26 in total)

; Q: How many recursive applications of the append function?
; A: 12 (13 in total)

; Q: Does this contradict the first part of the exercise?
; A: Just my estimations on time complexity. Truth is, quicksort
;    performs pretty bad on already sorted data.

; =================== End of exercise ==================

(test)

