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
; 
; I.e.:
;     (gcd L S) == (gcd S (remainder L S))
;
; E.g.:
;     (gcd 15 10)
;     (gcd 10 (remainder 15 10))
;     (gcd 10 5)
;     (gcd 5 (remainder 10 5))
;     (gcd 5 0)
;     ^^^ edge case: 0 can be evenly divided by any other number
;     5

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
;   (append '(2 3) '(6)
;     (quick-sort< '(8 9))
;     )
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append '(2 3) '(6)
;     (append
;       (quick-sort< '())
;       '(8)
;       (quick-sort< '(9))
;       ))
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append '(2 3) '(6)
;     (append '() '(8) '(9))
;     )
;   '(10)
;   (quick-sort< '(14 12 11 14 16))
;   )

; (append
;   (append '(2 3) '(6) '(8 9))
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
;   (append '(11 12) '(14 14)
;     (quick-sort< '(16))
;     ))

; (append
;   '(2 3 6 8 9)
;   '(10)
;   (append '(11 12) '(14 14) '(16))
;   )

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
; A: A naive approach would consist of dividing the calls
;    by the number of elements in the list:
;    ~1 quick-sort< calls/item
;    ~.5 append calls/item


; (quick-sort< '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))

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
; A: Just the estimations on time complexity. Truth is, quicksort
;    performs pretty bad on already sorted data.

; =================== End of exercise ==================




; ==================== Exercise 442 ====================

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


; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(define (sort< l)
  (cond
    [(empty? l) '()]
    [else 
      (insert 
        (first l) 
        (sort< (rest l))
        )]))
 

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else 
      (if 
        (< n (first l))
        (cons n l)
        (cons (first l) (insert n (rest l)))
        )]))


(define (create-random-list len)
  (cond
    [(zero? len) '()]
    [(cons (random 100) (create-random-list (sub1 len)))]
    ))


; Tests the passed algorithms against random data
(define (test-sort-algs name2fn len)
  (local
    ((define times 200)
     (define (measure fn)
       (time 
         (for ([_ (in-range times)]) 
           (fn (create-random-list len))
           ))))

    ; -- IN --
    (for [(pair name2fn)]
      (display (format "~a (len=~a): " (first pair) len))
      (measure (second pair))
      )))


(display "insert-sort vs quick-sort\n========================\n")
; NOTE: store the results on a dummy so that they don't get
; dumped on stdout
(define _ 
  (for [(size (in-range 0 40 2))]
    (test-sort-algs 
      `(("insert-sort" ,sort< ) 
        ("quick-sort" ,quick-sort<)
        )
      size
      )
    (display "\n")
    ))

; Q: Does the experiment confirm the claim that the plain 
;    sort< function often wins over quick-sort< for short 
;    lists and vice versa?
; A: Yes. Around length 32, quick-sort starts to win over
;    insert-sort.

(define CROSS-OVER 32)

(define (clever-sort< alon)
  (cond
    ; NOTE: calculating length of linked list on each
    ; iteration is not very efficient.
    [(< (length alon) CROSS-OVER) (sort< alon)]
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

(display "insert-sort vs quick-sort vs clever-sort\n")
(display "========================================\n")

(define _1
  (for [(size (in-range 0 40 2))]
    (test-sort-algs 
      `(("insert-sort" ,sort< ) 
        ("quick-sort" ,quick-sort<)
        ("clever-sort" ,clever-sort<)
        )
      size
      )
    (display "\n")
    ))

; NOTE: As expected, the clever-sort behaves like insert-sort
; with small lists, and like quick-sort with larger lists

; Q: Compare with excercise-427
; A: The only real difference is the threshold. In exercise 427
;    the threshold was 3 while here it is 32

; =================== End of exercise ==================




; ==================== Exercise 443 ====================

; Q: Why is it impossible to find a divisor with this strategy?:
; (define (gcd-structural n m)
;   (cond
;     [(and (= n 1) (= m 1)) ...]
;     [(and (> n 1) (= m 1)) ...]
;     [(and (= n 1) (> m 1)) ...]
;     [else
;      (... (gcd-structural (sub1 n) (sub1 m)) ...
;       ... (gcd-structural (sub1 n) m) ...
;       ... (gcd-structural n (sub1 m)) ...)]))
;
; A: Easy to see, hard to explain. I would say because 
;    the recursive step is generating new smaller
;    problems that do not have anything to do with the
;    original problem, and therefore cannot help us solve it.
;    For instance, if we are trying to find (gcd 12 8)
;    solving (gcd 11 7) or (gcd 12 7) or (gcd 11 8) does not
;    help us so much.


; =================== End of exercise ==================




; ==================== Exercise 444 ====================

; See this two-stage approach to solve the GCD problem:

;     (define (gcd-structural S L)
;       (largest-common (divisors S S) (divisors S L)))
; 
;     ; N[>= 1] N[>= 1] -> [List-of N]
;     ; computes the divisors of l smaller or equal to k
;     (define (divisors k l)
;       '())
; 
;     ; [List-of N] [List-of N] -> N
;     ; finds the largest number common to both k and l
;     (define (largest-common k l)
;       1)

; Q: Why do you think divisors consumes two numbers? 
; A: Two reasons:
;    - So that it can start checking the divisors from a given number, 
;      instead of the very number we want to find divisors for.
;    - Because the function will recur, and in each call we still
;      need access to the number we want to find divisors for.

; Q: Why does it consume S as the first argument in both uses? 
; A: - When calculating the divisors of the smaller number, 
;      because all its divisors are candidates to be divisors of
;      the larger one as well. 
;    - When calculating the divisors of the larger number,
;      because we do not care about divisors greater than the smaller one,
;      since they could never ever be found in the divisors of the smaller
;      one as well. For example, when calculating (gcd 6 10), 
;      we do not care about the divisors of 10 in (6, 10], thus they just
;      cannot be divisors of 6.
;      
;      However, if we calculated ALL divisors of the larger number,
;      the algorithm would still be correct, just less efficient.


; =================== End of exercise ==================

(test)

