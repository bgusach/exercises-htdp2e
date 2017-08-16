#lang htdp/isl+

(require test-engine/racket-tests)


; ==================== Exercise 424 ====================

;                   '(11 9 2 18 12 14 4 1)
;                            |
;                +-----------11----------+
;                |                       |
;           '(9 2 4 1)              '(18 12 14)
;                |                       |
;           +----9------+          +-----18-----+
;           |           |          |            |
;        '(2 4 1)      '()     '(12 14)        '()
;           |           |          |            |
;      +----2----+      |     +----12---+       |
;      |         |      |     |         |       |
;    '(1)      '(4)     |    '()      '(14)     |
;      |         |      |     |         |       |
;   +--1--+   +--4--+   |     |      +--14--+   |
;   |     |   |     |   |     |      |      |   |
;  '()   '() '()   '()  |     |     '()    '()  |
;   |     |   |     |   |     |      |      |   |
;   +--+--+   +--+--+   |     |      +---+--+   |
;      |         |      |     |          |      |
;    '(1)      '(4)     |     |        '(14)    |
;      |         |      |     |          |      |
;      +----+----+      |     +----+-----+      |
;           |           |          |            |
;       '(1 2 4)        |      '(12 14)         |
;           |           |          |            |
;           +-----+-----+          +------+-----+
;                 |                       |
;            '(1 2 4 9)              '(12 14 18)
;                 |                       |
;                 +-----------+-----------+
;                             |
;                  '(1 2 4 9 11 12 14 18)


; [List-of Number] -> [List-of Number]
; Sorts alon
(check-expect (quick-sort< '(9 3 1 6)) '(1 3 6 9))
(check-expect (quick-sort< '()) '())
(define (quick-sort< alon)
  (cond
    [(empty? alon) '()]
    [else
      (local
        ((define pivot (first alon))
         (define tail (rest alon))
         )
        ; -- IN --
        (append
          (quick-sort< (smallers tail pivot))
          (list pivot)
          (quick-sort< (largers tail pivot))
          ))]))

; =================== End of exercise ==================





; ==================== Exercise 425 ====================

; [List-of Number] Number -> [List-of Number]
; Collects numbers of `alon` that are smaller than `pivot`
(define (smallers alon pivot)
  (filter (λ (n) (< n pivot)) alon)
  )


; [List-of Number] Number -> [List-of Number]
; Collects numbers of `alon` that are larger than `pivot`
(define (largers alon pivot)
  (filter (λ (n) (> n pivot)) alon)
  )

; =================== End of exercise ==================




; ==================== Exercise 426 ====================

(check-expect (quick-sort<.v2 '(9 3 1 6)) '(1 3 6 9))
(check-expect (quick-sort<.v2 '()) '())
(define (quick-sort<.v2 alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [else
      (local
        ((define head (first alon))
         (define tail (rest alon))
         )
        ; -- IN --
        (append
          (quick-sort<.v2 (smallers tail head))
          (list head)
          (quick-sort<.v2 (largers tail head))
          ))]))

; Q: How many steps does the revised algorithm save?
; A: initial=232 steps, revised=164 steps, savings=68

; =================== End of exercise ==================




; ==================== Exercise 427 ====================


(define INSERT-SORT-THRESHOLD 3)

(check-expect (quick-sort<.v3 '(9 3 1 6)) '(1 3 6 9))
(check-expect (quick-sort<.v3 '()) '())
(define (quick-sort<.v3 alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [(< (length alon) INSERT-SORT-THRESHOLD) 
     (sort< alon)
     ]
    [else
      (local
        ((define head (first alon))
         (define tail (rest alon))
         )
        ; -- IN --
        (append
          (quick-sort<.v3 (smallers tail head))
          (list head)
          (quick-sort<.v3 (largers tail head))
          ))]))


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


; =================== End of exercise ==================




; ==================== Exercise 428 ====================

(check-expect (quick-sort<.v4 '(9 3 1 1 1 1 6)) '(1 1 1 1 3 6 9))
(check-expect (quick-sort<.v4 '()) '())
(define (quick-sort<.v4 alon)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [else
      (local
        ((define head (first alon))
         )
        ; -- IN --
        (append
          (quick-sort<.v4 (smallers alon head))
          (equals alon head)
          (quick-sort<.v4 (largers alon head))
          ))]))


; [List-of Number] Number -> [List-of Number]
; Returns a list containing only the elemens equal to `n`
(define (equals lon n)
  (filter (λ (x) (= x n)) lon)
  )

; NOTE: another option would have been either that `smallers`
; or `largers` does an inclusive comparison (<= or >=), and
; then re-sort again and again... but seems less efficient
; than collecting the pivots and merging them between the 
; sorted branches.

; =================== End of exercise ==================




; ==================== Exercise 429 ====================

; Already done in ex-425 :)

; =================== End of exercise ==================




; ==================== Exercise 430 ====================


; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; Sorts `alon` according to `cmp`
(check-expect (quick-sort<.v5 '(9 3 1 6) <) '(1 3 6 9))
(check-expect (quick-sort<.v5 '(9 3 1 6) >) '(9 6 3 1))
(check-expect (quick-sort<.v5 '() <) '())
(define (quick-sort<.v5 alon cmp)
  (cond
    [(empty? alon) '()]
    [(empty? (rest alon)) alon]
    [else
      (local
        ((define pivot (first alon))
         (define tail (rest alon))
         )

        ; -- IN --
        (append
          (quick-sort<.v5 
            (filter (λ (n) (cmp n pivot)) tail)
            cmp
            )
          (list pivot)
          (quick-sort<.v5 
            (filter (λ (n) (not (cmp n pivot))) tail)
            cmp
            )))]))

; =================== End of exercise ==================

(test)

