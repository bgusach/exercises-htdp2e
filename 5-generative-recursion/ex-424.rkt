#lang htdp/isl+

(require test-engine/racket-tests)

; ==================== Exercise 424 ====================

;                   '(11 9 2 18 12 14 4 1)
;                            |
;                +-----------11---------+
;                |                      |
;           '(9 2 4 1)             '(18 12 14)
;                |                      |
;           +----9---- -+          +-----18-----+
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
;                 |                      |
;            '(1 2 4 9)             '(12 14 18)
;                 |                      |
;                 +-----------+----------+
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
         )
        ; -- IN --
        (append
          (quick-sort< (smallers alon pivot))
          (list pivot)
          (quick-sort< (largers alon pivot))
          ))]))


; [List-of Number] Number -> [List-of Number]
(define (smallers alon pivot)
  (filter (λ (n) (< n pivot)))
  )


; [List-of Number] Number -> [List-of Number]
(define (larger alon pivot)
  (filter (λ (n) (> n pivot)))
  )


; =================== End of exercise ==================

; ==================== Exercise 424 ====================
(test)

