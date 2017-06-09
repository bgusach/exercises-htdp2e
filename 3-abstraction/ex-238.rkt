#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

(define 
  long-list-1
  (list 
    1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
    17 18 19 20 21 22 23 24 25
      ))

(define 
  long-list-2
  (list 
    25 24 23 22 21 20 19 18 17 16 15 14 13
    12 11 10 9 8 7 6 5 4 3 2 1
      ))

; ### Data Definitions

; ### Functions

(define (abstract lon comp)
  (cond
    [(empty? (rest lon)) (first lon)]
    [else
      (if 
        (comp (first lon) (abstract (rest lon) comp))
        (first lon)
        (abstract (rest lon) comp)
        )]))


(check-expect (inf-1 (list 10 5 2)) 2)
(define (inf-1 lon)
  (abstract lon <)
  )

(check-expect (sup-1 (list 23 24 25)) 25)
(define (sup-1 lon)
  (abstract lon >)
  )


(define (abstract-2 lon reducer)
  (cond
    [(empty? (rest lon)) (first lon)]
    [else
      (reducer
        (first lon)
        (abstract-2 (rest lon) reducer)
        )]))


(check-expect (inf-2 (list 10 5 2)) 2)
(define (inf-2 lon)
  (abstract-2 lon min)
  )


(check-expect (sup-2 (list 10 5 2)) 10)
(define (sup-2 lon)
  (abstract-2 lon max)
  )

"(inf-1 long-list-1))"
(time (inf-1 long-list-1))
"(inf-1 long-list-2)"
(time (inf-1 long-list-2))

"(sup-1 long-list-1)"
(time (sup-1 long-list-1))
"(sup-1 long-list-2)"
(time (sup-1 long-list-2))

"(inf-2 long-list-1))"
(time (inf-2 long-list-1))
"(inf-2 long-list-2)"
(time (inf-2 long-list-2))

"(sup-2 long-list-1))"
(time (sup-2 long-list-1))
"(sup-2 long-list-2)"
(time (sup-2 long-list-2))

(test)

