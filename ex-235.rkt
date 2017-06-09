#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

; ### Functions

; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else 
      (or 
        (string=? (first l) s)
        (contains? s (rest l)))
      ]))


(define (contains-atom? los)
  (contains? "atom" los)
  )


(define (contains-basic? los)
  (contains? "basic" los)
  )


(check-expect (contains-zoo? '("lol" "troll" "n00b")) #false)
(check-expect (contains-zoo? '("lol" "troll" "n00b" "zoo")) #true)
(define (contains-zoo? los)
  (contains? "zoo" los)
  )

(test)

