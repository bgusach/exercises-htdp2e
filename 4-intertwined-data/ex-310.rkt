#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ### Data Definitions

(define-struct no-parent [])
(define NP (make-no-parent))


(define-struct child [father mother name date eyes])
; A FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; ### Constants

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))


; ==================== Exercise 310 ====================
; ### Functions

; FT -> Number
; Counts the nodes in the tree
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Adam) 3)
(check-expect (count-persons Gustav) 5)
(define (count-persons tree)
  (match
    tree
    [(no-parent) 0]
    [(child ma pa _ _ _) 
     (+ 1 (count-persons ma) (count-persons pa))
     ]
    ))


; =================== End of exercise ==================

(test)

