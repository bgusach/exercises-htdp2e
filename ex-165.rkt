#lang htdp/bsl

; ### Constants

; ### Data Definitions

; ### Functions

; List-of-toys -> List-of-toys
; given a list of one-word strings, it replaces the occurrences
; of "robot" with "r2d2"
(check-member-of (subst-robot '()) '())
(check-member-of 
  (subst-robot (cons "lol" (cons "troll" (cons "robot" '()))))
  (cons "lol" (cons "troll" (cons "r2d2" '())))
  )

(define (subst-robot lot)
  (cond
    [(empty? lot) '()]
    [else
      (cons
        (if 
          (string=? (first lot) "robot") 
          "r2d2"
          (first lot)
          )
        (subst-robot (rest lot))
        )]))


; List-of-strings String String -> List-of-strings
; given a list of strings los, it replaces the occurrences
; of the string s with r
(check-member-of (substitute '() "lol" "troll") '())
(check-member-of 
  (substitute (cons "lol" (cons "troll" (cons "robot" '()))) "lol" "troll")
  (cons "troll" (cons "troll" (cons "robot" '())))
  )

(define (substitute lot s r)
  (cond
    [(empty? lot) '()]
    [else
      (cons
        (if 
          (string=? (first lot) s) 
          r
          (first lot)
          )
        (substitute (rest lot) s r)
        )]))



(require test-engine/racket-tests)
(test)

