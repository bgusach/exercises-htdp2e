#lang htdp/bsl


(require test-engine/racket-tests)

; String List-of-strings -> Boolean
; determines whether the string is on a-list-of-strings
(check-expect (contains? "Hey" '()) #false)
(check-expect (contains? "Flatt" (cons "Flatt" '())) #true)
(check-expect (contains? "Flatt" (cons "Johnny" (cons "Flatt" '()))) #true)
(check-expect (contains? "Magila Gorilla "(cons "Johnny" (cons "Bravo" '()))) #false)
(define (contains? needle alos)
  (cond
    [(empty? alos) #false]
    [(string=? (first alos) needle) #true]
    [else (contains? needle (rest alos))]
    ))

(test)
