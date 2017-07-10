#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require racket/string)

; ### Functions
; [List-of [List-of String]] -> [List-of Number]
; Returns the number of words on each line
(check-expect (words-on-line '(("hola" "amigo") ("adios"))) '(2 1))
(define (words-on-line lls)
  (match 
    lls
    ['() '()]
    [(cons line rest-of-lines) 
     (cons 
       (length line) 
       (words-on-line rest-of-lines)
       )]))

(test)

