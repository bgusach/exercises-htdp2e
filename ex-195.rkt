#lang htdp/bsl+
(require 2htdp/batch-io)

; ### Constants
; NOTE: no need to take a huge dict
(define DICT (read-lines "short-dictionary"))

; A Dictionary is one of:
; - '()
; - (const String Dictionary)


; ### Data Definitions
; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))


; ### Functions

; Letter Dictionary -> Number
; Counts how many words start with letter in the dict
(check-expect (starts-with# "a" (list "arrow" "available" "pig" "donkey")) 2)
(check-expect (starts-with# "z" (list "arrow" "pig" "donkey")) 0)
(define (starts-with# letter dict)
  (cond
    [(empty? dict) 0]
    [else
      (+
        (if (starts-with? (first dict) letter) 1 0)
        (starts-with# letter (rest dict))
        )]))


; String 1-String -> Boolean
; Checks whether str strings with first
(check-expect (starts-with? "lol" "l") #true)
(check-expect (starts-with? "lol" "z") #false)
(define (starts-with? str first)
  (string=?
    (string-ith str 0)
    first
    ))


(require test-engine/racket-tests)
(test)


(write-file 
  'stdout 
  (format 
    "There are ~a words starting with the letter 'a'\n"
    (starts-with# "a" DICT)
    ))

(write-file 
  'stdout 
  (format 
    "There are ~a words starting with the letter 'd'\n"
    (starts-with# "d" DICT)
    ))

