#lang htdp/bsl+
(require 2htdp/batch-io)
(require test-engine/racket-tests)


; ### Constants
; NOTE: no need to take a huge dict
(define DICT (read-lines "short-dictionary"))
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; ### Data Definitions
; A Dictionary is one of:
; - '()
; - (const String Dictionary)

; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"

; A Letter-Count is a structure
; (make-lc letter count)
(define-struct lc [letter count])


; ### Functions

; Dictionary -> Letter
; Returns the most frequent first word letter from a non-empty dictionary
(check-expect (most-frequent (list "lol" "troll" "tunnel" "cod")) "t")
(define (most-frequent dict)
  (lc-letter
    (max-freq-from-lolc
      (count-by-letter dict LETTERS)
      )))


; List-of-letter-count -> Letter-Count
; Returns the letter-count of the list with the greatest frequency
(check-expect 
  (max-freq-from-lolc (list (make-lc "a" 10) (make-lc "b" 3)))
  (make-lc "a" 10)
  )
(define (max-freq-from-lolc lolc)
  (cond
    [(empty? (rest lolc)) (first lolc)]
    [else 
      (max-freq-lc 
        (first lolc) 
        (max-freq-from-lolc (rest lolc))
        )]))


; Letter-Count Letter-Count -> Letter-Count
; Given two Letter-Counts, it returns the one with the greatest frequency
(check-expect 
  (max-freq-lc (make-lc "a" 10) (make-lc "b" 3))
  (make-lc "a" 10)
  )
(define (max-freq-lc a b)
  (if 
    (>= (lc-count a) (lc-count b))
    a
    b
    ))


; Dictionary List-of-letters -> List-of-Letter-Count
; Given a dictionary, it returs a list of Letter-Count with the frequencies
; of words by first letter
(check-expect (count-by-letter '() '()) '())
(check-expect 
  (count-by-letter (list "arrow" "available" "pig" "donkey") (list "a" "b" "p" "d"))
  (list (make-lc "a" 2) (make-lc "b" 0) (make-lc "p" 1) (make-lc "d" 1))
  )
(define (count-by-letter dict lol)
  (cond
    [(empty? lol) '()]
    ; NOTE: this is O(n^2), but I think we don't have the tools to make it better so far
    [else
      (cons
        (make-lc
          (first lol)
          (starts-with# (first lol) dict)
          )
        (count-by-letter dict (rest lol))
        )]))


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
    (first-letter str)
    first
    ))


; String -> 1-String
; Returns the first letter of a string
(define (first-letter str)
  (string-ith str 0)
  )


(test)

(write-file 
  'stdout 
  (format "The most frequent first letter is: ~a\n\n" (most-frequent DICT))
  )
