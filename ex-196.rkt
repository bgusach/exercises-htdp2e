#lang htdp/bsl+
(require 2htdp/batch-io)
(require racket/string)

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


; List-of-letter-count Letter -> List-of-letter-count
; "Updates" the lolc with a new letter
(define (upsert-lolc lolc letter)
  lolc
  )

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


(require test-engine/racket-tests)
(test)


(define (format-lolc lolc) 
  (cond
    [(empty? lolc) '()]
    [else
      (cons
        (format "~s: ~a" (lc-letter (first lolc)) (lc-count (first lolc)))
        (format-lolc (rest lolc))
        )]))


(write-file 
  'stdout 
  (string-append 
    "Frequencies of first letters in dict:\n"
    (string-join 
      (format-lolc (count-by-letter DICT LETTERS))
      "\n"
      )
    "\n"
    ))
