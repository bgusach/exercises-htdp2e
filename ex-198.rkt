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

; Dictionary -> List-of-dictionaries
; Groups words by first letter. If a letter is not used to start any word, there won't be a list
; for words starting with that letter
(check-expect (words-by-first-letter '()) '())
(check-expect 
  (words-by-first-letter (list "asterisk" "asteroid" "beer" "burrito" "zealot")) 
  (list
    (list "asterisk" "asteroid")
    (list "beer" "burrito")
    (list "zealot")
    ))
(define (words-by-first-letter dict)
  (filter-out-empty-lists (group-by-first-letter dict LETTERS))
  )


; Dictionary -> List-of-dictionaries
; Groups words by first letter. If a letter is not used to start any word, there will be an
; empty list for words starting with that letter
(define (group-by-first-letter dict letters)
  (cond
    [(empty? letters) '()]
    [else
      (cons
        (filter-starting-with dict (first letters))
        (group-by-first-letter dict (rest letters))
        )]))


; List-of-lists-of-anything -> List-of-lists-of-anything
; Filters out inner empty lists
(check-expect 
  (filter-out-empty-lists (list (list "a") '()))
  (list (list "a"))
  )
(define (filter-out-empty-lists lol)
  (cond
    [(empty? lol) '()]
    [(empty? (first lol)) (filter-out-empty-lists (rest lol))]
    [else 
     (cons 
       (first lol) 
       (filter-out-empty-lists (rest lol))
       )]))


; Dictionary Letter -> Dictionary
; Filters out words not starting by letter
(check-expect 
  (filter-starting-with (list "hola" "hello" "ciao") "c") 
  (list "ciao")
  )
(define (filter-starting-with dict letter)
  (cond
    [(empty? dict) '()]
    [(starts-with? (first dict) letter)
     (cons
       (first dict)
       (filter-starting-with (rest dict) letter)
       )]
    [else (filter-starting-with (rest dict) letter)]
    ))

; Returns the most frequent first word letter from a non-empty dictionary
(check-expect (most-frequent (list "lol" "troll" "tunnel" "cod")) "t")
(define (most-frequent dict)
  (lc-letter
    (max-freq-from-lolc
      (count-by-letter dict LETTERS)
      )))

; Returns the most frequent first word letter from a non-empty dictionary
(check-expect (most-frequent.v2 (list "lol" "troll" "tunnel" "cod")) "t")
(define (most-frequent.v2 dict)
  (first-letter
    (first
      (longest
        (words-by-first-letter dict)
      ))))


(check-expect
  (most-frequent DICT)
  (most-frequent.v2 DICT)
  )


; List-of-list-of-anything -> List-of-anything
; Returns the longest list
(check-expect (longest (list (list 1 2) (list 1) '())) (list 1 2))
(define (longest lol)
  (cond
    [(empty? (rest lol)) (first lol)]
    [(> 
       (length (first lol))
       (length (longest (rest lol)))
       )
       (first lol)
       ]
    [else (longest (rest lol))]
    ))


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
