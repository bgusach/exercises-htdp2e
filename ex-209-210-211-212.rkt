#lang htdp/bsl+

(require test-engine/racket-tests)

; ==================== Exercise 209 ====================
; ### Functions
; String -> Boolean
; Function for testing purposes
(define (all-words-from-rat? w)
  (and 
    (member? "rat" w) 
    (member? "art" w) 
    (member? "tar" w)
    ))
 

; String -> List-of-strings
; find all words that the letters of some given word spell
; (check-member-of (alternative-words "cat") (list "act" "cat") (list "cat" "act"))
; (check-satisfied (alternative-words "rat") all-words-from-rat?)
(define (alternative-words s)
  (in-dictionary
    (words->strings 
      (arrangements 
        (string->word s)
        ))))



; String -> Word
; Turns the string s into a Word
(check-expect (string->word "hola") (list "h" "o" "l" "a"))
(define (string->word s)
  (explode s)
  )


; Word -> String
; Turns the Word w into a String
(check-expect (word->string (list "h" "o" "l" "a")) "hola")
(define (word->string w)
  (implode w)
  )

; =================== End of exercise ==================




; ==================== Exercise 210 ====================
; ### Functions
; List-of-strings -> List-of-words
; Turns Strings into Words
(check-expect 
  (strings->words (list "hey" "yo"))
  (list (list "h" "e" "y") (list "y" "o"))
  )
(define (strings->words los)
  (cond
    [(empty? los) '()]
    [else
      (cons
        (string->word (first los))
        (strings->words (rest los))
        )]))


; List-of-words -> List-of-strings
; Turn all Words in low into Strings 
(check-expect 
  (words->strings (list (list "h" "e" "y") (list "y" "o")))
  (list "hey" "yo")
  )
(define (words->strings low) 
  (cond 
    [(empty? low) '()]
    [else 
      (cons 
        (implode (first low))
        (words->strings (rest low))
        )]))

; =================== End of exercise ==================



; ==================== Exercise 211 ====================
; ### Constants
(define 
  DICT 
  (list "art" "act" "brother" "cat" "dog" "god" "rat" "tar")
  )  ; Ignorance is bliss =)


; ### Functions
; List-of-strings -> List-of-strings
(check-expect (in-dictionary (list "hello" "cat")) (list "cat"))
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(member (first los) DICT) (cons (first los) (in-dictionary (rest los)))]
    [else (in-dictionary (rest los))]
    ))

; =================== End of exercise ==================




; ==================== Exercise 212 ====================
; ### Data Definitions
; A Word is one of:
; - '()
; - (const 1-String Word)
;
; For example:
(define word1 (list "c" "a" "t"))
(define word2 (list "d" "o" "g"))
 

; A List-of-words is one of:
; - '()
; - (Word List-of-words)
;
; For example:
(define low (list word1 word2))


; ### Functions
; Word -> List-of-words
(check-satisfied (arrangements (list "d" "e")) all-de-permutations?)
(define (arrangements w)
  '()
  )


(define (all-de-permutations? low)
  (and
    (= (length low) 2)
    (member (list "e" "d") low)
    (member (list "d" "e") low)
    ))

; =================== End of exercise ==================

(test)

