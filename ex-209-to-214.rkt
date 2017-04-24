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
(check-member-of (alternative-words "cat") (list "act" "cat") (list "cat" "act"))
(check-member-of (alternative-words "dog") (list "dog" "god") (list "god" "dog"))
(check-satisfied (alternative-words "rat") all-words-from-rat?)
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
; NOTE: Load the whole dict here if you want real results
(define 
  DICT 
  (list "art" "act" "brother" "cat" "dog" "god" "rat" "tar")
  )  


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
; Calculates all the letter-permutations of w
(check-satisfied (arrangements (list "d" "e")) all-de-permutations?)
(check-satisfied (arrangements (list "d" "o" "g")) all-dog-permutations?)
(check-expect (arrangements '()) (list '()))
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else
      (insert-everywhere/in-all-words
        (first w)
        (arrangements (rest w))
        )]))


; Test function for arrangements
(define (all-de-permutations? low)
  (and
    (= (length low) 2)
    (member (list "e" "d") low)
    (member (list "d" "e") low)
    ))

; Test function for arrangements
(define (all-dog-permutations? low)
  (and
    (= (length low) 6)
    (member (list "d" "o" "g") low)
    (member (list "o" "d" "g") low)
    (member (list "o" "g" "d") low)
    (member (list "g" "o" "d") low)
    (member (list "g" "d" "o") low)
    (member (list "d" "g" "o") low)
    ))

; =================== End of exercise ==================




; ==================== Exercise 213 ====================
; ### Constants
(define empty-word '())

; ### Functions
; 1-String List-of-words -> List-of-words
; Inserts the letter into all positions of all words
(check-expect 
  (insert-everywhere/in-all-words "a" (list empty-word))
  (list (string->word "a"))
  )
(check-expect
  (insert-everywhere/in-all-words "a" (list (list "b" "c") (list "h" "o")))
  (list
    (list "a" "b" "c") (list "b" "a" "c") (list "b" "c" "a")
    (list "a" "h" "o") (list "h" "a" "o") (list "h" "o" "a")
    ))
(define (insert-everywhere/in-all-words letter low)
  (cond
    [(empty? low) '()]
    [else 
      (append
       (insert-everywhere letter (first low))
       (insert-everywhere/in-all-words letter (rest low))
       )]))


; 1-String Word -> List-of-words
; Returns a list of words where the letter has been inserted in all
; possible positions of the word
(check-expect (insert-everywhere "a" empty-word) (list (list "a")))
(check-expect 
  (insert-everywhere "a" (string->word "b"))
  (list 
    (string->word "ab")
    (string->word "ba")
    ))
(check-expect 
  (insert-everywhere "a" (string->word "bc"))
  (list
    (string->word "abc")
    (string->word "bac")
    (string->word "bca")
    ))
(define (insert-everywhere letter word)
  (distribute empty-word letter word)
  )


; List-of-any Any List-of-any -> List-of-list-of-any
; Helper function to insert element in all the positions
; of acc-post, "prefixed" with acc-pre.
(define (distribute acc-pre element acc-post)
  (cond 
    [(empty? acc-post) 
     (list (append acc-pre (list element)))
     ]

    [else
      (append
        (list (append acc-pre (list element) acc-post))
        (distribute
          (append acc-pre (list (first acc-post)))
          element
          (rest acc-post))
        )]))

; =================== End of exercise ==================


(test)

