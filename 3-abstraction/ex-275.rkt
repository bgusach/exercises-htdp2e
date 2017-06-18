#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants
(define 
  DICT
  '("albeit"
    "arrow"
    "boycott"
    "buffalo"
    "coast"
    "california"
    "dice"
    "doritos"
    "doughnut"
    "penis"
    "zebra"
    ))

; ### Data Definitions

; A Letter is a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz")
  )

; A Word is a String

; A Dictionary is a [List-of Word]

; A Letter-Count is a structure
; (make-lc letter count)
(define-struct lc [letter count])


; ### Functions

; Dictionary -> Letter-Count
; Returns a letter count for the most frequently used first 
; letter in the dict
(check-expect (most-frequent DICT) (make-lc "d" 3))
(define (most-frequent dict)
  (local
    (; Letter -> Letter-Count
     (define (letter-count letter)
       (get-letter-count dict letter)
       ))

    ; -- IN --
    (argmax lc-count (map letter-count LETTERS))
    ))


; Word Letter -> Boolean
; Checks whether word starts with letter
(check-expect (starts-with-letter? "lol" "l") #true)
(check-expect (starts-with-letter? "troll" "l") #false)
(define (starts-with-letter? word letter)
  (string=? (string-ith word 0) letter)
  )


; Dictionary Letter -> Letter-Count
; Returns a Letter-Count for the passed letter and dict
(check-expect (get-letter-count DICT "d") (make-lc "d" 3))
(define (get-letter-count dict letter)
  (local
    (; Word Number -> Number
     (define (reduce word acc)
       (if
         (starts-with-letter? word letter)
         (add1 acc)
         acc
         )))

    ; -- IN -- 
    (make-lc
      letter
      (foldl reduce 0 dict)
      )))


; Dictionary -> [List-of Dictionary]
(check-expect 
  (words-by-first-letter '("aeroplane" "australia" "bacon"))
  '(("aeroplane" "australia")
    ("bacon")
    ))
(define (words-by-first-letter dict)
  (local
    (; Letter -> Dictionary
     ; Collects words starting with letter
     (define (subdict-from-letter letter)
       (local
         (; Word -> Boolean
          ; Returns whether word starts with letter
          (define (starts-with-this-letter? word)
            (starts-with-letter? word letter)
            ))

         ; -- IN -- 
         (filter starts-with-this-letter? dict)
         )))

    ; -- IN -- 
    (filter not-empty? (map subdict-from-letter LETTERS))
    ))


; [A] [List-of A] -> Boolean
; Returns whether the list is **not** empty
(define (not-empty? l)
  (not (empty? l))
  )


(test)

