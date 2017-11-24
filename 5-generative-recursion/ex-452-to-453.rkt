#lang htdp/isl+

(require test-engine/racket-tests)


; ### Constants

; ### Data Definitions

; A File is one of: 
; – '()
; – (cons "\n" File)
; – (cons 1String File)
; Interpretation: represents the content of a file 
; "\n" is the newline character 

; A Line is a [List-of 1String].

; ### Functions

; File -> [List-of Line]
(check-expect 
  (file->list-of-lines (explode "abc\nde\nfgh\n"))
  (list
    (explode "abc")
    (explode "de")
    (explode "fgh")
    ))
(define (file->list-of-lines afile)
  (cond
    [(empty? afile) '()]
    [else
      (cons 
        (first-line afile)
        (file->list-of-lines (remove-first-line afile))
        )]))


; ==================== Exercise 452 ====================

; File -> Line
; Given a file, it returns the first line, i.e. the
; [List-of 1String] until the first line break, not including it.
(check-expect 
  (first-line (explode "abc\ndef")) 
  (explode "abc")
  )
(define (first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) "\n") '()]
    [else 
      (cons 
        (first afile) 
        (first-line (rest afile))
        )]))


; File -> File
; Given a file, it drops the first line and returns the 
; remaining file. Line break is dropped as well.
(check-expect 
  (remove-first-line (explode "abc\ndef")) 
  (explode "def")
  )
(define (remove-first-line afile)
  (cond
    [(empty? afile) '()]
    [(string=? (first afile) "\n") (rest afile)]
    [else (remove-first-line (rest afile))]
    ))

; =================== End of exercise ==================




; ==================== Exercise 453 ====================


; Line -> [List-of Token]
; Converts a line into a list of tokens or "words"
(check-expect
  (tokenize (explode "say hello to my little friend!"))
  '("say" "hello" "to" "my" "little" "friend!")
  )
(define (tokenize line)
  (cond
    [(empty? line) '()]
    [else
      (cons
        (get-first-token line)
        (tokenize (drop-first-token line))
        )]))


; Line -> Token
; Returns the first token of the line
(check-expect
  (get-first-token (explode "hello amigo"))
  "hello"
  )
(check-expect
  (get-first-token (explode "hello123 amigo"))
  "hello123"
  )
(define (get-first-token line)
  (local
    ((define (get-first-token-1-strings line)
       (cond
         [(empty? line) '()]
         [(string-whitespace? (first line)) '()]
         [else
           (cons 
             (first line)
             (get-first-token-1-strings (rest line))
             )])))

    ; -- IN --
    (implode (get-first-token-1-strings line))
    ))


; Line -> Line
; Drops the first token from the line and returns the rest
(check-expect
  (drop-first-token (explode "hello amigo"))
  (explode "amigo")
  )
(check-expect
  (drop-first-token (explode "hello123 amigo"))
  (explode "amigo")
  )
(define (drop-first-token line)
  (cond
    [(empty? line) '()]
    [(string-whitespace? (first line)) (rest line)]
    [else (drop-first-token (rest line))]
    ))
; =================== End of exercise ==================

(test)

