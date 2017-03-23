#lang htdp/bsl

(require 2htdp/batch-io)

; ### Functions

; String -> nothing...
; Encodes the file n and prints it to stdout
(define (encode-file n)
  (write-file 
    'stdout
    (join-strings 
      (encode-lines (read-lines n))
      "\n"
      )))


; List-of-string -> List-of-string
; Encodes each of the strings of a list
(define (encode-lines los)
  (cond
    [(empty? los) '()]
    [else
      (cons
        (encode-string (first los))
        (encode-lines (rest los))
        )]))


; String -> String
; Encodes a string
(define (encode-string s)
  (join-strings (encode-letters (explode s)) "")
  )


; List-of-strings String -> String
; joins a list of strings with the string s
(define (join-strings los s)
  (cond
    [(empty? los) ""]
    [(empty? (rest los)) (first los)]
    [else
      (string-append
        (first los)
        s
        (join-strings (rest los) s)
        )]))


; List-of-1-strings -> List-of-strings
; Encodes each of the 1-Strings of a list
(check-expect 
  (encode-letters (explode "he")) 
  (cons (encode-letter "h") (cons (encode-letter "e") '()))
  )
(define (encode-letters lol)
  (cond
    [(empty? lol) '()]
    [else
      (cons
        (encode-letter (first lol))
        (encode-letters (rest lol))
        )]))


; 1String -> String
; converts the given 1String to a 3-letter numeric String
(check-expect (encode-letter "z") (code1 "z"))
(check-expect (encode-letter "\t") (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a") (string-append "0" (code1 "a")))
(define (encode-letter s)
  (cond
    [(>= (string->int s) 100) (code1 s)]
    [(< (string->int s) 10)
     (string-append "00" (code1 s))]
    [(< (string->int s) 100)
     (string-append "0" (code1 s))]))
 

; 1String -> String
; convert the given 1String into a String
(check-expect (code1 "z") "122")
(define (code1 c)
  (number->string (string->int c)))


(require test-engine/racket-tests)
(test)

(encode-file "ttt.txt")
