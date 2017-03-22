#lang htdp/bsl

; ### Constants

(define line0 (cons "Hasta" (cons "la" '())))
(define line1 (cons "vista" (cons "baby" '())))
 
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))

; ### Data Definitions

; LLS (List-of-list-of-strings) is one of:
; - '()
; - (cons List-of-strings LLS)

; ### Functions

; LLS -> String
; Collapses a list of list of words into a single string
(check-expect (collapse lls0) "")
(check-expect (collapse lls1) "Hasta la\nvista baby")

(define (collapse lls)
  (cond
    [(empty? lls) ""]
    [(empty? (rest lls)) (join-line (first lls))]
    [else 
      (string-append
        (join-line (first lls))
        "\n"
        (collapse (rest lls))
        )]))


; List-of-strings -> String
; Joins a list of strings by a white space
(check-expect (join-line '()) "")
(check-expect (join-line line0) "Hasta la")

(define (join-line los)
  (cond
    [(empty? los) ""]
    [(empty? (rest los)) (first los)]
    [else
      (string-append
        (first los)
        " "
        (join-line (rest los))
        )]))

(require test-engine/racket-tests)
(test)
