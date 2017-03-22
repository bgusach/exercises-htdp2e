#lang htdp/bsl

; ### Constants

(define ARTICLES (cons "a" (cons "an" (cons "the" '()))))
(define line0 (cons "I" (cons "am" (cons "a" (cons "horse" '())))))
(define line0-no-art (cons "I" (cons "am" (cons "horse" '()))))
(define line1 (cons "the" (cons "wise" (cons "man" '()))))
(define line1-no-art (cons "wise" (cons "man" '())))

; ### Data Definitions

; LLS (List-of-list-of-strings) is one of:
; - '()
; - (cons List-of-strings LLS)

; ### Functions

; LLS -> LLS
; Removes all articles from all lines
(check-expect (remove-articles '()) '())
(check-expect 
  (remove-articles (cons line0 (cons line1 '())))
  (cons line0-no-art (cons line1-no-art '()))
  )

(define (remove-articles lls)
  (cond
    [(empty? lls) '()]
    [else 
      (cons
        (remove-line-articles (first lls))
        (remove-articles (rest lls))
        )]))


; List-of-string -> List-of-string
; Removes all articles from line
(check-expect (remove-line-articles '()) '())
(check-expect (remove-line-articles line0) line0-no-art)

(define (remove-line-articles los)
  (cond
    [(empty? los) '()]
    [(article? (first los)) (remove-line-articles (rest los))]
    [else (cons (first los) (remove-line-articles (rest los)))]
    ))


; String -> Boolean
; Checks whether a given string is an article
(define (article? s)
  (member? s ARTICLES)
  )


(require test-engine/racket-tests)
(test)
