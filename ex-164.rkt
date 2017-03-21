#lang htdp/bsl

; ### Constants
(define RATIO-$-to-€ 0.92)

; ### Data Definitions
; Euro is a Number
; Dollar is a Number


; ### Functions
; Dollar -> Euro
; converts d dollars into euros
(define ($-to-€ d)
  (* d RATIO-$-to-€)
  )

; List-of-dollar -> List-of-euro
; converts dollars to euros
(check-member-of (convert-euro '()) '())
(check-member-of (convert-euro (cons 2 '())) (cons ($-to-€ 2) '()))

(define (convert-euro lod)
  (cond
    [(empty? lod) '()]
    [else
      (cons
        ($-to-€ (first lod))
        (convert-euro (rest lod))
        )]))


; Number Number -> Euro
; converts the amount of money v given an exchange rate e
(check-member-of (any-to-€ 2 RATIO-$-to-€) ($-to-€ 2))

(define (any-to-€ v e)
  (* v e))


; List-of-any-currency ExchangeRate -> List-of-euro
; converts any currency to euros according to the passed rate
(check-member-of (convert-euro* '() RATIO-$-to-€) '())
(check-member-of (convert-euro* (cons 2 '()) RATIO-$-to-€) (cons ($-to-€ 2) '()))

(define (convert-euro* loc rate)
  (cond
    [(empty? loc) '()]
    [else
      (cons
        (any-to-€ (first loc) rate)
        (convert-euro* (rest loc) rate)
        )]))

(require test-engine/racket-tests)
(test)

