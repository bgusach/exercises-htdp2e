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


; Dollar Number -> Euro
; converts the amount of dollar v given an exchange rate e
(check-member-of ($-to-€* 2 RATIO-$-to-€) ($-to-€ 2))

(define ($-to-€* d e)
  (* d e))


; List-of-Dollar ExchangeRate -> List-of-euro
; converts a list of dollars into a list of euros according to the passed rate
(check-member-of (convert-euro* '() RATIO-$-to-€) '())
(check-member-of (convert-euro* (cons 2 '()) RATIO-$-to-€) (cons ($-to-€ 2) '()))

(define (convert-euro* lod rate)
  (cond
    [(empty? lod) '()]
    [else
      (cons
        ($-to-€* (first lod) rate)
        (convert-euro* (rest lod) rate)
        )]))

(require test-engine/racket-tests)
(test)

