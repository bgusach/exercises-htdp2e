#lang htdp/bsl+

(require test-engine/racket-tests)

; ### Data Definitions
; Song is a String

; LOS is one of:
; - '()
; - (cons Song LOS)

; ### Constants
(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"
    ))

; ### Functions
; List-of-songs -> List-of-rank-songs
; Adds ranks to a list of songs. First song gets rank #1
(define (ranking los)
  (reverse (add-ranks (reverse los)))
  )
 
; List-of-songs -> List-of-rank-songs
; Adds ranks to a list of songs. Last songs gets rank #1
(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else 
      (cons 
        (list 
          ; NOTE: original exercise does not convert to string
          (number->string (length los))
          (first los))
        (add-ranks (rest los))
        )]))

(define (make-ranking lors)
  `(table ,@(make-rows lors))
  )

; List-of-ranking-songs -> List-of-trs
(check-expect
  (make-rows
    '(("1" "Nested Coil: I like when you talk to that potato")
      ("2" "Abba: Kill 'em with fire")
      ))
  '((tr (td "1") (td "Nested Coil: I like when you talk to that potato"))
    (tr (td "2") (td "Abba: Kill 'em with fire"))
    ))
(check-expect
  (make-rows (ranking one-list))
  '((tr (td "1") (td "Asia: Heat of the Moment"))
    (tr (td "2") (td "U2: One"))
    (tr (td "3") (td "The White Stripes: Seven Nation Army"))
    ))
(define (make-rows lors)
  (cond
    [(empty? lors) '()]
    [else
      (cons
        `(tr ,@(make-cells (first lors)))
        ; or just:
        ; (cons 'tr (make-cells (first lors)))
        (make-rows (rest lors))
        )]))


; List-of-strings -> List-of-tds
(check-expect 
  (make-cells '("2" "Johnny: the lazy bird"))
  '((td "2") (td "Johnny: the lazy bird"))
  )
(define (make-cells los)
  (cond
    [(empty? los) '()]
    [else
      (cons
        (list 'td (first los))
        (make-cells (rest los))
        )]))

(test)

