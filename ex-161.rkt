#lang htdp/bsl

; ### Constants
(define HOUR-RATE 14)


; ### Data Definitions
; List-of-numbers is one of:
; - '()
; - (cons Number List-of-numbers)


; ### Functions
; Number -> Number
; computes the wage for h hours of work
(check-member-of (wage 10) (* HOUR-RATE 10))
(define (wage h)
  (* HOUR-RATE  h)
  )


; List-of-numbers -> List-of-numbers
; computes a list of wages 
(check-member-of (wage* '()) '())
(check-member-of (wage* (cons 2 '())) (cons (* 2 HOUR-RATE) '()))
(define (wage* loh)
  (cond
    [(empty? loh) '()]
    [else 
      (cons
        (wage (first loh))
        (wage* (rest loh))
        )]))


(require test-engine/racket-tests)
(test)

