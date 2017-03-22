#lang htdp/bsl

; ### Constants


; ### Data Definitions
(define-struct work [employee rate hours])
; A (piece of) Work is a structure (make-work String Number Number)
; Interpretation: (make-work n r h) combines the name n, the pay rate r
; and hours h

(define-struct paycheck [employee amount])
; A Paycheck is a structure (make-paycheck String Number)
; Interpretation: (make-paycheck n a) combines one employee with the
; amount of money he has got to be paid


; A Low (list of works) is one of:
; - '()
; - (cons Work Low)


; ### Functions
; Number Number -> Number
; computes the wage for h hours of work and rate r
(check-expect (wage 10 10) 100)
(check-error (wage 101 0))
(define (wage h r)
  (if 
    (> h 100)
    (error "wage: amount of hours greater than 100")
    (* r  h)
    ))


; Work -> Number
; Calculates the wage for a Work structure w
(check-expect (work-wage (make-work "lol" 20 5)) 100)
(define (work-wage w)
  (*
    (work-hours w)
    (work-rate w)
    ))


; Low -> List-of-numbers
; Computes a list of wages for the given Work records
(check-expect (wage*.v2 '()) '())
(check-expect
  (wage*.v2 
    (cons (make-work "friedrich" 20 70)
      (cons (make-work "john" 10 10) 
        '())))
  (cons 1400 (cons 100 '()))
  )
(define (wage*.v2 low)
  (cond
    [(empty? low) '()]
    [else 
      (cons
        (work-wage (first low))
        (wage*.v2 (rest low))
        )]))


; Low -> List-of-paychecks
; Computes a list of wages for the given Work records
(check-expect (wage*.v3 '()) '())
(check-expect
  (wage*.v3 
    (cons (make-work "friedrich" 20 70)
      (cons (make-work "john" 10 10) 
        '())))
  (cons
    (make-paycheck "friedrich" 1400)
    (cons 
      (make-paycheck "john" 100)
      '()
      )))
(define (wage*.v3 low)
  (cond
    [(empty? low) '()]
    [else 
      (cons
        (paycheck-from-work (first low))
        (wage*.v3 (rest low))
        )]))


; Work -> Paycheck
; Computes a paycheck from a work record
(check-expect (paycheck-from-work (make-work "john" 10 50)) (make-paycheck "john" 500))

(define (paycheck-from-work w)
  (make-paycheck
    (work-employee w)
    (* 
      (work-hours w)
      (work-rate w)
    )))


(require test-engine/racket-tests)
(test)

