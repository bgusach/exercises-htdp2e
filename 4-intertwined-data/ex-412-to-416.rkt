#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
; (require racket/list)
; (require racket/string)
; (require racket/base)


; ### Data Definitions

(define-struct inex [mantissa sign exponent])
; An Inex is a structure: 
;   (make-inex N99 S N99)

; An S is one of:
; - 1
; - -1

; An N99 is an N between 0 and 99 (inclusive).

(define MAX-M 99)
(define MAX-E MAX-M)


; ### Functions


; N Number N -> Inex
; make an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and 
       (<= 0 m MAX-M) (<= 0 e MAX-E) 
       (or (= s 1) (= s -1))
       )
     (make-inex m s e)
     ]
    [else (error "bad values given")]
    ))


; Inex -> Number
; convert an inex into its numeric equivalent 
(check-within (inex->number (create-inex 2 1 2)) 200 0.001)
(define (inex->number an-inex)
  (* 
    (inex-mantissa an-inex)
    (expt
       10 
       (* 
         (inex-sign an-inex) 
         (inex-exponent an-inex)
         ))))

; ==================== Exercise 412 ====================

; Inex Inex -> [Either Inex Error]
; Adds two numbers of which exponents don't differ more than 1. 
(check-expect 
  (inex+ (create-inex 10 1 1) (create-inex 10 1 1))
  (create-inex 20 1 1)
  )
(check-expect 
  (inex+ (create-inex 80 1 1) (create-inex 30 1 1))
  (create-inex 11 1 2)
  )
(check-expect
  (inex+ (create-inex 1 1 0) (create-inex 1 1 1))
  (create-inex 11 1 0)
  )
(check-expect
  (inex+ (create-inex 1 1 0) (create-inex 1 -1 1))
  (create-inex 11 -1 1)
  )
(check-expect
  (inex+ (create-inex 9 1 90) (create-inex 9 1 89))
  (create-inex 99 1 89)
  )
(check-error 
  (inex+ (create-inex 99 1 99) (create-inex 99 1 99))
  "exponent out of range"           
  )
(define (inex+ a b)
  (local
    ((define sign-exp-a (get-signed-exp a))
     (define sign-exp-b (get-signed-exp b))
     (define exp-diff (abs (- sign-exp-a sign-exp-b)))
     (define mant-a (inex-mantissa a))
     (define mant-b (inex-mantissa b))
     )

    ; -- IN --
    (cond
      [(> exp-diff 1) (error "exponents differ by more than 1")]

      [(zero? exp-diff) 
       (norm-and-create (+ mant-a mant-b) sign-exp-a)]

      [(< sign-exp-a sign-exp-b)
       (norm-and-create (+ mant-a (* mant-b 10)) sign-exp-a)
       ]

      [(> sign-exp-a sign-exp-b)
       (norm-and-create (+ (* mant-a 10) mant-b) sign-exp-b)
       ])))


; Inex -> Number
; Extracts the signed exponent
(check-expect (get-signed-exp (make-inex 1 1 15)) 15)
(check-expect (get-signed-exp (make-inex 1 -1 3)) -3)
(define (get-signed-exp in) 
  (* (inex-sign in) (inex-exponent in))
  )


; Number -> N
; Returns -1 if num is negative, and +1 if positive or zero
(check-expect (get-sign -1) -1)
(check-expect (get-sign -14) -1)
(define (get-sign num)
  (if (>= num 0) 1 -1)
  )


; N Number -> [Either Inex Error]
; Given a mantissa and a signed exponent, possibly any of them
; out of the 0-99 range, it generates a valid Inex if possible.
(check-expect (norm-and-create 34 -3) (create-inex 34 -1 3))
(check-expect (norm-and-create 340 3) (create-inex 34 1 4))
(check-expect (norm-and-create 340 -3) (create-inex 34 -1 2))
(check-error (norm-and-create 999 99))
(define (norm-and-create mant sign-ex)
 (cond
   [(> mant MAX-M)
    (norm-and-create 
      (inexact->exact (round (/ mant 10)))
      (add1 sign-ex)
      )]

   [(> (abs sign-ex) MAX-E) (error "exponent out of range")]

   [else
     (make-inex
       mant
       (get-sign sign-ex)
       (abs sign-ex)
       )]))

; =================== End of exercise ==================




; ==================== Exercise 413 ====================

; Inex Inex -> Inex
; Multiplies two inex. Signals error on out of range
(check-expect 
  (inex* (create-inex 30 1 0) (create-inex 4 1 0))
  (create-inex 12 1 1)
  )
(check-expect 
  (inex* (create-inex 30 1 0) (create-inex 4 1 0))
  (create-inex 12 1 1)
  )
(check-expect 
  (inex* (create-inex 2 -1 1) (create-inex 3 1 0))
  (create-inex 6 -1 1)
  )
(check-error (inex* (create-inex 1 1 99) (create-inex 1 1 99)))
(define (inex* a b)
  (norm-and-create 
    (* (inex-mantissa a) (inex-mantissa b))
    (+ (get-signed-exp a) (get-signed-exp b))
    ))

; =================== End of exercise ==================




; ==================== Exercise 414 ====================

; N -> Number
; Adds n times #i1/185
(check-expect (add 0) 0)
; (check-within (add 1) 0 0.0001)  ; fail
(check-within (add 1) 0 0.01)  ; success
(check-expect (< (add 185) 1) #t)  ; almost 1, but not exactly
(define (add n)
  (cond
    [(zero? n) 0]
    [else
      (+
        #i1/185
        (add (sub1 n))
        )]))


; N -> N
; Returns how many times 1/185 can be subtracted from n
(check-expect (sub 1) 185)
(check-expect (sub 0) 0)
; Following test will loop forever, since exact zero will
; never be reached. See sub.v2 for an alternative
; (check-expect (sub #i1.0) 185)
(define (sub n)
  (cond
    [(zero? n) 0]
    [else (add1 (sub (- n 1/185)))]
    ))


; Same as `sub`, but checks for zero in a non-exact way
(check-expect (sub.v2 1) 185)
(check-expect (sub.v2 0) 0)
(check-expect (sub.v2 #i1.0) 185)
(define (sub.v2 n)
  (cond
    [(< n 0.000001) 0]
    [else (add1 (sub.v2 (- n 1/185)))]
    ))

; =================== End of exercise ==================




; ==================== Exercise 415 ====================

; Number -> N
; Finds the first integer for which (expt base n) overflows
(check-expect 
  (= (expt #i10. (find-overflow #i10.)) +inf.0)
  #f
  )
(check-expect 
  (= (expt #i10. (add1 (find-overflow #i10.))) +inf.0)
  #t
  )
(define (find-overflow base)
  (local
    (; N -> N
     (define (helper n)
       (if
         (= (expt base (add1 n)) +inf.0)
         n
         (helper (add1 n))
         )))

    ; -- IN --
    (helper 0)
    ))



; =================== End of exercise ==================




; ==================== Exercise 416 ====================

; Number -> N
; Finds the first integer for which (expt base n) underflows
(check-expect 
  (= (expt #i10. (find-underflow #i10.)) #i0.0)
  #f
  )
(check-expect 
  (= (expt #i10. (sub1 (find-underflow #i10.))) #i0.0)
  #t
  )
(define (find-underflow base)
  (local
    (; N -> N
     (define (helper n)
       (if
         (= (expt base (sub1 n)) #i0.0)
         n
         (helper (sub1 n))
         )))

    ; -- IN --
    (helper 0)
    ))



; Number [N -> N] [Number -> Boolean]
; Given a base number, an function that increases
; or decreases an integer, and a function that checks
; for under/overflow, returns the first integer 
; that finds the edge
(check-expect 
  (find-extreme #i10. add1 (λ (num) (= num +inf.0)))
  (find-overflow #i10.)
  )
(check-expect 
  (find-extreme #i10. sub1 (λ (num) (= num #i0.0)))
  (find-underflow #i10.)
  )
(define (find-extreme base bump-int is-edge?)
  (local
    ((define (helper n)
       (if 
         (is-edge? (expt base (bump-int n)))
         n
         (helper (bump-int n))
         )))

    ; -- IN --
    (helper 0)
    ))


; =================== End of exercise ==================

(test)

