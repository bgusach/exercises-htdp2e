#lang htdp/isl+

(require test-engine/racket-tests)
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

     ; N N -> [Either Inex Error]
     ; Given a mantissa and a signed exponent, possibly any of them
     ; out of the 0-99 range, it generates a valid Inex if possible.
     (define (norm mant sign-ex)
      (cond
        [(> mant MAX-M)
         (norm 
           (inexact->exact (round (/ mant 10)))
           (add1 sign-ex)
           )]

        [(> (abs sign-ex) MAX-E) (error "exponent out of range")]

        [else
          (make-inex
            mant
            (get-sign sign-ex)
            (abs sign-ex)
            )])))

    ; -- IN --
    (cond
      [(> exp-diff 1) (error "exponents differ by more than 1")]

      [(zero? exp-diff) (norm (+ mant-a mant-b) sign-exp-a)]

      [(< sign-exp-a sign-exp-b)
       (norm (+ mant-a (* mant-b 10)) sign-exp-a)
       ]

      [(> sign-exp-a sign-exp-b)
       (norm (+ (* mant-a 10) mant-b) sign-exp-b)
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


; =================== End of exercise ==================


(test)

