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
; – 1
; – -1
; An N99 is an N between 0 and 99 (inclusive).


; ### Functions

; N Number N -> Inex
; make an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond
    [(and 
       (<= 0 m 99) (<= 0 e 99) 
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

; Inex Inex -> Inex
; Adds two numbers (of which exponents don't differ 
; more than 1)
(check-expect 
  (inex+ (create-inex 10 1 1) (create-inex 10 1 1))
  (create-inex 20 1 1)
  )
(check-expect 
  (inex+ (create-inex 80 1 1) (create-inex 30 1 1))
  (create-inex 11 1 2)
  )
(define (inex+ a b)
  (local
    ((define m (+ (inex-mantissa a) (inex-mantissa b)))
     (define m-overflow (> m 99))
     (define s (inex-sign a))
     (define e (if m-overflow (add1 ) (inex-exponent a))
     )

    ; -- IN --
    (cond
      [(< m 99) (make-inex m s e)]
      [(< exp-a) ]
      )
    (if
      (> m 99)
      (make-inex (inexact->exact (/ m 10)) s (add1 e))
      (make-inex m s e)
      )))

; =================== End of exercise ==================

(test)

