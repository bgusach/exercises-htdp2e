#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

; A [Matrix-of X] is a [List-of [List-of X]]

; ### Functions

; N -> [Matrix-of N]
; Returns an identity matrix n x n
(check-expect (identityM 1) '((1)))
(check-expect (identityM 2) '((1 0) (0 1)))
(check-expect 
  (identityM 5) 
  '((1 0 0 0 0)
    (0 1 0 0 0)
    (0 0 1 0 0)
    (0 0 0 1 0)
    (0 0 0 0 1)
    ))
(define (identityM n)
  (local 
    (; N -> [List-of N]
     ; Makes a row of zeros with a 1 in position m
     (define (make-row m)
       (append 
         (make-list m 0)
         '(1)
         (make-list (- n m 1) 0)
         )))

    ; -- IN --
    (build-list n make-row)
    ))

(test)

