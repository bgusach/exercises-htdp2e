#lang htdp/bsl

; ### Constants

; ### Data Definitions

(define-struct phone [area switch four])
; Phone is a structure (make-phone Three Three Four)
; Three is a 3-digit Number
; Four is a 4-digit Number

; List-of-phones is one of:
; - '()
; - (cons (make-phone a s f) List-of-phones)

; ### Functions
; List-of-phones -> List-of-phones
; Replaces the area code from 713 to 281 from all the passed Phones
(check-expect (replace '()) '())
(check-expect 
  (replace (cons (make-phone 100 100 1000) (cons (make-phone 713 999 9999) '())))
  (cons (make-phone 100 100 1000) (cons (make-phone 281 999 9999) '()))
  )
(define (replace lop)
  (cond 
    [(empty? lop) '()]
    [else 
      (cons
        (if 
          ; NOTE: another good option is to have area-to-281 make this check
          (= (phone-area (first lop)) 713)
          (area-to-281 (first lop))
          (first lop)
          )
        (replace (rest lop))
        )]))


; Phone -> Phone
; Returns a Phone with the area changed to 281
(check-expect (area-to-281 (make-phone 999 999 9999)) (make-phone 281 999 9999))
(define (area-to-281 p)
  (make-phone
    281
    (phone-switch p)
    (phone-four p)
    ))



(require test-engine/racket-tests)
(test)


