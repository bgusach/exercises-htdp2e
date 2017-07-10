#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require racket/string)

; ### Data Definitions

(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999. 


; ### Functions

; [List-of Phone] -> [List-of Phone]
; Replaces the area code 713 with 281
(check-expect 
  (replace (list (make-phone 1 1 1) (make-phone 713 1 1)))
  (list (make-phone 1 1 1) (make-phone 281 1 1))
  )
(define (replace lop)
  (map
    (λ (p)
       (match p
         [(phone 713 switch four) (make-phone 281 switch four)]
         [any-other any-other]
         ))
    lop
    ))


(test)

