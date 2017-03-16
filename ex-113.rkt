#lang htdp/bsl

; NOTE: this code has not been tested


; Any -> Boolean
; Predicate for SIGS values
(define (sigs? v)
  (or (aim? v) (fired? v))
  )


; Any -> Boolean
(define (coordinate? v)
  (or (number? v) (posn? v))
  )


; Any -> Boolean
; Predicate for VAnimal values
(define (v-animal? v)
  (or (cat? v) (cham? v))
  )


