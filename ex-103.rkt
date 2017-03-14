#lang htdp/bsl

(require test-engine/racket-tests)


; Spider
; A Spider is a structure: (make-spider legs space)
; interpretation: a spider with `legs` legs, and taking `space` cubic metres
(define-struct spider [legs space])

; Elephant
; An Elephant is a structure: (make-elephant space)
; interpretation: an elephant taking a volume of `space`
(define-struct elephant [space])

; Boa
; A Boa is a structure: (make-boa length girth)
; interpretation: a boa with a length of `length` and a girth of `girth`
(define-struct boa [length girth])

; Armadillo
; An Armadillo is a structure: (make-armadillo weight space)
; interpretation: an armadillo of `weight` kilos
(define-struct armadillo [weight space])

; Animal is one of
; - Spider
; - Elephant
; - Boa
; - Armadillo

; Animal -> Boolean
; Returns whether an animal fits in a cage
(check-expect (fits? (make-spider 8 10) 10) #true)
(check-expect (fits? (make-elephant 1000) 10) #false)
(check-expect (fits? (make-boa 2 0.5) 100) #true)
(check-expect (fits? (make-armadillo 100 50) 20) #false)
(define (fits? animal cage-vol)
  (>=
    cage-vol
    (cond
      [(spider? animal) (spider-space animal)]
      [(elephant? animal) (elephant-space animal)]
      [(boa? animal) (* (boa-length animal) (boa-girth animal))]
      [(armadillo? animal) (armadillo-space animal)])))

(test)
