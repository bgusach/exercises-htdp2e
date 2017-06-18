#lang htdp/isl+

(require test-engine/racket-tests)

; ### Data Definitions

; Natural -> [List-of Natural]
; Returns a list 0, 1, ... (- n 1)
(check-expect (build-series-1 0) '())
(check-expect (build-series-1 3) '(0 1 2))
(check-expect (build-series-1 5) '(0 1 2 3 4))
(define (build-series-1 n)
  (build-list n (λ (n) n))
  )


; Natural -> [List-of Natural]
; Returns a list 1, ... n
(check-expect (build-series-2 0) '())
(check-expect (build-series-2 3) '(1 2 3))
(check-expect (build-series-2 5) '(1 2 3 4 5))
(define (build-series-2 n)
  (build-list n (λ (n) (+ n 1)))
  )


; Natural -> [List-of Number]
; Returns a list 1 1/2 1/3 ... 1/n
(check-expect (build-series-3 0) '())
(check-expect (build-series-3 3) '(1 1/2 1/3))
(check-expect (build-series-3 5) '(1 1/2 1/3 1/4 1/5))
(define (build-series-3 n)
  (build-list n (λ (n) (/ 1 (+ n 1))))
  )


; Natural -> [List-of Number]
; Returns a list with the n first even numbers
(check-expect (build-series-4 0) '())
(check-expect (build-series-4 3) '(0 2 4))
(check-expect (build-series-4 5) '(0 2 4 6 8))
(define (build-series-4 n)
  (build-list n (λ (n) (* n 2)))
  )


; NOTE: the diagonal square exercise was already
; solved in ex-262.rkt with build-list.

; NOTE: tabulate was not defined with a local function
; and I guess using lambda makes no sense here...

(test)

