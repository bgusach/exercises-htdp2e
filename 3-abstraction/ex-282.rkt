#lang htdp/isl+

(require test-engine/racket-tests)

; ### Functions


; Number -> Number
(define (f-plain x)
  (* 10 x)
  )


; Number -> Number
(define f-lambda
  (lambda (x) (* 10 x))
  )


; Number -> Boolean
(define (compare x)
  (= (f-plain x) (f-lambda x))
  )

(check-expect
  (compare (random 100000))
  #true
  )

(test)
