#lang htdp/bsl

(require test-engine/racket-tests)


(check-expect (missile-or-not? #true) #false)
(check-expect (missile-or-not? #false) #true)
(check-expect (missile-or-not? (make-posn 0 0)) #true)
(check-expect (missile-or-not? "lol") #false)

(define (missile-or-not? v)
  (or
    (false? v)
    (posn? v)
    ))


(test)

