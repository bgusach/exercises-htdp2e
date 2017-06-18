#lang htdp/bsl+

(require test-engine/racket-tests)


(check-expect
  (list (string=? "a" "b") #false)
  (list #false #false)
  )


(check-expect
  (list (+ 10 20) (* 10 20) (/ 10 20))
  (list 30 200 1/2)
  )


(check-expect
  (list "dana" "jane" "mary" "laura")
  ; ...??
  (list "dana" "jane" "mary" "laura")
  )


(test)
