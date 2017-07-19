#lang htdp/isl

(require test-engine/racket-tests)
    
(check-expect
  ((local 
     ((define (f x)
        (+ (* 4 (sqr x)) 3)
        ))

     ; -- IN --
     f
     )
   1
   )
  7
  )

(test)

