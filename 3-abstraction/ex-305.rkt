#lang htdp/isl

(require test-engine/racket-tests)
(require 2htdp/abstraction)

; ### Constants
(define $->€ 1.22)


; ### Functions
; [List-of Number] -> [List-of Number]
; Converts a list of dollars into a list of euros
(check-within
  (convert-to-euro '(0 1 5))
  (list 0 $->€ (* 5 $->€))
  0.01
  )
(define (convert-to-euro lo$)
  (for/list ([it lo$]) (* it $->€))
  )

(test)

