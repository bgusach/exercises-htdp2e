#lang htdp/isl+

(require test-engine/racket-tests)

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
  (map (lambda (x) (* x $->€)) lo$)
  )

; [List-of Number] -> [List-of Number]
; Converts a list of Fahrenheit into a list of Celsius NOTE: here again,
; fahrenheit->celsius seems useful outside of the context of this
; function, so I place it at top-level
(check-within
  (convertFC '(-50 0 50))
  '(-45.56 -17.78 10.00)
  0.1
  )
(define (convertFC lof)
  (map (lambda (fahr) (* (- fahr 32) 5/9)) lof)
  )


; [List-of Posn] -> [[List-of Number]]
; Converts a list of Posn into a list of pairs of numbers
; NOTE: here the helper seems pretty specific, so it stays within
; translate lop
(check-expect
  (translate (list (make-posn 1 2) (make-posn 10 12)))
  '((1 2) (10 12))
  )
(define (translate lop)
  (map 
    (lambda (pos) (list (posn-x pos) (posn-y pos))) 
    lop
    ))


(test)

