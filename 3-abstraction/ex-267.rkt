#lang htdp/isl

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
    (map dollar->euro lo$)
    )

; Number -> Number
; Converts an amount of dollars to an amount of euros NOTE: I think the
; exercise wanted that this is a local defined within convert-to-euro but
; it makes more sense if it is at top-level
(check-expect (dollar->euro 1) $->€)
(define (dollar->euro amount)
   (* amount $->€)
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
  (map fahrenheit->celsius lof)
  )

; Number -> Number
; Converts a Fahrenheit temperature into a Celsius temp.
(define (fahrenheit->celsius fahr)
  (* (- fahr 32) 5/9)
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
  (local
    (; Posn -> [List-of Number]
     ; Returns a 2-item list with the x and y coordinates of the posn
     (define (posn->pair pos)
       (list (posn-x pos) (posn-y pos))
       ))

    ; -- IN --
    (map posn->pair lop)
    ))


(test)

