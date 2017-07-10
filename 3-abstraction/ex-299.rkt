#lang htdp/isl+

(require test-engine/racket-tests)

; ### Data definitions

; Set is a function: [N -> Boolean]
; Interpretation: function that produces #true
; if the `div` modulo returns `rest`
(check-expect (even-numbers 6) #true)
(check-expect (even-numbers 5) #false)
(define (mk-set div rest)
  (λ (ed) 
    (= (modulo ed div) rest)
    ))

; FiniteSet is a function: [N -> Boolean]
; Interpretation: function that produces #true
; if `ed` is contained in the initial finite set
(check-expect ((mk-fin-set '(1 2 3)) 3) #true)
(check-expect ((mk-fin-set '(1 2 3)) 4) #false)
(define (mk-fin-set set)
  (λ (ed) (member ed set))
  )


; ### Constants
(define even-numbers (mk-set 2 0))
(define odd-numbers (mk-set 2 1))
(define divisible-by-10 (mk-set 10 0))
(define smaller-than-5 (mk-fin-set '(0 1 2 3 4 5)))


; ### Functions

; [N -> Boolean] N -> [N -> Boolean]
; Returns a new set with `el` added to it
(check-expect ((add-element even-numbers 7) 7) #true)
(check-expect ((add-element smaller-than-5 6) 6) #true)
(check-expect ((add-element smaller-than-5 6) 10) #false)
(define (add-element set el)
  (λ (ed) 
    (or
      (= ed el)
      (set ed)
      )))


; [N -> Boolean] [N -> Boolean] -> [N -> Boolean]
; Returns a new set with the elements of `s1` and `s2`
(check-expect ((union even-numbers odd-numbers) 13) #true)
(check-expect ((union even-numbers smaller-than-5) 1) #true)
(check-expect ((union even-numbers smaller-than-5) 11) #false)
(define (union s1 s2)
  (λ (ed) (or (s1 ed) (s2 ed)))
  )


(test)

