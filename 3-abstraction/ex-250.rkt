#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

; ### Functions

; Number -> [List-of Number]
; tabulates sin between n 
; and 0 (incl.) in a list
(check-within (tab-sin 10) (map sin '(10 9 8 7 6 5 4 3 2 1 0)) 0.001)
(define (tab-sin n)
  (cond
    [(= n 0) (list (sin 0))]
    [else
     (cons
      (sin n)
      (tab-sin (sub1 n)))]))
	
	
; Number -> [List-of Number]
; tabulates sqrt between n 
; and 0 (incl.) in a list
(check-within (tab-sqrt 10) (map sqrt '(10 9 8 7 6 5 4 3 2 1 0)) 0.001)
(define (tab-sqrt n)
  (cond
    [(= n 0) (list (sqrt 0))]
    [else
     (cons
      (sqrt n)
      (tab-sqrt (sub1 n)))]))
  

; [Number -> Any] Number -> [List-of Any]
; Applies fn to each value in [n, 0]
(define (tabulate fn n)
  (cond
    [(negative? n) '()]  ; Edge case -> (= n -1)
    [else
      (cons
        (fn n)
        (tabulate fn (sub1 n))
        )]))


(check-within (tab-sqrt2 10) (map sqrt '(10 9 8 7 6 5 4 3 2 1 0)) 0.001)
(define (tab-sqrt2 n)
  (tabulate sqrt n)
  )


(check-within (tab-sin2 10) (map sin '(10 9 8 7 6 5 4 3 2 1 0)) 0.001)
(define (tab-sin2 n)
  (tabulate sin n)
  )


(test)

