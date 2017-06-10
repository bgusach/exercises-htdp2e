#lang htdp/isl

(require test-engine/racket-tests)

; ### Functions

; [A B] [A B -> B] B [List-of A] -> B
; f*oldl works like foldl
(check-expect (f*oldl cons '() '(1 2 3)) '(1 2 3))
(define (f*oldl fn acc loa)
  (cond
    [(empty? loa) acc]
    [else
      (fn
        (first loa)
        (f*oldl fn (rest loa) acc)
        )]))


; [N -> X] N -> [List-of X]
; Clon of build-list
(check-expect (build-l*st number->string 5) '("0" "1" "2" "3" "4"))
(define (build-l*st fn n)
  (map-over-interval fn 0 n)
  )


; [X] [N -> X] N N -> [List-of X]
; Maps fn over the natural interval [lower, upper)
(check-expect (map-over-interval number->string 3 5) '("3" "4"))
(define (map-over-interval fn lower upper)
  (cond
    [(>= lower upper) '()]
    [else
      (cons
        (fn lower)
        (map-over-interval fn (add1 lower) upper)
        )]))


(test)
