#lang htdp/isl+

(require test-engine/racket-tests)
(require racket/list)
(require racket/string)
(require racket/base)


; ==================== Exercise 399 ====================

; [List-of String] -> [List-of String] 
; picks a random non-identity arrangement of names
(define (gift-pick names)
  (random-pick
    (non-same names (arrangements names))
    ))
 

; [List-of String] -> [List-of [List-of String]]
; returns all possible permutations of names
(define arrangements permutations)


; [NEList-of X] -> X 
; returns a random item from the list 
(define (random-pick l)
  (list-ref l (random (length l)))
  )
 

; [List-of String] [List-of [List-of String]] 
;   -> [List-of [List-of String]]
; Produces the list of those lists in ll that do 
; not agree with names at any place 
(check-expect 
  (non-same '("a" "b" "c") '(("a" "b" "c") ("b" "a" "c") ("c" "a" "b")))
  '(("c" "a" "b"))
  )
(define (non-same names ll)
  (local
    ((define (any-collision? l0 l1)
       (cond
         [(empty? l0) #false]
         [else
           (or
             (string=? (first l0) (first l1))
             (any-collision? (rest l0) (rest l1))
             )])))

    ; -- IN --
    (cond
      [(empty? ll) '()]
      [else
       (if 
         (any-collision? names (first ll))
         (non-same names (rest ll))
         (cons (first ll) (non-same names (rest ll)))
         )])))

(define input '("Louise" "Jane" "Laura" "Dana" "Mary"))
(define output (gift-pick input))

; NOTE: just a nice way to print to stdout
(display 
  (string-join 
    (for/list [(a input) (b output)] (format "~a -> ~a\n" a b))
    ))

; =================== End of exercise ==================


(test)

