#lang htdp/isl+

(require test-engine/racket-tests)
(require racket/list)

; ### Constants
; Distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; ### Functions
; N -> [List-of Posn]
; generate n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3) (n-inside-playground? 3))
(check-satisfied (random-posns 5) (n-inside-playground? 5))
(define (random-posns n)
  (build-list
    n
    (λ (i) (make-posn (random WIDTH) (random HEIGHT)))
    ))


; N -> [[List-of Posn] -> Boolean]
; Function specification for random-posns
(define (n-inside-playground? n)
  (λ (lop)
    (and
      (= (length lop) n)
      (andmap 
        (λ (p) 
          (and 
            (>= (posn-x p) 0)
            (< (posn-x p) WIDTH)
            (>= (posn-y p) 0)
            (< (posn-y p) HEIGHT)
            ))
        lop
      ))))


(check-satisfied (random-posns/bad 3) (n-inside-playground? 3))
(check-satisfied (random-posns/bad 5) (n-inside-playground? 5))
(define (random-posns/bad n)
  (build-list
    n
    (λ (i) (make-posn 0 0))
    ))

(test)

