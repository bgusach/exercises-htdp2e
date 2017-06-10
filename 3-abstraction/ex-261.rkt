#lang htdp/isl

(require test-engine/racket-tests)


; ### Data Definitions
(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)

; An Inventory is [List-of IR]
; for instance:

; ### Constants
(define 
  some-inv
  (list 
    (make-IR "a" 0.1)
    (make-IR "b" 1)
    (make-IR "c" 4)
    (make-IR "d" 6)
    (make-IR "e" 0.6)
    (make-IR "f" 0.7)
    (make-IR "g" 3.7)
    ))

; ### Functions

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar

(check-expect 
  (extract1 some-inv) 
  (list 
    (make-IR "a" 0.1)
    (make-IR "b" 1)
    (make-IR "e" 0.6)
    (make-IR "f" 0.7)
    ))
(define (extract1 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
     (cond
       [(<= (IR-price (first an-inv)) 1.0)
        (cons (first an-inv) (extract1 (rest an-inv)))]
       [else (extract1 (rest an-inv))])]))


(check-expect 
  (extract2 some-inv) 
  (list 
    (make-IR "a" 0.1)
    (make-IR "b" 1)
    (make-IR "e" 0.6)
    (make-IR "f" 0.7)
    ))
(define (extract2 an-inv)
  (cond
    [(empty? an-inv) '()]
    [else
      (local 
        ((define extraction-rest (extract2 (rest an-inv)))
         )

        ; -- IN --
        (cond
          [(<= (IR-price (first an-inv)) 1.0) 
           (cons (first an-inv) extraction-rest)]
          [else extraction-rest]
          ))]))

; [X] X -> IR
(define (make-random-IR n)
  (make-IR n (random 4))
  )

(define massive-list (build-list 1000000 make-random-IR))


; NOTE: Just a helper so that the millions of objects don't clutter the
; stdout
(define (suppress-stdout _)
  ":)"
  )

(suppress-stdout (time (extract1 massive-list)))
(suppress-stdout (time (extract2 massive-list)))

; NOTE: extract2 seems to be worse... which does not make too much
; sense... maybe I effed up

(test)

