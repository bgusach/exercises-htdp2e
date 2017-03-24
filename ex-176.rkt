#lang htdp/bsl

; ### Constants

; ### Data Definitions

; ### Functions

; Matrix -> Matrix
; Transpose the given matrix along the diagonal 
; Sorry, it's too annoying to use (cons ...), and htpd/bsl seems to accept (list ...)
(define wor1 (list 11 21))
(define wor2 (list 12 22))
(define tam1 (list wor1 wor2))
(define mat1 (list (list 11 12) (list 21 22)))
(check-expect (transpose mat1) tam1)
(define mat2 (list (list 11 0) (list 21 0) (list 31 0)))  
(check-expect (transpose mat2) (list (list 11 21 31) (list 0 0 0)))
 
(define (transpose lln)
  (cond
    [(empty? (first lln)) '()]
    [else (cons (first* lln) (transpose (rest* lln)))]))


; Matrix -> List-of-numbers
; Gets the first column of the matrix as a list of numbers
(check-expect (first* tam1) (list 11 12))
(check-expect (first* mat2) (list 11 21 31))
(define (first* m)
  (cond
    [(empty? m) '()]
    [else 
      (cons
        (first (first m))
        (first* (rest m))
        )]))

; Matrix -> Matrix
; Drops the first column of the matrix
(check-expect (rest* tam1) (list (list 21) (list 22)))
(define (rest* m)
  (cond
    [(empty? m) '()]
    [else
      (cons
        (rest (first m))
        (rest* (rest m))
        )]))

(require test-engine/racket-tests)
(test)

