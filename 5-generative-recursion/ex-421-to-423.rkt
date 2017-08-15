#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; [List-of 1String] N -> [List-of String]
; Bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(check-expect 
  (bundle (explode "abcdefg") 3)
  (list "abc" "def" "g")
  )
(check-expect (bundle '("a" "b") 3) (list "ab"))
(check-expect (bundle '() 3) '())
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
      (cons 
        (implode (take s n)) 
        (bundle (drop s n) n)
        )]))

 
; [List-of X] N -> [List-of X]
; Keep the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else 
      (cons 
        (first l) 
        (take (rest l) (sub1 n))
        )]))
 

; [List-of X] N -> [List-of X]
; Remove the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]
    ))


; ==================== Exercise 421 ====================

; (bundle '("a" "b" "c") 0)

; Q: Is (bundle '("a" "b" "c") 0) a proper use of the bundle 
;    function? What does it produce? Why?
; A: Probably not, since it causes bundle to call itself with
;    the very same arguments... forever.

; =================== End of exercise ==================




; ==================== Exercise 422 ====================

; [List-of X] N -> [List-of [List-of X]]
; Takes chunks from `l` of `n` elements 
(check-expect 
  (list->chunks '(1 2 3 4 5 6 7) 2)
  '((1 2)
    (3 4)
    (5 6)
    (7)
    ))
(define (list->chunks l n)
  (cond
    [(empty? l) '()]
    [else
      (cons
        (take l n)
        (list->chunks (drop l n) n)
        )]))

(check-expect 
  (bundle.v2 (explode "abcdefg") 3)
  (list "abc" "def" "g")
  )
(check-expect (bundle.v2 '("a" "b") 3) (list "ab"))
(check-expect (bundle.v2 '() 3) '())
(define (bundle.v2 s n)
  (map implode (list->chunks s n))
  )

; =================== End of exercise ==================




; ==================== Exercise 423 ====================

; String N -> [List-of String]
; Splits `s` into chunks of length `n`
(check-expect (partition "hola manolo" 3) '("hol" "a m" "ano" "lo"))
(check-expect (partition "" 3) '())
(define (partition s n)
  (local
    ((define this-chunk-len (min (string-length s) n))
     )

    ; -- IN --
    (cond
      [(zero? this-chunk-len) '()]
      [else
        (cons
          (substring s 0 this-chunk-len)
          (partition (substring s this-chunk-len) n)
          )])))

; =================== End of exercise ==================

(test)

