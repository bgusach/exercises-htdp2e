#lang htdp/isl

(require test-engine/racket-tests)

; ### Constants

; ### Data Definitions

; ### Functions

; String [List-of String] -> Boolean
; Returns whether any string in los starts with str
(check-expect (find-name "john" '("matt" "paco" "sven")) #false)
(check-expect (find-name "john" '("matt" "johnny" "sven", "joe")) #true)
(define (find-name str-to-find los)
  (local
    (; String -> Boolean
     ; Returns whether some-str starts with str
     (define (match? some-str)
       (and
         (<= len-of-str-to-find (string-length some-str))
         (string=?
           str-to-find
           (substring some-str 0 len-of-str-to-find)
           )))

       (define len-of-str-to-find (string-length str-to-find))
       )

    ; -- IN --
    (ormap match? los)
    ))


; [List-of String] -> [List-of String]
; Checks whether all names of list start with "a"
(check-expect (all-start-with-a? '("armando" "anita")) #true)
(check-expect (all-start-with-a? '("armando" "anita" "jose")) #false)
(define (all-start-with-a? los)
  (local
    (; String -> Boolean
     (define (starts-with-a str)
       (and
         (> (string-length str) 0)
         (string=? 
           (string-ith str 0)
           "a"
           ))))

    ; -- IN --
    (andmap starts-with-a los)
    ))


; Q: Should you use ormap or andmap to define a function that 
;    ensures that no name on some list exceeds some given width? 
; 
; A: ormap is good to check that at least one item meets a condition
;    whereas andmap is to check that **all** elements meet a condition.
;    Therefore andmap is the right one.

(test)

