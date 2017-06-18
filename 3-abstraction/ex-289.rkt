#lang htdp/isl+

(require test-engine/racket-tests)
(require racket/string)

; ### Constants

; ### Data Definitions

; ### Functions

; String [List-of String] -> Boolean
; Returns whether any string in los starts with str
(check-expect (find-name "john" '("matt" "paco" "sven")) #false)
(check-expect (find-name "john" '("matt" "johnny" "sven", "joe")) #true)
(define (find-name str-to-find los)
  (ormap (λ (s) (string-prefix? s str-to-find)) los)
  )


; [List-of String] -> [List-of String]
; Checks whether all names of list start with "a"
(check-expect (all-start-with-a? '("armando" "anita")) #true)
(check-expect (all-start-with-a? '("armando" "anita" "jose")) #false)
(define (all-start-with-a? los)
  (andmap (λ (s) (string-prefix? s "a")) los)
  )


; Q: Should you use ormap or andmap to define a function that 
;    ensures that no name on some list exceeds some given width? 
; 
; A: andmap. Because it checks a condition on all elemets, whereas
;    ormap checks that at least one satisfies the predicate.

(test)

