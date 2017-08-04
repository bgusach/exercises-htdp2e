#lang htdp/isl+

(require test-engine/racket-tests)
(require racket/list)
(require racket/string)
(require racket/base)


; ==================== Exercise 401 ====================

; ### Data Definitions

; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol 


; ### Functions
; S-expr S-expr -> Boolean
(check-expect (sexp=? 'a 'a) #t)
(check-expect (sexp=? 'a 'b) #f)
(check-expect (sexp=? '(a) '(a)) #t)
(check-expect (sexp=? '(a b) '(a b)) #t)
(check-expect (sexp=? '(a b c) '(a b c)) #t)
(check-expect (sexp=? '(a b c) '(a b d)) #f)
(check-expect (sexp=? '(a b (c)) '(a b (c))) #t)
(check-expect (sexp=? '(a b (c)) '(a b (d))) #f)
(define (sexp=? a b)
  (cond
    [(and (atom? a) (atom? b)) (equal? a b)]
    [(and (not (atom? a)) (atom? b)) #f]
    [(and (atom? a) (not (atom? b))) #f]
    [else (sl=? a b)]
    ))

(define (atom? x)
  (or (number? x) (string? x) (symbol? x))
  )


; [List-of S-expr] [List-of S-expr] -> Boolean
(define (sl=? a b)
  (cond
    [(and (empty? a) (empty? b)) #t]
    [(and (not (empty? a)) (empty? b)) #f]
    [(and (empty? a) (not (empty? b))) #f]
    [else
      (and
        (sexp=? (first a) (first b))
        (sl=? (rest a) (rest b))
        )]))

; =================== End of exercise ==================




; ==================== Exercise 402 ====================

; Q: Why was it a good a idea to think of the given expression 
;    as an atomic value at first in exercise 354?
; A: It seems to me it is a "Type 1" of multiple complex data,
;    where there is a dominant one (the association list) and
;    less dominant one (the expression). Once we got in the hand
;    an association, we can pass it along with the expression
;    to another function, and there the expression is the element
;    to be traversed.

; =================== End of exercise ==================

(test)

