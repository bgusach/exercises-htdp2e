#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; [X] X -> Boolean
; Atom predicate
(define (atom? x)
  (or (number? x) (string? x) (symbol? x))
  )


; S-expr Symbol Symbol -> N 
; Replaces all the occurrences of `old` with `new`
(check-expect (substitute "hello" 'a 'b) "hello")
(check-expect 
  (substitute '(hello world) 'hello 'hola)
  '(hola world)
  )
(check-expect 
  (substitute '((world) hello) 'hello 'hola) 
  '((world) hola)
  )
(check-expect 
  (substitute '(((world) bye) bye) 'bye '42)
  '(((world) 42) 42)
  )
(define (substitute sexp old new)
  (cond
    [(atom? sexp) (if (equal? sexp old) new sexp)]
    [else (for/list ([it sexp]) (substitute it old new))]
    ))
 
; Q: Explain why we had to use lambda for this last simplification.
; A: Well, I didn't :). But I guess the right answer is because
;    we need a function that applies substitute to those three
;    arguments. And a lambda (or for/list) is less verbose than 
;    local definition.


(test)

