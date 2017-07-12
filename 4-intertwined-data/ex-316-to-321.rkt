#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)

; ### Data definitions

; An Atom is one of: 
; – Number
; – String
; – Symbol 
;
; For instance:
; - 'lol
; - "lol"
; - 42

; An SL is one of: 
; – '()
; – (cons S-expr SL)
;
; For instance:
; - '()
; - '(lol 42)
; - '(lol 42 (huehue))
	
; An S-expr is one of: 
; – Atom
; – SL
; For instance:
; - 34
; - '()
; - '(hola lol (huehue hihihi)
 


; ==================== Exercise 316 ====================

; ### Functions

; [X] X -> Boolean
; Atom predicate
(define (atom? x)
  (or (number? x) (string? x) (symbol? x))
  )

; =================== End of exercise ==================




; ==================== Exercise 317 ====================

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(define (count sexp sy)
  (local
    (; SL Symbol -> N 
     ; counts all occurrences of sy in sl 
     (define (count-sl sl)
       (match 
         sl
         ['() 0]
         [(cons head tail)
          (+
            (count head sy)
            (count-sl tail)
            )]))

     ; Atom Symbol -> N 
     ; counts all occurrences of sy in at 
     (define (count-atom at)
       (cond
         [(number? at) 0]
         [(string? at) 0]
         [(symbol? at) (if (symbol=? at sy) 1 0)]
         )))

    ; -- IN --
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)]
      )))
 
 

; =================== End of exercise ==================




; ==================== Exercise 318 ====================

; S-expr -> N 
; Determines depth of symbolic expression
(check-expect (depth "hello") 1)
(check-expect (depth '()) 1)
(check-expect (depth '(world hello)) 2)
(check-expect (depth '((world) hello)) 3)
(define (depth sexp)
  (local
    ((define (depth-sl sl)
       (match
         sl
         ['() 0]
         [(cons head tail) 
          (max (depth head) (depth-sl tail))]
          )))

    ; -- IN --
    (match 
      sexp
      [(? atom?) 1]
      [else
        (add1 (depth-sl sexp))
        ])))
 

; =================== End of exercise ==================




; ==================== Exercise 319 ====================

; S-expr Symbol Symbol -> N 
; Replaces all the occurrences of `old` with `new`
(check-expect (replace "hello" 'a 'b) "hello")
(check-expect 
  (replace '(hello world) 'hello 'hola)
  '(hola world)
  )
(check-expect 
  (replace '((world) hello) 'hello 'hola) 
  '((world) hola)
  )
(define (replace sexp old new)
  (local
    (; Atom -> Atom
     (define (replace-atom atom)
     (if 
       (and (symbol? atom) (symbol=? atom old))
       new
       atom
       ))

     ; SL -> SL
     (define (replace-sl sl)
       (match
         sl
         ['() '()]
         [(cons head tail)
          (cons
            (replace head old new)
            (replace-sl tail)
            )])))

    ; -- IN --
    (match
      sexp
      [(? atom?) (replace-atom sexp)]
      [else
        (replace-sl sexp)
        ])))
 

; =================== End of exercise ==================




; ==================== Exercise 320 ====================

; ### Data definitions

; An SL is a [List-of S-expr]
	
; An S-expr is one of: 
; – Number
; – String
; – Symbol 
; – SL


; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(check-expect (count.v2 '(((world) hello) hello) 'hello) 2)
(define (count.v2 sexp sy)
  (local
    ((define (count-sl sexp)
       (match
         sexp
         ['() 0]
         [(cons head tail)
          (+
            (count.v2 head sy)
            (count-sl tail)
            )])))

    ; -- IN --
    (match
      sexp
      [(? number?) 0]
      [(? string?) 0]
      [(? symbol?) (if (symbol=? sexp sy) 1 0)]
      [else (count-sl sexp)]
      )))


; Now integrating SL into S-expr

; An S-expr is one of: 
; – Number
; – String
; – Symbol 
; – [List-of S-expr]

(check-expect (count.v3 'world 'hello) 0)
(check-expect (count.v3 '(world hello) 'hello) 1)
(check-expect (count.v3 '(((world) hello) hello) 'hello) 2)
(define (count.v3 sexp sy)
  (cond
    [(number? sexp) 0]
    [(string? sexp) 0]
    [(symbol? sexp) (if (symbol=? sexp sy) 1 0)]
    [else (for/sum ([subsexp sexp]) (count.v3 subsexp sy))]
    ))

; =================== End of exercise ==================


; ==================== Exercise 321 ====================

; ### Data definitions

; An [Abs-expr A ... Z] is one of: 
; – A
; – ...
; – Z
; – [List-of [Abs-expr A ... Z]


; An [Abs-SL A ... Z] is a [List-of [Abs-expr A ... Z]]
; NOTE: if SL is integrated into S-expr, there is no 
; need to redefine it. It makes it more confusing IMO

; =================== End of exercise ==================
(test)

