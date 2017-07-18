#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require htdp/dir)
(require racket/string)
(require racket/list)

; ==================== Exercise 345 ====================

; ### Data definitions
(define-struct add [left right])
(define-struct mul [left right])

; A BSL-expr is one of:
; - Number
; - (make-add BSL-expr BSL-expr)
; - (make-mul BSL-expr BSL-expr)
;
; Examples:
; - 42
; - (make-add 2 2)
; - (make-mul 10 (make-add 2 5))

; Translate the following expressions into data:
; (+ 10 -10) ->
(make-add 10 -10)

; (+ (* 20 3) 33) ->
(make-add (make-mul 20 3) 33)

; (+ 
;   (* 3.14 (* 2 3)) 
;   (* 3.14 (* -1 -9))
;   ) ->
(make-add
  (make-mul 3.14 (make-mul 2 3))
  (make-mul 3.14 (make-mul -1 -9))
  )


; Translate the following data as expressions:
(make-add -1 2)  
; -> (+ -1 2)

(make-add (make-mul -2 -3) 33)  
; -> (+ (* -2 3) 33)

(make-mul (make-add 1 (make-mul 2 3)) 3.14)
; -> (* (+ 1 (* 2 3)) 3.14)

; =================== End of exercise ==================




; ==================== Exercise 346 ====================

; ### Data Definitions

; An EvalResult is one of:
; - Number

; =================== End of exercise ==================




; ==================== Exercise 347 ====================

; ### Functions

; BSL-Expr -> EvalResult
; Evaluates `bsl-expr`
(check-expect (eval-expression 4) 4)
(check-expect (eval-expression (make-add 2 3)) 5)
(check-expect (eval-expression (make-mul 2 3)) 6)
(check-within
  (eval-expression (make-mul (make-add 1 (make-mul 2 3)) 3.14))
  (* (+ 1 (* 2 3)) 3.14)
  0.01
  )
(define (eval-expression bsl-expr)
  (match
    bsl-expr
    [(? number?) bsl-expr]
    [(add left right) (+ (eval-expression left) (eval-expression right))]
    [(mul left right) (* (eval-expression left) (eval-expression right))]
    ))

; =================== End of exercise ==================




; ==================== Exercise 348 ====================
; ### Data Definitions

(define-struct bool-or [left right])
; A Bool-Or is a structure:
;   (make-bool-or Bool-BSL-Expr Bool-BSL-Expr)
; Intepretation: data representation of boolean OR expression

(define-struct bool-and [left right])
; A Bool-And is a structure:
;   (make-bool-and Bool-BSL-Expr Bool-BSL-Expr)
; Intepretation: data representation of boolean AND expression

(define-struct bool-not [expr])
; A Bool-Not is a structure:
;   (make-bool-not Bool-BSL-Expr)
; Intepretation: data representation of boolean NOT expression

; A Bool-BSL-Expr is one of
; - #true
; - #false
; - (make-bool-or Bool-BSL-Expr Bool-BSL-Expr)
; - (make-bool-and Bool-BSL-Expr Bool-BSL-Expr)
; - (make-bool-not Bool-BSL-Expr)
; 
; For instance:
; - #true
; - #false
; - (make-bool-not #true)
; - (make-bool-or 
;     #false 
;     (make-bool-and 
;       #true 
;       (make-bool-not #false)
;       ))

; A BoolEvalResult is one of:
; - #true
; - #false

; Bool-BSL-Expr -> BoolEvalResult
; Evaluates a boolean bsl expression
(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression #false) #false)
(check-expect 
  (eval-bool-expression (make-bool-not #false))
  #true
  )
(check-expect 
  (eval-bool-expression (make-bool-not #true))
  #false
  )
(check-expect 
  (eval-bool-expression (make-bool-and #true (make-bool-not #false)))
  #true
  )
(check-expect 
  (eval-bool-expression (make-bool-or #false (make-bool-not #false)))
  #true
  )
(define (eval-bool-expression expr)
  (match
    expr
    [(? boolean?) expr]

    [(bool-or left right) 
     (or 
       (eval-bool-expression left)
       (eval-bool-expression right)
       )]

    [(bool-and left right) 
     (and 
       (eval-bool-expression left)
       (eval-bool-expression right)
       )]

    [(bool-not expr) (not (eval-bool-expression expr))]
    ))


; =================== End of exercise ==================


; ==================== Exercise 349 ====================

; S-expr -> BSL-expr
; Generates an BSL expression from a S-expression, as
; long as it can be parsed. Bear in mind, that Bool-BSL-Expr
; are not (so far) part of BSL-Expr
(check-expect (parse 45) 45)
(check-expect (parse '(* 3 4)) (make-mul 3 4))
(check-expect (parse '(* 3 (+ 3 4))) (make-mul 3 (make-add 3 4)))
(check-error (parse "lol"))
(check-error (parse '(/ 10 5)))
(check-error (parse '(+ 10 5 3)))
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]
    ))
 
; NOTE: these kind of error are not requested by the exercise
(define ERROR-LENGTH "length of s-expr must be exactly 3)")
(define ERROR-SYMBOL "unexpected symbol")
(define ERROR-ATOM "unexpected atom")


; SL -> BSL-expr 
(define (parse-sl s)
  (local 
    ((define L (length s)))

    ; -- IN --
    (cond
      [(< L 3) (error ERROR-LENGTH)]

      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error ERROR-SYMBOL)]
         )]

      [else (error ERROR-LENGTH)]
      )))
 

; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error ERROR-ATOM)]
    [(symbol? s) (error ERROR-ATOM)]
    ))


; [X] X -> Boolean
; Atom predicate
(define (atom? x)
  (or (number? x) (string? x) (symbol? x))
  )

; =================== End of exercise ==================




; ==================== Exercise 350 ====================

; Q: What is unusual about the definition of this 
;    program with respect to the design recipe?
; A: I think what is unusual of the function `parse`
;    is the fact that we don't process the list inputs
;    as lists (i.e. iterating over its items, and 
;    merging them in some way), but as a some kind of 
;    "structure" or record, where the first element 
;    represents the structure type.

; =================== End of exercise ==================




; ==================== Exercise 351 ====================

; S-Expr -> [Either Number Error]
; Parses the S-expression and executes it if successfully
; parsed, otherwise the parsing error is returned
(check-expect (interpreter-expr 10) 10)
(check-expect (interpreter-expr '(+ 1 1)) 2)
(check-expect (interpreter-expr '(* 2 (+ 2 2))) 8)
(check-error (interpreter-expr "lol") ERROR-ATOM)
(check-error (interpreter-expr '(/ 2 2)) ERROR-SYMBOL)
(check-error (interpreter-expr '(+ 2 2 2)) ERROR-LENGTH)
(define (interpreter-expr sexpr)
  (eval-expression (parse sexpr))
  )

; =================== End of exercise ==================


(test)

