#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require racket/string)


; ### Data definitions

; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())


; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])


; An Xexpr.v2 is a list: 
; – (cons Symbol [List-of Xexpr.v2])
; – (cons Symbol (cons [List-of Attribute] [List-of Xexpr.v2]))

; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))


; ==================== Exercise 363 ====================

; An Xexpr.v2 is a (cons Symbol XE-Body)


; An XE-Body (short for XML expression body)
; is one of:
; - [List-of Xexpr.v2]
; - '(cons [List-of Attribute] [List-of Xexpr.v2])


; An Attribute is a pair (list of two items):
;   (cons Symbol (cons String '()))

; =================== End of exercise ==================




; ==================== Exercise 364 ====================

; 1: <transition from="seen-e" to="seen-f" />
'(transition ((from "seen-e") (to "seenf")))

; 2: <ul><li><word /><word /></li><li><word /></li></ul>
'(ul
   (li (word) (word))
   (li (word))
   )

; Q: Which one could be represented in Xexpr.v0 or Xexpr.v1?
; A: Xexpr.v0 only supports one node without subnodes or
;    attributes, therefore neither 1 nor 2 can be represented
;    with that definition.
;    Xexpr.v1 supports subnodes, and therefore can be used
;    for 2, but not for 1, because it does not allow attributes.
;    

; =================== End of exercise ==================




; ==================== Exercise 365 ====================

; 1: '(server ((name "example.org"))
;    <server name="example.org" />

; 2: '(carcas (board (grass)) (player ((name "sam"))))
;    <carcas>
;      <board>
;        <grass />
;      </board>
;      <player name="sam" />
;    </carcas>

; 3: '(start)
;    <start />

; Q: Which ones are elements of Xexpr.v0 or Xexpr.v1?
; A: 1 -> is neither
;    2 -> Xexpr.v1
;    3 -> Xexpr.v0 

; =================== End of exercise ==================




; ==================== Exercise 366 ====================

(define a0 '((initial "X")))
(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))

; NOTE: does not belong to the exercise, but xexpr-attr has
; been rewritten

; Xexpr.v2 -> [List-of Attribute]
; Retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))
(define (xexpr-attr xe)
  (local
    ((define attr+children (rest xe)))

    ; -- IN --
    (match
      attr+children
      ['() '()]
      [(cons head _)
       (if (list-of-attributes? head) head '())
       ]
      [else '()]
      )))


; [List-of Attribute] or Xexpr.v2 -> Boolean
; Returns whether the given value is a list of attributes
(define (list-of-attributes? x)
  (match
    x
    ['() #true]
    [(cons (? cons?) _) #true]
    [else #false]
    ))


; Xexpr.v2 -> Symbol
; Returns the name of `xe`
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)
(check-error (xexpr-name a0) "Expected Xexpr, got ((initial \"X\")) instead")
(define (xexpr-name xe)
  (match
    xe
    [(cons (? symbol?) _) (first xe)]
    [else (error (format "Expected Xexpr, got ~s instead" xe))]
    ))


; Xexpr.v2 -> XEConts
; Returns the contents (i.e. children) of `xe`
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
(check-error 
  (xexpr-content "broken") 
  "Expected Xexpr, got \"broken\" instead"
  )
(define (xexpr-content xe)
  (local
    ((define _ (xexpr-name xe))  ; Just to crash gracefully
     (define loa+content (rest xe))
     )

     ; -- IN --
     (match
       loa+content
       ['() '()]
       [(cons head tail)
        (if 
          (list-of-attributes? head)
          tail
          loa+content
          )])))

; =================== End of exercise ==================




; ==================== Exercise 367 ====================

; Q: Explain why xexpr-attr does not need a self-reference
; A: The recipe calls for recursion when dealing with
;    self-referencing data structures as lists are.
; 
;    However, lists are not used here as an arbitrarily 
;    large data structure, but as a "shorthand" for 
;    structures (i.e. first attribute is pos 0, second 1, etc)
;    Therefore it does not make sense to apply the same
;    function to all the elements of the "list as a structure".
;    Each element of the list as a different whole meaning.
;    In other languages, tuples would be used instead of lists.

; =================== End of exercise ==================




; ==================== Exercise 368 ====================

; An [Either A B] is a one of:
; - A
; - B

; An AttributesOrXexpr is a:
;  [Either [List-of Attribute] Xexpr.v2]

; With this new Data Definition, the function
; signature of xexpr-attr would look like this:
; AttributesOrXexpr -> Boolean

; =================== End of exercise ==================




; ==================== Exercise 369 ====================

; [List-of Attribute] Symbol -> [Maybe String]
(check-expect (find-attr a0 'initial) "X")
(check-expect (find-attr a0 'not-existing) #false)
(check-expect (find-attr '((w 39) (h 84) (b 99)) 'b) 99)
(define (find-attr attrs name)
  (local
    ((define res (assq name attrs)))

    ; -- IN --
    (if 
      (false? res) 
      #false
      (second res)
      )))


; =================== End of exercise ==================


; NOTE: from now on, Xexpr refers to Xexpr.v2

; ### Data Definitions
; An XWord is '(word ((text String))).
; 
; Its XML node would be: <word text="...">



; ==================== Exercise 370 ====================

(define word0 '(word ((text "hello"))))
(define word1 '(word ((text "schnitzel"))))
(define word2 '(word ((text "paella"))))


; [X] X -> Boolean
(check-expect (word? word0) #true)
(check-expect (word? word1) #true)
(check-expect (word? '(this is no word)) #false)
(check-expect (word? '(word "neither this")) #false)
(define (word? x)
  (match
    x
    [(cons 'word (cons attrs '()))
     (and
       (list-of-attributes? attrs)
       (symbol=? (caar attrs) 'text)
       (string? (cadar attrs))
       )]
    [else #false]
    ))


; XWord -> String
(check-expect (word-text word0) "hello")
(define (word-text w)
  (if
    (word? w)
    (second (first (second w)))
    (error (format "expected word, got ~a" w))
    ))


(require racket/trace)

; =================== End of exercise ==================




(test)

