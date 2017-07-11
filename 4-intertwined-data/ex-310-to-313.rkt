#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ### Data Definitions

(define-struct no-parent [])
(define NP (make-no-parent))


(define-struct child [father mother name date eyes])
; A FT (short for family tree) is one of: 
; – NP
; – (make-child FT FT String N String)

; ### Constants

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))


; ==================== Exercise 310 ====================
; ### Functions

; FT -> Number
; Counts the nodes in the tree
(check-expect (count-persons Carl) 1)
(check-expect (count-persons Adam) 3)
(check-expect (count-persons Gustav) 5)
(define (count-persons tree)
  (match
    tree
    [(no-parent) 0]
    [(child ma pa _ _ _) 
     (+ 1 (count-persons ma) (count-persons pa))
     ]
    ))


; =================== End of exercise ==================




; ==================== Exercise 311 ====================
; ### Functions


; FT -> [List-of Child]
; Extracts all the tree nodes as a list
(check-expect (extract-nodes Carl) (list Carl))
(check-expect 
  (extract-nodes Gustav) 
  (list Gustav Fred Eva Carl Bettina)
  )
(define (extract-nodes tree)
  (cond
    [(no-parent? tree) '()]
    [else
      (cons
        tree
        (append 
          (extract-nodes (child-father tree))
          (extract-nodes (child-mother tree))
          ))]))


; FT -> Number
; Calculates the average age in the tree
(check-expect (average-age Carl) 91)
(check-expect (average-age Adam) 83)
(define (average-age tree)
  (local
    ((define family-line (extract-nodes tree))
     (define this-year 2017)
     )

    ; -- IN --
    (/
      (for/sum ([node family-line])
        (- this-year (child-date node)))

      (length family-line)
      )))


; =================== End of exercise ==================




; ==================== Exercise 312====================
; ### Functions


; FT -> [List-of String]
; Extracts the eye colour of the whole family line upwards
(check-expect (eye-colours Carl) '("green"))
(check-expect (eye-colours Adam) '("hazel" "green" "green"))
(define (eye-colours tree)
  (map child-eyes (extract-nodes tree))
  )

; =================== End of exercise ==================




; ==================== Exercise 313 ====================
; Q: why does the original function fail?
; A: because no clause ever returns #true. There is no check 
;    for colour anywhere. 

;    The following function works. It is however kind of clumsy, 
;    and another version of the func. can be found further down.


; ### Functions

; FT -> Boolean
; Returns whether any ancestor (not the starting node!)
; has blue eyes
(check-expect (blue-eyed-ancestor? Eva) #false)
(check-expect (blue-eyed-ancestor? Gustav) #true)
(define (blue-eyed-ancestor? a-ftree)
  (cond
    [(no-parent? a-ftree) #false]
    [else
      (local
        ((define ma (child-mother a-ftree))
         (define has-ma (child? ma))
         (define pa (child-father a-ftree))
         (define has-pa (child? pa))
         )

         ; -- IN --
         (or
           (and has-pa (or (blue-eyed? pa) (blue-eyed-ancestor? pa)))
           (and has-ma (or (blue-eyed? ma) (blue-eyed-ancestor? ma)))
           ))]))


; FT -> Boolean
(define (blue-eyed? node)
  (string=? (child-eyes node) "blue")
  )


; FT -> Boolean
; Returns whether any ancestor (not the starting node!)
; has blue eyes
(check-expect (blue-eyed-ancestor?.v2 Eva) #false)
(check-expect (blue-eyed-ancestor?.v2 Gustav) #true)
(define (blue-eyed-ancestor?.v2 tree)
  (match
    tree
    [(no-parent) #false]
    [(child (child _ _ _ _ "blue") _ _ _ _) #true]
    [(child _ (child _ _ _ _ "blue") _ _ _) #true]
    [(child ma pa _ _ _) 
     (or 
       (blue-eyed-ancestor?.v2 ma)
       (blue-eyed-ancestor?.v2 pa)
       )]))


; =================== End of exercise ==================


(test)

