#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require racket/list)


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
(check-expect (average-age Carl 2017) 91)
(check-expect (average-age Adam 2017) 83)
(define (average-age tree curr-year)
  (local
    ((define family-line (extract-nodes tree))
     )

    ; -- IN --
    (/
      (for/sum ([node family-line])
        (- curr-year (child-date node)))

      (length family-line)
      )))


; =================== End of exercise ==================




; ==================== Exercise 312 ====================
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

;    The following function works. A version .v2 is also available


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
      (or
        (blue-eyed-child? (child-mother a-ftree))
        (blue-eyed-child? (child-father a-ftree))
        )]))


; FT -> Boolean
; Does a-ftree contain a child with "blue" eyes?
(check-expect (blue-eyed-child? Carl) #false)
(check-expect (blue-eyed-child? Gustav) #true)
(define (blue-eyed-child? a-ftree)
  (cond
    [(no-parent? a-ftree) #false]
    [else 
      (or 
        (string=? (child-eyes a-ftree) "blue")
        (blue-eyed-child? (child-father a-ftree))
        (blue-eyed-child? (child-mother a-ftree))
        )]))


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



; ==================== Exercise 314 ====================
; ### Data Definitions

; a FF is a [List-of FT]

; ### Constants
(define ff1 (list Carl Bettina))
(define ff2 (list Fred Eva))
(define ff3 (list Fred Eva Carl))


; ### Functions
; FF -> Boolean
; Returns whether the forest contain any child with "blue" eyes
(check-expect (blue-eyed-child-in-forest? ff1) #false)
(check-expect (blue-eyed-child-in-forest? ff2) #true)
(check-expect (blue-eyed-child-in-forest? ff3) #true)
(define (blue-eyed-child-in-forest? a-forest)
  (ormap blue-eyed-child? a-forest)
  )

; =================== End of exercise ==================




; ==================== Exercise 315 ====================

; ### Functions
; FF Number -> Number
; Returns the average age of the forest
(check-expect (average-forest-age ff1 2017) 91)
(check-expect (average-forest-age ff2 2017) 71.25)
(define (average-forest-age a-forest curr-year)
  (local
    ((define all-nodes
       (flatten
         (for/list ([ft a-forest]) (extract-nodes ft))
         ))
     (define all-dates (map child-date all-nodes))
     (define all-ages (for/list ([year all-dates]) (- curr-year year)))
     )

     ; -- IN --
     (/ 
       (foldl + 0 all-ages)
       (length all-nodes)
       )))

; =================== End of exercise ==================

(test)

