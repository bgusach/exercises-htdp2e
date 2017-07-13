#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)

; ### Data definitions

(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BinaryTree (short for BT) is one of:
; – NONE
; – (make-node Number Symbol BT BT)



; ==================== Exercise 322 ====================

(define tree0 
  (make-node 
    15 
    'd 
    NONE 
    (make-node 24 'i NONE NONE)
    ))
;
;     15
;     |
;  +--+--+
;  |     |
;        24
;

(define tree1
  (make-node
    15
    'd
    (make-node 87 'h NONE NONE)
    NONE
    ))

;
;     15
;     |
;  +--+--+
;  |     |
;  87     
;

; ### Functions

; BT Number -> Boolean
; Returns whether a number is in the tree
(check-expect (contains-bt? tree0 15) #true)
(check-expect (contains-bt? tree0 24) #true)
(check-expect (contains-bt? tree0 99) #false)
(check-expect (contains-bt? tree1 87) #true)
(define (contains-bt? btree num)
  (cond
    [(no-info? btree) #false]
    [else
      (or
        (= (node-ssn btree) num)
        (contains-bt? (node-left btree) num)
        (contains-bt? (node-right btree) num)
        )]))

; =================== End of exercise ==================



; ==================== Exercise 323 ====================

; ### Functions

; BT Number -> Symbol
; Finds a node that matches the ssn `num` and returns its name
; or #false if not found
(check-expect (search-bt tree0 90) #false)
(check-expect (search-bt tree0 15) 'd)
(check-expect (search-bt tree0 24) 'i)
(check-expect (search-bt tree1 87) 'h)
(define (search-bt btree num)
  (cond
    [(no-info? btree) #false]
    [else
      (if
        (= (node-ssn btree) num)
        (node-name btree)
        (for/or ([subtree (list (node-left btree) (node-right btree))]) 
          (search-bt subtree num))
          )]))


; =================== End of exercise ==================




; ==================== Exercise 324 ====================

(define twelve-leaf
  (make-node 12 'ppp NONE NONE)
  )

(define four-leaf
  (make-node 4 'ppp NONE NONE)
  )

(define bst0
  (make-node 5 'd
    (make-node 3 'h
      (make-node 2 'ppp NONE NONE)
      four-leaf
      )
    (make-node 10 'x
      (make-node 8 'ppp NONE NONE)
      twelve-leaf
      )))

; bst0
;                5
;                |
;        +-------+-------+
;        |               |
;        3               10
;        |               |
;   +----+----+     +----+----+
;   |         |     |         |
;   2         4     8         12
; 



; BT -> [List-of Number]
(check-expect (inorder NONE) '())
(check-expect (inorder (make-node 5 'x NONE NONE)) '(5))
(check-expect (inorder tree1) '(87 15))
(check-expect (inorder bst0) '(2 3 4 5 8 10 12))
(define (inorder bst)
  (cond
    [(no-info? bst) '()]
    [else
      (append
        (inorder (node-left bst))
        (list (node-ssn bst))
        (inorder (node-right bst))
	)]))

; Q: What does inorder produce for a binary search tree?
; A: A perfectly sorted list (as shown in last test)

; =================== End of exercise ==================




; ==================== Exercise 325 ====================

; Number BST -> BST
; Finds a node in `bst` that matches `n`, or returns NONE
(check-expect (search-bst 7 NONE) NONE)
(check-expect (search-bst 9 bst0) NONE)
(check-expect (search-bst 12 bst0) twelve-leaf)
(check-expect (search-bst 4 bst0) four-leaf)
(define (search-bst n bst)
  (cond
    [(no-info? bst) NONE]
    [else
      (local 
        ((define this-ssn (node-ssn bst)))

        ; -- IN --
        (cond 
          [(= n this-ssn) bst]
          [(< n this-ssn) (search-bst n (node-left bst))]
          [(> n this-ssn) (search-bst n (node-right bst))]
            ))]))


; =================== End of exercise ==================




; ==================== Exercise 326 ====================

(define big-bst
  (make-node 63 'x
    (make-node 29 'x
      (make-node 15 'x
        (make-node 10 'x NONE NONE)
        (make-node 24 'x NONE NONE)
        )
      NONE
      )
    (make-node 89 'x
      (make-node 77 'x NONE NONE)
      (make-node 95 'x
        NONE
        (make-node 99 'x NONE NONE)
        ))))


; BST N Symbol -> BST
; Given a BST, it extends it with the new `ssn` and `name`
(check-expect (create-bst NONE 43 'lol) (make-node 43 'lol NONE NONE))
(check-expect 
  (create-bst twelve-leaf 43 'lol) 
  (make-node 12 'ppp
    NONE 
    (make-node 43 'lol NONE NONE)
    ))
(check-expect 
  (create-bst
    (make-node 5 'x
      (make-node 3 'x NONE NONE)
      NONE
      )
    1 
    'x
    )
  (make-node 5 'x
    (make-node 3 'x
      (make-node 1 'x NONE NONE)
      NONE
      )
    NONE
    ))
(check-expect
  (create-bst bst0 1 'x)
  (make-node 5 'd
    (make-node 3 'h
      (make-node 2 'ppp 
        (make-node 1 'x NONE NONE)
        NONE
        )
      four-leaf
      )
    (make-node 10 'x
      (make-node 8 'ppp NONE NONE)
      twelve-leaf
      )))
; Example from Figure 115
(check-expect
  (create-bst big-bst 75 'x)
  (make-node 63 'x
    (make-node 29 'x
      (make-node 15 'x
        (make-node 10 'x NONE NONE)
        (make-node 24 'x NONE NONE)
        )
      NONE
      )
    (make-node 89 'x
      (make-node 77 'x 
        (make-node 75 'x NONE NONE)
        NONE
        )
      (make-node 95 'x
        NONE
        (make-node 99 'x NONE NONE)
        ))))
(define (create-bst bst ssn name)
  (cond
    [(no-info? bst) (make-node ssn name NONE NONE)]
    [else
      (local
        ((define this-ssn (node-ssn bst))
         (define this-name (node-name bst))
         (define left (node-left bst))
         (define right (node-right bst))
         )

        ; -- IN --
        (cond
          [(< ssn this-ssn) 
           (make-node 
             this-ssn 
             this-name 
             (create-bst left ssn name)
             right
             )]
          [(> ssn this-ssn) 
           (make-node 
             this-ssn 
             this-name 
             left 
             (create-bst right ssn name)
             )]))]))

; =================== End of exercise ==================




; ==================== Exercise 327 ====================

; ### Functions
; [List-of [List Number Symbol]] -> BST
; Creates a BST from a list of social security numbers
(check-expect 
  (create-bst-from-list '((10 x))) 
  (make-node 10 'x NONE NONE)
  )
(check-expect 
  (create-bst-from-list '((10 x) (20 x) (2 x) (7 x))) 
  (make-node 10 'x 
    (make-node 2 'x
      NONE 
      (make-node 7 'x NONE NONE)
      )
    (make-node 20 'x NONE NONE)
    ))
(define (create-bst-from-list pairs)
  (foldl
    (λ (pair acc) (create-bst acc (first pair) (second pair)))
    NONE
    pairs
    ))

; Q: if create-bst-from-list is applied to 
;    '((99 o) (77 l) (24 i) (10 h) (95 g) (15 d) (89 c) (29 b) (63 a))
;    you may get the tree of Figure 115, but inverted. Why?
;
; A: we do get the inverted tree here. This happens because we are processing
;    the items from left to right with foldl. If foldr is used instead,
;    the BST matches exactly the one of figure 115. Alternatively, the
;    list can be reversed before being passed to the function.

; =================== End of exercise ==================

(test)

