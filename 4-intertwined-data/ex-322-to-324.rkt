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
  (make-node
    5
    'd
    (make-node 
      3
      'h
      (make-node 2 'ppp NONE NONE)
      four-leaf
      )
    (make-node 
      10
      'x
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

(test)

