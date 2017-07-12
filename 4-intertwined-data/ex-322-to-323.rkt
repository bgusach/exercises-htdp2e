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


(test)

