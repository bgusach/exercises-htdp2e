#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ### Constants

; ### Data Definitions

; A Node is a Symbol

; A Graph is a list of (cons Node [List-of Node])
; Meaning: list of elements, of which first element
; is a node, and second element a list of nodes to 
; which the previous node is connected

; Examples:
(define sample-graph-0
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())
    ))

(define sample-graph-1
  (list
    (list 'A (list 'B 'E))
    (list 'B (list 'E 'F))
    (list 'C (list 'D))
    (list 'D '())
    (list 'E (list 'C 'F))
    (list 'F (list 'D 'G))
    (list 'G '())
    ))

(check-expect sample-graph-0 sample-graph-1)


; ==================== Exercise 471 ====================
; ### Functions

; Node Graph -> List of Node
; Returns the list of nodes connected to `node`
(check-expect (neighbours 'B sample-graph-0) '(E F))
(define (neighbours node graph)
  (cond
    [(empty? graph) (error "node not found")]
    [else
      (local
        ((define first-pair (first graph))
         (define first-node (first first-pair))
         (define first-neighbours (second first-pair))
         )
        ; -- IN --
        (if 
          (symbol=? first-node node)
          first-neighbours
          (neighbours node (rest graph))
          ))]))

; =================== End of exercise ==================

(test)

