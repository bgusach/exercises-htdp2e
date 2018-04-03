#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require racket/list)


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




; A Path is a [List-of Node].
; Interpretation: the list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one

; Node Node Graph -> [Maybe Path]
(check-expect (find-path 'C 'D sample-graph-0) '(C D))
(check-member-of (find-path 'E 'D sample-graph-0) '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph-0) #f)
(define (find-path orig dest graph)
  (cond
    [(symbol=? orig dest) (list dest)]
    [else
      (local
        ((define orig-neighbours (neighbours orig graph))
         (define candidate 
           (find-path/list orig-neighbours dest graph)
           ))

         ; -- IN --
         (if
           (false? candidate)
           #f
           (cons orig candidate)
           ))]))


; [List-of Node] Node Graph -> [Maybe Path]
; Finds a path from some node on `origs` to
; `dest`; otherwise, it produces #false
(check-expect (find-path/list '(A) 'B sample-graph-0) '(A B))
(define (find-path/list origs dest graph)
  (cond
    [(empty? origs) #false]
    [else
      (local
        ((define candidate 
           (find-path (first origs) dest graph)
           ))

        ; -- IN --
        (if 
          (false? candidate)
          (find-path/list (rest origs) dest graph)
          candidate
          ))]))




; ==================== Exercise 472 ====================

(check-expect (find-path 'A 'G sample-graph-0) '(A B E F G))

; Q: which path does it find? why?
; A: It finds A-B-E-F-G because neighbours are explored in the
;    order they are defined, and the first found solution is 
;    returned. For instance, if we reordered B's neighbours, 
;    we would get A-B-F-G


; Graph -> Boolean
(check-expect (test-on-all-nodes sample-graph-0) #f)
(check-expect (test-on-all-nodes '((A (B)) (B (A))) ) #t)
(define (test-on-all-nodes graph)
  (local
    ((define all-nodes (for/list ([node-def graph]) (first node-def)))
     (define combos (cartesian-product all-nodes all-nodes))
     )

    ; -- IN --
    (for/and ([comb combos])
      (cons? (find-path (first comb) (second comb) graph))
      )))


; =================== End of exercise ==================


(define cyclic-graph
  '((A (B E))
    (B (E F))
    (C (B D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())
    ))


; ==================== Exercise 473 ====================

(check-expect (find-path 'B 'C cyclic-graph) '(B E C))

; Next one loops forever
; (check-expect (test-on-all-nodes cyclic-graph) #f)

; =================== End of exercise ==================




; ==================== Exercise 474 ====================

(check-expect (find-path.v2 'C 'D sample-graph-0) '(C D))
(check-member-of (find-path.v2 'E 'D sample-graph-0) '(E F D) '(E C D))
(check-expect (find-path.v2 'C 'G sample-graph-0) #f)
(define (find-path.v2 orig dest graph)
  (local
    ((define (find-path orig dest)
      (cond
        [(symbol=? orig dest) (list dest)]
        [else 
          (local
            ((define candidate
               (find-path/list (neighbours orig graph) dest)
               ))
            ; -- IN --
            (if
              (false? candidate)
              #f
              (cons orig candidate)
              ))]))

      (define (find-path/list origs dest)
        (cond
          [(empty? origs) #f]
          [else
            (local
              ((define candidate 
                 (find-path (first origs) dest)
                 ))
              ; -- IN --
              (if
                (false? candidate)
                (find-path/list (rest origs) dest)
                candidate
                ))])))

    ; -- IN --
    (find-path orig dest)
    ))

; =================== End of exercise ==================


; ==================== Exercise 475 ====================

; [List-of Node] Node Graph -> [Maybe Path]
; Finds a path from some node on `origs` to
; `dest`; otherwise, it produces #false
(check-expect (find-path/list.v2 '(A) 'B sample-graph-0) '(A B))
(define (find-path/list.v2 origs dest graph)
  (cond
    [(empty? origs) #false]
    [else
      (foldl
        (λ (it acc) 
          (if 
            (list? acc)
            acc
            (find-path it dest graph)
            ))
        #false
        origs
        )]))

; Q: How does ISL+'s ormap differ from Racket's ormap?
; A: ISL+'s returns #t if any value is #t or #f otherwise, 
;    whereas Racket's returns the first non-falsy
;    value or #f is none at all. 
;    In our case Racket's ormap would help quite a lot,
;    since we could loop through neighbours, and if 
;    any provides a valid path, that would be returned

; =================== End of exercise ==================

(test)

