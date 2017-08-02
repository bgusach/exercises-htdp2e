#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ==================== Exercise 390 ====================

(define-struct branch [left right])

; A Tree of Symbols (TOS) is one of:
; - Symbol
; - (make-branch TOS TOS)

; A Direction is one of:
; - 'left
; - 'right

; A Path is a [List-of Direction]


; TOS Path -> [Either Direction Error]
(check-expect (tree-pick 'a '()) 'a)
(check-expect (tree-pick (make-branch 'a 'b) '(right)) 'b)
(check-expect 
  (tree-pick 
    (make-branch 'a (make-branch 'b 'c)) 
    '(right left)
    )
  'b
  )
(check-error 
  (tree-pick (make-branch 'a 'b) '(right right right))
  "too many directions"
  )
(check-error 
  (tree-pick (make-branch 'a 'b) '())
  "too few directions"
  )
(define (tree-pick tree lod)
  (cond
    [(and (symbol? tree) (empty? lod)) tree]
    [(and (symbol? tree) (cons? lod)) (error "too many directions")]
    [(and (branch? tree) (empty? lod)) (error "too few directions")]
    [(and (branch? tree) (cons? lod)) 
     (if
       (symbol=? (first lod) 'left)
       (tree-pick (branch-left tree) (rest lod))
       (tree-pick (branch-right tree) (rest lod))
       )]))

; =================== End of exercise ==================




; ==================== Exercise 391 ====================

; [List-of Any] [List-of Any] -> [List-of Any]
; Merges two lists
(check-expect 
  (replace-eol-with (cons 1 '()) '(a))
  (cons 1 '(a))
  )
(check-expect 
  (replace-eol-with '(2 1) '(a))
  '(2 1 a)
  )
(check-expect (replace-eol-with '() '()) '())
(check-expect (replace-eol-with '(1 2) '()) '(1 2))
(check-expect (replace-eol-with '() '(1 2)) '(1 2))
(define (replace-eol-with front end)
  (cond
    [(empty? end) front]
    [(empty? front) end]
    [else
     (cons
       (first front)
       (replace-eol-with (rest front) end)
       )]))

; =================== End of exercise ==================




; ==================== Exercise 392 ====================


; TOS Path -> [Either Direction Error]
(check-expect (tree-pick.v2 'a '()) 'a)
(check-expect (tree-pick.v2 (make-branch 'a 'b) '(right)) 'b)
(check-expect 
  (tree-pick.v2 
    (make-branch 'a (make-branch 'b 'c)) 
    '(right left)
    )
  'b
  )
(check-error 
  (tree-pick.v2 (make-branch 'a 'b) '(right right right))
  )
(check-error 
  (tree-pick.v2 (make-branch 'a 'b) '())
  )
(define (tree-pick.v2 tree lod)
  (cond
    [(and (symbol? tree) (empty? lod)) tree]
    [(and (branch? tree) (cons? lod)) 
     (local
       ((define selector
          (if 
            (symbol=? (first lod) 'left) 
            branch-left 
            branch-right
            )))

       ; -- IN --
       (tree-pick.v2 (selector tree) (rest lod))
       )]
     [else (error "directions and tree not compatible")]
     ))

; NOTE: it wasn't a huge simplification :)

; =================== End of exercise ==================




; ==================== Exercise 393 ====================


; Son.R Son.R -> Son.R
; Returns the union of two sets
(check-expect (union '(1 2 3) '(1 5)) '(3 2 1 5))
(check-expect (union '() '()) '())
(check-expect (union '(1) '()) '(1))
(check-expect (union '() '(1)) '(1))
(define (union s0 s1)
  (cond
    [(empty? s0) s1]
    [(empty? s1) s0]
    [else
      (if 
        (member? (first s0) s1)
        (union (rest s0) s1)
        (union (rest s0) (cons (first s0) s1))
        )]))


; Son.R Son.R -> Son.R
; Returns the intersection of two sets
(check-expect (intersect '() '()) '())
(check-expect (intersect '(1) '()) '())
(check-expect (intersect '() '(1)) '())
(check-expect (intersect '(1 2 3) '(1 5)) '(1))
(define (intersect s0 s1)
  (cond
    [(empty? s0) '()]
    [else
      (if 
        (member? (first s0) s1)
        (cons 
          (first s0)
          (intersect (rest s0) s1)
          )
        (intersect (rest s0) s1)
        )]))


; =================== End of exercise ==================




; ==================== Exercise 394 ====================

; [List-of Number] [List-of Number] -> [List-of Number]
; Merges two sorted lists into a new sorted list
(check-expect (merge '(1 3 5 9) '(4 9)) '(1 3 4 5 9 9))
(define (merge l0 l1)
  (cond
    [(empty? l0) l1]
    [(empty? l1) l0]
    [else
      (local
        ((define f0 (first l0))
         (define f1 (first l1))
         )

        ; -- IN --
        (if
          (< f0 f1)
          (cons f0 (merge (rest l0) l1))
          (cons f1 (merge l0 (rest l1)))
          ))]))

; =================== End of exercise ==================




; ==================== Exercise 395 ====================

; [List-of Any] N -> [List-of Any]
; Takes the first n elemets from l
(check-expect (take '() 0) '())
(check-expect (take '(12) 0) '())
(check-expect (take '() 5) '())
(check-expect (take '(1 2 3 4 5) 5) '(1 2 3 4 5))
(check-expect (take '(1 2 3 4 5 6 7) 5) '(1 2 3 4 5))
(define (take l n)
  (cond
    [(empty? l) '()]
    [(zero? n) '()]
    [else
      (cons
        (first l)
        (take (rest l) (sub1 n))
        )]))


; [List-of Any] N -> [List-of Any]
; Drops the first n elemets from l
(check-expect (drop '() 0) '())
(check-expect (drop '(12) 0) '(12))
(check-expect (drop '() 5) '())
(check-expect (drop '(1 2 3 4 5) 5) '())
(check-expect (drop '(1 2 3 4 5 6 7) 5) '(6 7))
(define (drop l n)
  (cond
    [(empty? l) '()]
    [(zero? n) l]
    [else (drop (rest l) (sub1 n))]
    ))

; =================== End of exercise ==================

(test)

