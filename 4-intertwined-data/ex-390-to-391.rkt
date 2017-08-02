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

(test)

