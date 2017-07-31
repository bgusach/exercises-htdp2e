#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ==================== Exercise 387 ====================

; [List-of Symbol] [List-of Number] 
;    -> [List-of (cons Symbol (cons Number '()))]
(check-expect (cross '() '()) '())
(check-expect (cross '(a) '()) '())
(check-expect (cross '() '(1)) '())
(check-expect 
  (cross '(a b c) '(1 2))
  '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2))
  )
(define (cross los lon)
  (match
    los
    ['() '()]
    [(cons head tail)
     (append
       (combine head lon)
       (cross tail lon)
       )]))


; [X Y] X [List-of Y] -> [List-of (cons X (cons Y '()))]
(check-expect (combine 1 '()) '())
(check-expect (combine 1 '(a)) '((1 a)))
(check-expect (combine 1 '(a b)) '((1 a) (1 b)))
(define (combine el lst)
  (match
    lst
    ['() '()]
    [(cons head tail)
      (cons
        (list el head)
        (combine el tail)
        )]))


; =================== End of exercise ==================




; ==================== Exercise 388 ====================

(define-struct employee [name ssn rate])
; An Employee is an structure (make-employee String N Number)
; For instance:
(define john (make-employee "john" 123 45))
(define paco (make-employee "paco" 444 65))


(define-struct work-rec [employee hours])
; A WorkRecord is an structure (make-work-rec String Number)
; For instance:
(define john-wr (make-work-rec (employee-name john) 10))
(define paco-wr (make-work-rec (employee-name paco) 7))


(define-struct payment [employee amount])
; A Payment is an structure (make-payment String Number)
; For instance:
(define john-pay (make-payment (employee-name john) 450))
(define paco-pay (make-payment (employee-name paco) 455))

; [List-of Employee] [List-of WorkRecord]
(check-expect (wages*.v3 '() '()) '())
(check-expect 
  (wages*.v3 (list john paco) (list paco-wr john-wr)) 
  (list john-pay paco-pay)
  )
(define (wages*.v3 loe lowr)
  (match
    loe
    ['() '()]
    [(cons empl tail)
     (cons
       (calculate-payment 
         empl
         (find-work-rec (employee-name empl) lowr)
         )
       (wages*.v3 tail lowr)
       )]))


; String [List-of WorkRecord] -> [Either WorkRecord Error]
(define (find-work-rec name lowr)
  (match
    lowr
    ['() (error "employee record not found")]
    [(cons this-rec tail)
     (if 
       (string=? (work-rec-employee this-rec) name)
       this-rec
       (find-work-rec name tail)
       )]))


; Employee WorkRecord -> Payment
(define (calculate-payment empl w-rec)
  (make-payment
    (employee-name empl)
    (*
      (employee-rate empl)
      (work-rec-hours w-rec)
      )))

; =================== End of exercise ==================




; ==================== Exercise 389 ====================

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)


; [List-of String] [List-of N] -> [List-of PhoneRecord]
; Combines a list of names with an equally long list of
; numbers and returns a list of phone records
(check-expect
  (zip '("johnny" "francman") '(123 555))
  (list 
    (make-phone-record "johnny" 123)
    (make-phone-record "francman" 555)
    ))
(define (zip lon lopn)
  (match
    lon
    ['() '()]
    [(cons head tail)
     (cons
       (make-phone-record
         head
         (first lopn)
         )
       (zip tail (rest lopn))
       )]))

; =================== End of exercise ==================




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

(test)

