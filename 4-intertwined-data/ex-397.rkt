#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)


; ==================== Exercise 397 ====================

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


; [List-of Employee] [List-of WorkRecord] -> [List-of Payment]
(check-expect (wages*.v3 '() '()) '())
(check-expect 
  (wages*.v3 (list john paco) (list paco-wr john-wr)) 
  (list john-pay paco-pay)
  )
(check-error (wages*.v3 '(john) '()))
(check-error (wages*.v3 '(john) '(paco)))
(check-error (wages*.v3 '() '(paco)))
(define (wages*.v3 loe lowr)
  (local
    ((define sorted-empl 
       (sort 
         loe 
         (λ (a b) (string<? (employee-name a) (employee-name b)))
         ))
     (define sorted-work-recs
       (sort 
         lowr 
         (λ (a b) (string<? (work-rec-employee a) (work-rec-employee b)))
         ))

     ; [List-of Employee] [List-of WorkRecord] -> [List-of Payment]
     (define (resolve loe lowr)
       (cond
         [(and (empty? loe) (empty? lowr)) '()]
         [(or
            (and (cons? loe) (empty? lowr))
            (and (empty? loe) (cons? lowr)) 
            )
            (error "different lengths")
            ]
         [else 
           (cons
             (calculate-payment (first loe) (first lowr))
             (resolve (rest loe) (rest lowr))
             )])))

    ; -- IN --
    (resolve sorted-empl sorted-work-recs)
    ))


; Employee WorkRecord -> Payment
(define (calculate-payment empl w-rec)
  (if 
   (string=? (employee-name empl) (work-rec-employee w-rec))
   (make-payment
     (employee-name empl)
     (*
       (employee-rate empl)
       (work-rec-hours w-rec)
       ))
   (error "employee name and record don't match!")
   ))

; =================== End of exercise ==================



(test)

