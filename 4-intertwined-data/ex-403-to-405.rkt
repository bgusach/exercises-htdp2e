#lang htdp/isl+

(require test-engine/racket-tests)
(require racket/list)
(require racket/string)
(require racket/base)


; ### Data Definitions

(define-struct db [schema content])
; A DB is a structure:
;   (make-db Schema Content)
 

; A Schema is a [List-of Spec]

(define-struct spec [label predicate])
; Spec is a structure: (make-spec Label Predicate)
; A Label is a String
; A Predicate is a [Any -> Boolean]
 

; A (piece of) Content is a [List-of Row]
; A Row is [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 

; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch


; ==================== Exercise 403 ====================

(define school-schema
  (list 
    (make-spec "Name" string?)
    (make-spec "Age" integer?)
    (make-spec "Present" boolean?)
    ))


(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)
    ))
 

(define school-db 
  (make-db school-schema school-content)
  )


(define presence-schema
  (list 
    (make-spec "Present" boolean?)
    (make-spec "Description" string?)
    ))


(define presence-content
  '((#true  "presence")
    (#false "absence")
    ))

	
(define presence-db 
  (make-db presence-schema presence-content)
  )
 

; =================== End of exercise ==================




; ==================== Exercise 404 ====================

; DB -> Boolean
; Returns whether all rows in db satisfy (I1) and (I2)
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true)
(check-expect 
  (integrity-check 
    (make-db
      presence-schema
      '(("not-boolean" "description"))
      )) 
  #false
  )
(check-expect 
  (integrity-check 
    (make-db
      presence-schema
      '(("this" "is" "more" "than" "2" "items"))
      )) 
  #false
  )
(define (integrity-check db)
  (local
    ((define schema (db-schema db))
     ; Row -> Boolean
     (define (good-row? row)
       (and
         (length-ok? row)
         (cells-ok? row schema)
         ))

     ; Row -> Boolean
     (define (length-ok? row)
       (= (length row) (length schema))
       )

     ; Row -> Boolean
     (define (cells-ok? row schema)
       (cond
         [(empty? row) #true]
         [else
           (and
             ((spec-predicate (first schema))
              (first row))
             (cells-ok?
               (rest row)
               (rest schema)
               ))])))

    ; -- IN --
    (andmap good-row? (db-content db))
    ))

; =================== End of exercise ==================




; ==================== Exercise 405 ====================

; [X] [Y] [X Y -> Boolean] [List-of X] [List-of Y] -> Boolean
; Given a function that accepts two arguments, it takes
; two lists of the same length, and passes the values
; in pairs to fn. Returns true if all pairs satisfy the predicate
; like a "zip-andmap"
(check-expect (andmap2 > '(1 4 9) '(0 3 3)) #true)
(check-expect (andmap2 > '(1 4 1) '(0 3 3)) #false)
(define (andmap2 fn l0 l1)
  (cond
    [(empty? l0) #true]
    [else
      (and
        (fn (first l0) (first l1))
        (andmap2 fn (rest l0) (rest l1))
        )]))

; =================== End of exercise ==================


(test)

