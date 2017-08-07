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


; ==================== Exercise 404 ====================

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


; ==================== Exercise 405 ====================

(define projected-content
  '(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
 

(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))
 

(define projected-db
  (make-db projected-schema projected-content))
 

; DB [List-of Label] -> DB
(check-expect
  (db-content (project school-db '("Name" "Present")))
  projected-content
  )
(define (project db labels)
  (local 
    ((define schema  (db-schema db))
     (define content (db-content db))

     ; Spec -> Boolean
     ; Returns whether this spec belong to the new schema
     (define (keep? c) 
       (member? (spec-label c) labels)
       )

     ; Row -> Row
     ; retains those columns whose name is in labels
     (define (row-project row) 
       (row-filter row (map spec-label schema))
       )

     ; Row [List-of Label] -> Row
     ; Keeps the cells in `labels`
     (define (row-filter row initial-labels)
       (cond
         [(empty? row) '()]
         [else
           (if 
             (member? (first initial-labels) labels)
             (cons 
               (first row) 
               (row-filter (rest row) (rest initial-labels))
               ) 
             (row-filter (rest row) (rest initial-labels))
             )])))

    ; -- IN --
    (make-db (filter keep? schema) (map row-project content))
    ))

; =================== End of exercise ==================




; ==================== Exercise 406 ====================

; DB [List-of Label] -> DB
(check-expect
  (db-content (project.v2 school-db '("Name" "Present")))
  projected-content
  )
(define (project.v2 db labels)
  (local 
    ((define schema  (db-schema db))
     (define schema-labels (map spec-label schema))
     (define content (db-content db))

     ; Spec -> Boolean
     ; Returns whether this spec belong to the new schema
     (define (keep? c) 
       (member? (spec-label c) labels)
       )

     ; Row -> Row
     ; retains those columns whose name is in labels
     (define (row-project row) 
       (row-filter row schema-labels)
       )

     ; Row [List-of Label] -> Row
     ; Keeps the cells in `labels`
     (define (row-filter row initial-labels)
       (cond
         [(empty? row) '()]
         [else
           (if 
             (member? (first initial-labels) labels)
             (cons 
               (first row) 
               (row-filter (rest row) (rest initial-labels))
               ) 
             (row-filter (rest row) (rest initial-labels))
             )])))

    ; -- IN --
    (make-db (filter keep? schema) (map row-project content))
    ))

; =================== End of exercise ==================



; ==================== Exercise 407 ====================

; DB [List-of Label] -> DB
(check-expect
  (db-content (project.v3 school-db '("Name" "Present")))
  projected-content
  )
(define (project.v3 db labels)
  (local 
    ((define schema  (db-schema db))
     (define schema-labels (map spec-label schema))
     (define content (db-content db))

     ; Spec -> Boolean
     ; Returns whether this spec belong to the new schema
     (define (keep? c) 
       (member? (spec-label c) labels)
       )

     ; Row -> Row
     ; retains those columns whose name is in labels
     (define (row-project row) 
       (row-filter row schema-labels)
       )

     ; Row [List-of Label] -> Row
     ; Keeps the cells in `labels`
     (define (row-filter row initial-labels)
       (foldr
         (λ 
           (cell label acc)
           (if 
             (member? label labels)
             (cons cell acc)
             acc
             ))
         '()
         row
         initial-labels
         )))

    ; -- IN --
    (make-db (filter keep? schema) (map row-project content))
    ))

; =================== End of exercise ==================



; ==================== Exercise 408 ====================

; DB [List-of Label] [List-of [Row -> Boolean]] -> [List-of Row]
; Selects from `db` the columns in `lol` that satisfy `pred`
(check-expect 
  (select school-db '("Name") (λ (r) (> (second r) 27)))
  '(("Alice")
    ("Carol")
    ("Dave")
    ))
(check-expect 
  (select school-db '("Name" "Age") (λ (r) (third r)))
  '(("Alice" 35)
    ("Carol" 30)
    ))
(check-expect 
  (select school-db '("Name" "Age") (λ (r) #f))
  '()
   )
(define (select db lol pred)
  (local
    ((define rows (db-content db))
     (define selected-rows (filter pred rows))
     (define db-columns (map spec-label (db-schema db)))
     (define column-mask 
       (for/list [(col db-columns)] (member? col lol))
       )
     (define (drop-row-cells row)
       (foldr
         (λ (cell selected? acc) 
            (if selected? (cons cell acc) acc)
           )
         '()
         row
         column-mask
       )))

    ; -- IN --
    (map drop-row-cells selected-rows)
    ))

; =================== End of exercise ==================




; ==================== Exercise 409 ====================


; DB [List-of Label] -> DB
; Given a DB and a list of label/columns, it returns a projection
; of that DB where only those labels are present, and in the passed
; order
(check-expect
  (db-content (reorder school-db '("Age" "Name")))
  '((35 "Alice")
    (25 "Bob")
    (30 "Carol")
    (32 "Dave")
    ))
(check-expect
  (map spec-label (db-schema (reorder school-db '("Age" "Name"))))
  '("Age" "Name")
  )
(check-expect 
  (db-schema (reorder school-db '()))
  '()
  )
(define (reorder db lol) 
  (local
    ((define contents (db-content db))
     (define schema (db-schema db))
     (define schema-columns (map spec-label schema))

     ; Label -> [Either N Error]
     ; Returns the position of the column in the schema,
     ; or signals an error if not found
     (define (find-col-pos col-name)
       (local 
         ((define res (index-of schema-columns col-name))
          )
           
         ; -- IN --
         (if 
           (false? res) 
           (error (format "Column ~a not found" col-name))
           res
           )))
      
     (define col-selection (map find-col-pos lol))

     ; [List-of X] -> [List-of X]
     ; Picks the positions defined at `col-selection`
     ; from the passed lists
     (define (pick-positions a-list)
       (for/list [(col col-selection)] (list-ref a-list col))
       )

     (define new-schema (pick-positions schema))
     (define new-contents (map pick-positions contents))
     )
     
     ; -- IN --
     (make-db new-schema new-contents)
     ))

; NOTE: this solution handles the cases where the desired column
; order is smaller than the amount of columns, and only delivers
; what has been asked for (i.e. drops columns)

; =================== End of exercise ==================




; ==================== Exercise 409 ====================

; DB DB -> DB
; Joins the two databases into a new one, eliminating
; duplicated rows. It performs no error checking, DBs 
; must have same schema and be consistent
(check-expect
  (db-content
    (db-union
      (make-db school-schema '(("Alice" 20 #t) ("Johnny" 30 #t)))
      (make-db school-schema '(("Alice" 20 #t) ("Paco" 40 #t)))
      ))
  '(("Johnny" 30 #t)
    ("Alice" 20 #t)
    ("Paco" 40 #t)
    ))
(define (db-union db-a db-b)
  (local
    ((define schema-a (db-schema db-a))
     (define rows-a (db-content db-a))
     (define rows-b (db-content db-b))
     (define merged-rows
       (foldr 
         (λ (row acc)
           (if
             (member row acc)
             acc
             (cons row acc)
             ))
         rows-b
         rows-a
         )))
    ; -- IN --
    (make-db
      schema-a
      merged-rows
      )))

; =================== End of exercise ==================




; ==================== Exercise 410 ====================

(define test-users-db
  (make-db
    (list
      (make-spec "name" string?)
      (make-spec "user-id" string?)
      )
    '(("John" "johny38")
      ("Francisco" "paco")
      ("Friedrich" "fritz")
      )))
(define test-bought-items
  (make-db
    (list
      (make-spec "buyer-id" string?)
      (make-spec "item-name" string?)
      (make-spec "price" number?)
      )
    '(("johny38" "toothpaste" 10)
      ("johny38" "toiletpaper" 20)
      ("paco" "condoms" 1)
      ("fritz" "beer" 20)
      ("fritz" "potatoes" 10)
      )))

; DB DB -> DB
; Given the last Spec of db-1 matches the first of db-2,
; it joins the two databases into a single one. 
(check-expect
  (db-content (join school-db presence-db))
  '(("Alice" 35 "presence")
    ("Bob"   25 "absence")
    ("Carol" 30 "presence")
    ("Dave"  32 "absence")
    ))
(check-expect
  (db-content (join test-users-db test-bought-items))
  '(("John" "toothpaste" 10)
    ("John" "toiletpaper" 20)
    ("Francisco" "condoms" 1)
    ("Friedrich" "beer" 20)
    ("Friedrich" "potatoes" 10)
    ))
(check-expect 
  (map spec-label (db-schema (join test-users-db test-bought-items)))
  '("name" "item-name" "price")
  )
(define (join db-1 db-2)
  (local
    ((define cont-1 (db-content db-1))
     (define schema-1 (db-schema db-1))
     (define cont-2 (db-content db-2))
     (define schema-2 (db-schema db-2))

     (define new-schema 
       (append (drop-right schema-1 1) (rest schema-2))
       )

     ; Any -> [List-of Row]
     ; Retuns the rows of db-2 that match value `key`
     (define (find-matching-rows key)
       (filter 
         (λ (row) (equal? (first row) key))
         cont-2
         ))
     
     ; Row -> [List-of Row]
     ; Given a row from db-1, it findes its db-2 matches
     ; and returns a list of the new joined rows
     (define (get-joined-rows row)
       (local
         ((define key (last row))
          (define cleaned-row (drop-right row 1))
          (define subrows (find-matching-rows key))
          (define cleaned-subrows (map rest subrows))
          )

         ; -- IN --
         (for/list [(subrow cleaned-subrows)] (append cleaned-row subrow))
         ))

     (define new-content
       (foldl
        (λ (row acc) (append acc (get-joined-rows row)))
        '()
        cont-1
        )))
     
    ; -- IN --
    (make-db new-schema new-content)
    ))


; =================== End of exercise ==================

(test)

