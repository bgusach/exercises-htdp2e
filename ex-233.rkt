#lang htdp/bsl+

(require test-engine/racket-tests)

; Example 1
(define quasiquoted1 `(0 ,@'(1 2 3) 4))
(define with-lists1 (list 0 1 2 3 4))
(check-expect quasiquoted1 with-lists1)

; Example 2
(define 
  quasiquoted2
  `(("alan" ,(* 2 500))
    ("barb" 2000)
    (,@'("carl" ", the great") 1500)
    ("dawn" 2300))
  )
(define
  with-lists2
  (list
    (list "alan" 1000)
    (list "barb" 2000)
    (list "carl" ", the great" 1500)
    (list "dawn" 2300)
    ))

(check-expect quasiquoted2 with-lists2)

(define (make-cell v)
  `(td ,(number->string v))
  )

(define (make-row l)
  (cond
    [(empty? l) '()]
    [else
      (cons
        (make-cell (first l))
        (make-row (rest l))
        )]))

; Example 3
(define 
  quasiquoted3
  `(html
     (body
       (table 
         ((border "1"))
         (tr ((width "200")) ,@(make-row '(1  2)))
         (tr ((width "200")) ,@(make-row '(99 65)))))
     ))

(define
  with-lists3
  (list
    'html
    (list 
      'body
      (list
        'table
        (list (list 'border "1"))
        (list
          'tr
          (list (list 'width "200"))
          (list 'td "1") (list 'td "2")
          )
        (list
          'tr
          (list (list 'width "200"))
          (list 'td "99") (list 'td "65")
          )))))

(check-expect with-lists3 quasiquoted3)

(test)

