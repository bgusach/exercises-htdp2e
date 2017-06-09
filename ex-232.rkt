#lang htdp/bsl+

(require test-engine/racket-tests)

; Example 1
(define quasiquoted1 `(1 "a" 2 #false 3 "c"))
(define with-lists1 (list 1 "a" 2 #false 3 "c"))
(check-expect quasiquoted1 with-lists1)

; Example 2
(define 
  quasiquoted2
  `(("alan" ,(* 2 500))
    ("barb" 2000)
    (,(string-append "carl" ", the great") 1500)
    ("dawn" 2300))
  )
(define
  with-lists2
  (list
    (list "alan" 1000)
    (list "barb" 2000)
    (list "carl, the great" 1500)
    (list "dawn" 2300)
    ))

(check-expect quasiquoted2 with-lists2)

(define title "ratings")

(define 
  quasiquoted3
  `(html
     (head (title ,title))
     (body
       (h1 ,title)
       (p "A second web page")
       ))
    )

(define
  with-lists3
  (list
    'html
    (list 'head (list 'title title))
    (list 
      'body 
      (list 'h1 title)
      (list 'p "A second web page")
    )))


(check-expect quasiquoted3 with-lists3)

(test)

