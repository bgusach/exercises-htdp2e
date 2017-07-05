#lang htdp/isl+

(require test-engine/racket-tests)
(require racket/list)

; ### Functions

; X [List-of X] -> [Maybe N]
; Determines the index of the first occurence of x in l
; or #false otherwise
(check-satisfied 
  (index "a" '("b" "c" "d")) 
  (is-index? "a" '("b" "c" "d"))
  )
(check-satisfied 
  (index "z" '("b" "c" "d")) 
  (is-index? "z" '("b" "c" "d"))
  )
(check-satisfied 
  (index "z" '()) 
  (is-index? "z" '())
  )
(define (index x l)
  (cond
    [(empty? l) #false]
    [(equal? (first l) x) 0]
    [else
      (local 
        ((define i (index x (rest l)))
         )

        ; -- IN --
        (if
          (false? i)
          #false
          (add1 i)
          ))]))


; [X] X [List-of X] -> [[Maybe N] -> Boolean]
(define (is-index? x l)
  (lambda (output)
    (if
      (false? output)
      (not (member x l))
      (and
        (<= output (length l))
        (equal? (list-ref l output) x)
        (not (member x (take l (sub1 output))))
        ))))


(test)

