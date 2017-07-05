#lang htdp/isl+

(require test-engine/racket-tests)

; ### Functions

; X [List-of X] -> [Maybe [List-of X]]
; Returns the first sublist of lox that starts 
; with x, or #false otherwise
(check-satisfied (find 4 '(1 2 3 4)) (found? 4 '(1 2 3 4)))
(check-satisfied (find 7 '(1 2 3 4)) (found? 7 '(1 2 3 4)))
(check-satisfied (find 7 '()) (found? 7 '()))
(define (find x lox)
  (cond
    [(empty? lox) #false]
    [else
      (if
        (equal? (first lox) x)
        lox
        (find x (rest lox))
        )]))


(define (found? x lox)
  (lambda (find-res)
    (cond
      ; If res is false, element should not be in lis
      [(false? find-res) (not (member x lox))]
      [(> (length find-res) (length lox)) #false]
      [else
        (equal?
          find-res
          (drop-head lox (- (length lox) (length find-res)))
          )])))


; [A] [List-of A] Number -> [List-of A]
; Drops the first n elements from l
(check-expect (drop-head '(1 2 3) 1) '(2 3))
(check-expect (drop-head '(1 2 3) 3) '())
(define (drop-head l n)
  (cond
    [(zero? n) l]
    [else
      (drop-head (rest l) (sub1 n))
      ]))

(test)

