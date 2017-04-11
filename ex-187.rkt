#lang htdp/bsl+

; ### Constants

; ### Data Definitions
(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points 

(define play1 (make-gp "paco" 10))
(define play2 (make-gp "manolo" 15))

; ### Functions
(check-expect (sort-players '()) '())
(check-expect 
  (sort-players (list play1 play2))
  (list play2 play1)
  )
(define (sort-players lop)
  (cond
    [(empty? lop) '()]
    [else 
      (insert 
        (first lop) 
        (sort-players (rest lop))
        )]))


; GamePlayer List-of-players -> List-of-players
; Inserts gp into its proper place given a sorted List-of-players
(check-expect (insert play1 '()) (list play1))
(check-expect (insert play1 (list play2)) (list play2 play1))
(define (insert gp lop)
  (cond
    [(empty? lop) (list gp)]
    [(player>? gp (first lop)) (cons gp lop)]
    [else (cons (first lop) (insert gp (rest lop)))]
    ))


; GamePlayer GamePlayer -> Boolean
; Returns whether the player a has greater score than b
(check-expect (player>? play1 play2) #false)
(check-expect (player>? play2 play1) #true)
(define (player>? a b)
  (>= (gp-score a) (gp-score b))
  )


(require test-engine/racket-tests)
(test)

