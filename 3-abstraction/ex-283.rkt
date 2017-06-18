#lang htdp/isl+

(require test-engine/racket-tests)

; ### Functions

(map (lambda (x) (* 10 x))
     '(1 2 3))
 

(foldl (lambda (name rst)
         (string-append name ", " rst))
       "etc."
       '("Matthew" "Robby"))
 

(define-struct ir [name price])
(define th 15)

(filter (lambda (ir) (<= (ir-price ir) th))
        (list (make-ir "bear" 10)
              (make-ir "doll" 33)))

(test)
