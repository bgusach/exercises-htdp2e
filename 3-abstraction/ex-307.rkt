#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)
(require racket/string)

(check-expect (find-name "john" (list "pacman" "fritzie" "johnny")) "johnny")
(check-expect (find-name "johnny" (list "pacman" "fritzie")) #false)
(define (find-name name lon)
  (for/or ([it lon]) 
    (if 
      (string-prefix? it name)
      it
      #false
      )))

(test)

