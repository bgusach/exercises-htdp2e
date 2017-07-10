#lang htdp/isl+

(require test-engine/racket-tests)
(require 2htdp/abstraction)

(check-expect
  (for/list ([i 2] [j '(a b)]) (list i j))
  '((0 a)
    (1 b)
    ))

(check-expect
  (for*/list ([i 2] [j '(a b)]) (list i j))
  '((0 a)
    (0 b)
    (1 a)
    (1 b)
    ))


(test)
