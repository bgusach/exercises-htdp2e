#lang htdp/isl

(require test-engine/racket-tests)

; ### Data Definitions

; ### Functions

; [List-of 1String] -> [[List-of 1String]]
; For a list of 1Strings, return all the possible prefixes
(check-expect (prefixes '()) '())
(check-expect (prefixes '("a")) '(("a")))
(check-expect 
  (prefixes '("a" "b" "c")) 
   '(
     ("a")
     ("a" "b")
     ("a" "b" "c")
     ))
(define (prefixes lo1s)
  (local
    (; 1String [[List-of 1String]] -> [[List-of 1String]]
     ; Adds a new list with item, and prepends item to 
     ; all existing lists
     (define (merge item acc)
       (local
         (; [List-of 1String] -> [List-of 1String]
          ; Adds item to the beginning of lst
          (define (prepend-item lst)
            (cons item lst)
            ))

         ; -- IN --
         (map prepend-item (cons '() acc))
         )))

    ; -- IN --
    (foldr merge '() lo1s)
    ))

(test)

