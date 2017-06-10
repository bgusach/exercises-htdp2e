#lang htdp/isl

(require test-engine/racket-tests)


; ### Constants
(define empty-word '())

; ### Functions
; Word -> List-of-words
; Calculates all the letter-permutations of w
(check-satisfied (arrangements (list "d" "e")) all-de-permutations?)
(check-satisfied (arrangements (list "d" "o" "g")) all-dog-permutations?)
(check-expect (arrangements '()) (list '()))
(define (arrangements w)
  (local 
    (
      ; 1-String List-of-words -> List-of-words
      (define (insert-everywhere/in-all-words letter low)
        (cond
          [(empty? low) '()]
          [else 
            (append
             (insert-everywhere letter (first low))
             (insert-everywhere/in-all-words letter (rest low))
             )]))

      ; 1-String Word -> List-of-words
      ; Returns a list of words where the letter has been inserted in all
      ; possible positions of the word
      (define (insert-everywhere letter word)
        (local
          (
            ; List-of-any Any List-of-any -> List-of-list-of-any
            ; Helper function to insert element in all the positions
            ; of acc-post, "prefixed" with acc-pre.
            (define (distribute acc-pre element acc-post)
              (cond 
                [(empty? acc-post) 
                 (list (append acc-pre (list element)))
                 ]

                [else
                  (append
                    (list (append acc-pre (cons element acc-post)))
                    (distribute
                      (append acc-pre (list (first acc-post)))
                      element
                      (rest acc-post))
                    )]))
     
     )
    (distribute empty-word letter word)
    )))

    ; -- IN --
    (cond
      [(empty? w) (list '())]
      [else
        (insert-everywhere/in-all-words
          (first w)
          (arrangements (rest w))
          )])))


; Test function for arrangements
(define (all-de-permutations? low)
  (and
    (= (length low) 2)
    (member (list "e" "d") low)
    (member (list "d" "e") low)
    ))

; Test function for arrangements
(define (all-dog-permutations? low)
  (and
    (= (length low) 6)
    (member (list "d" "o" "g") low)
    (member (list "o" "d" "g") low)
    (member (list "o" "g" "d") low)
    (member (list "g" "o" "d") low)
    (member (list "g" "d" "o") low)
    (member (list "d" "g" "o") low)
    ))


; ### Functions



; =================== End of exercise ==================


(test)

