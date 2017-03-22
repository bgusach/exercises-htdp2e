#lang htdp/bsl

(require 2htdp/batch-io)

; ### Constants


; ### Data Definitions

; List-of-string is one of
; - '()
; - (cons String List-of-string)


; List-of-list-of-strings is one of:
; - '()
; - (cons List-of-string List-of-list-of-string)


; ### Functions

(check-expect 
  (read-lines "short-ttt.txt")
  (cons "Things Take Time."
  (cons "But not this poem." '())
    ))

(check-expect 
  (read-words "short-ttt.txt")
  (cons "Things"
  (cons "Take"
  (cons "Time."
  (cons "But"
  (cons "not"
  (cons "this"
  (cons "poem." '())
  )))))))

(check-expect
  (read-words/line "short-ttt.txt")
  (cons 
    (cons "Things" (cons "Take" (cons "Time." '())))

    (cons 
      (cons "But" (cons "not" (cons "this" (cons "poem." '()))))
      '()
      )))


(read-words/line "short-ttt.txt")
(require test-engine/racket-tests)
(test)

