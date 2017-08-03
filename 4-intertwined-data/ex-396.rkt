#lang htdp/isl+

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

; ### Constants
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; ### Data Definitions

; A HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 
 
; ### Functions

; HM-Word N -> String
; Run a simplistic Hangman game, produce the current state
(define (play the-pick time-limit)
  (local 
    ((define the-word  (explode the-pick))
     (define the-guess (make-list (length the-word) "_"))
     ; HM-Word -> HM-Word
     (define (do-nothing s) s)
     ; HM-Word KeyEvent -> HM-Word 
     (define (checked-compare current-status ke)
       (if 
         (member? ke LETTERS)
         (compare-word the-word current-status ke)
         current-status
         ))
     (define (over? current-status)
       (equal? current-status the-word)
       ))

    ; -- IN --
    (implode
     (big-bang 
       the-guess
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key checked-compare]
       [stop-when over? render-word]
       ))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black")
  )

; HM-Word HM-Word Letter -> HM-Word
(check-expect
  (compare-word (explode "hola") (explode "____") "o")
  (explode "_o__")
  )
(check-expect
  (compare-word (explode "hola") (explode "ho_a") "z")
  (explode "ho_a")
  )
(define (compare-word target current-status guess)
  (cond
    [(empty? target) '()]
    [else
      (cons
        (if 
          (string=? (first target) guess)
          guess
          (first current-status)
          )
        (compare-word
          (rest target)
          (rest current-status)
          guess
          ))]))

(test)

(play "pepino" 30)

