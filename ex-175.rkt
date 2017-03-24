#lang htdp/bsl

(require 2htdp/batch-io)
; ### Constants

; ### Data Definitions
(define-struct stats [c w l])
; Statistic is a structure (make-statistic Number Number Number)

; ### Functions
; String -> Statistic
(define (wc n)
  (extract-stats (read-words/line n))
  )


; List-of-list-of-string -> Statistic
; Reads a text in form of list of lists and returns its statistics
(check-expect (extract-stats '()) (make-stats 0 0 0))
(check-expect 
  (extract-stats (list (list "hello" "there") (list "how" "you" "doing?")))
  (make-stats 22 5 2)
  )

(define (extract-stats lls)
  (cond
    [(empty? lls) (make-stats 0 0 0)]
    [else 
      (sum-stats
        (make-stats 
          (line-chars (first lls))  ; white spaces between words ignored...
          (length (first lls))
          1
          )
        (extract-stats (rest lls))
        )]))


; List-of-strings -> Number
; Sums the lengths of all the strings
(check-expect (line-chars (list "eeeyyy" "macarena" "aaaaamm")) 21)
(define (line-chars los)
  (cond
    [(empty? los) 0]
    [else 
      (+
        (string-length (first los))
        (line-chars (rest los))
        )]))


; Statistic Statistic -> Statistic
; Sums a Statistic to another Statistic
(check-expect 
  (sum-stats (make-stats 1 2 3) (make-stats 3 0 5)) 
  (make-stats 4 2 8)
  )
(define (sum-stats a b)
  (make-stats
    (+ (stats-c a) (stats-c b))
    (+ (stats-w a) (stats-w b))
    (+ (stats-l a) (stats-l b))
    ))


(require test-engine/racket-tests)
(test)


