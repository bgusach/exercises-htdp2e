#lang htdp/bsl

(define good (explode "good"))
(define all (explode "all"))
(define lla (explode "lla"))


(define-struct editor [pre post])
; An Editor is a structure (make-editor Lo1S Lo1S)

; An Lo1S is one of:
; - '()
; - (cons 1String Lo1S)


; Lo1S -> Lo1S
; Reverses a list of 1String
(check-expect (rev '()) '())
(check-expect (rev good) (explode "doog"))
(define (rev lo1s)
  (cond
    [(empty? lo1s) '()]
    [else 
      (add-at-end 
        (rev (rest lo1s)) 
        (first lo1s)
        )]))


; Lo1S 1String -> Lo1S
; Adds s to the end of lo1s
(check-expect (add-at-end '() "s") (cons "s" '()))
(check-expect (add-at-end (explode "hi") "s") (explode "his"))

(define (add-at-end lo1s s)
  (cond
    [(empty? lo1s) (cons s '())]
    [else
      (cons
        (first lo1s)
        (add-at-end (rest lo1s) s)
        )]))


; String String -> Editor
; Creates an editor from two strings
(check-expect (create-editor "abc" "dfg") (make-editor (explode "cba") (explode "dfg")))

(define (create-editor s1 s2)
  (make-editor
    (rev (explode s1))
    (explode s2)
    ))



(require test-engine/racket-tests)
(test)
